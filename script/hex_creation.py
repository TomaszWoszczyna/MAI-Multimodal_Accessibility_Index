import geopandas as gpd
gpd.options.io_engine = "fiona"

import rasterio
import rasterio.mask
import numpy as np
import requests
from shapely.geometry import Polygon
from shapely.affinity import translate
from osm2geojson import json2geojson
import os

# ============================================================
# PARAMETERS (USER EDITS THESE)
# ============================================================
city_name = "Salzburg"
hex_size  = 100  # meters (this number is used in output filenames)

data_dir = "your_data_path"

# Output filenames dynamically use hex_size
output_gpkg          = os.path.join(data_dir, f"{hex_size}m_hex_building.gpkg")
centroid_output_gpkg = os.path.join(data_dir, f"{hex_size}m_hex_building_centroids.gpkg")

# Keep layer names stable for your R script
layer_name          = "hexes_with_buildings"
centroid_layer_name = "hex_centroids"

# ============================================================
# 1) Download city boundary from Overpass
# ============================================================
print(f"Downloading city boundary for {city_name}...")
overpass_url = "http://overpass-api.de/api/interpreter"

boundary_query = f"""
[out:json][timeout:25];
relation["boundary"="administrative"]["name"="{city_name}"]["admin_level"="8"];
out body;
>;
out skel qt;
"""

boundary_response = requests.get(overpass_url, params={"data": boundary_query})
boundary_response.raise_for_status()
boundary_data = boundary_response.json()

boundary_geojson = json2geojson(boundary_data)
boundary_gdf = gpd.GeoDataFrame.from_features(boundary_geojson["features"])
boundary_gdf = boundary_gdf.set_crs("EPSG:4326").to_crs("EPSG:3857")
boundary = gpd.GeoDataFrame(geometry=[boundary_gdf.unary_union], crs="EPSG:3857")

# ============================================================
# 2) Generate full hex grid within city extent
# ============================================================
print(f"Generating hex grid (hex_size = {hex_size} m)...")

def generate_hex_grid(bounds, edge_length):
    xmin, ymin, xmax, ymax = bounds
    a = edge_length
    h = np.sqrt(3) * a
    dx = 3/2 * a
    dy = h

    cols = int((xmax - xmin) / dx) + 2
    rows = int((ymax - ymin) / dy) + 2

    base_hex = Polygon([
        (a * np.cos(np.radians(angle)), a * np.sin(np.radians(angle)))
        for angle in range(0, 360, 60)
    ])

    hexes = []
    for row in range(rows):
        for col in range(cols):
            x = xmin + col * dx
            y = ymin + row * dy
            if col % 2 == 1:
                y += dy / 2
            hexes.append(translate(base_hex, xoff=x, yoff=y))

    return gpd.GeoDataFrame(geometry=hexes, crs="EPSG:3857")

hex_grid = generate_hex_grid(boundary.total_bounds, hex_size)
hex_clipped = gpd.overlay(hex_grid, boundary, how="intersection")

# ============================================================
# 3) Download building footprints from Overpass
# ============================================================
print(f"Downloading building footprints for {city_name}...")

buildings_query = f"""
[out:json][timeout:60];
area["boundary"="administrative"]["name"="{city_name}"]["admin_level"="8"]->.a;
(
  way["building"](area.a);
  relation["building"](area.a);
);
out body;
>;
out skel qt;
"""

buildings_response = requests.get(overpass_url, params={"data": buildings_query})
buildings_response.raise_for_status()
buildings_data = buildings_response.json()

buildings_geojson = json2geojson(buildings_data)
buildings = gpd.GeoDataFrame.from_features(buildings_geojson["features"])
buildings = buildings.set_crs("EPSG:4326").to_crs("EPSG:3857")

# ============================================================
# 4) Keep only hexes intersecting at least 1 building
# ============================================================
print("Selecting full hexes that intersect buildings...")
joined = gpd.sjoin(buildings, hex_clipped, how="inner", predicate="intersects")
selected_ids = joined["index_right"].unique()
hex_with_buildings = hex_clipped.iloc[selected_ids].copy()

hex_with_buildings["hex_id"] = np.arange(len(hex_with_buildings))


# ============================================================
# 5) Save hex polygons + 6) Save centroids 
# ============================================================
print(f"Saving {len(hex_with_buildings)} hexes to:\n  {output_gpkg}")
hex_with_buildings.to_file(output_gpkg, layer=layer_name, driver="GPKG")

print("Calculating and exporting centroids...")
centroids = hex_with_buildings[["hex_id", "geometry"]].copy()
centroids["geometry"] = centroids.centroid
centroids.set_geometry("geometry", inplace=True)

print(f"Saving {len(centroids)} centroids to:\n  {centroid_output_gpkg}")
centroids.to_file(centroid_output_gpkg, layer=centroid_layer_name, driver="GPKG")

print("Done.")
