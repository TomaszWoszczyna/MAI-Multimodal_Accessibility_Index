import geopandas as gpd
gpd.options.io_engine = "fiona"
import pandas as pd
import numpy as np
import ast
import requests
from osm2geojson import json2geojson
from shapely.geometry import Polygon
from shapely.affinity import translate

# --- PARAMETERS ---
city_name = "Salzburg"  
hex_size = 100  # meters (approx. in EPSG:3857)

data_root = "your_data_path"

output_gpkg = f"{data_root}/100m_hex_poi.gpkg"
layer_name = "hexes_with_pois"

overpass_url = "http://overpass-api.de/api/interpreter"

# --- STEP 1: Download POIs (ONLY NODES) from Overpass ---
print(f"Downloading *point* POIs (nodes only) for {city_name} from Overpass...")

pois_query = f"""
[out:json][timeout:120];
area["boundary"="administrative"]["name"="{city_name}"]["admin_level"="8"]->.a;
(
  node["shop"](area.a);
  node["amenity"](area.a);
  node["leisure"](area.a);
  node["tourism"](area.a);
  node["office"](area.a);
  node["craft"](area.a);
);
out body;
>;
out skel qt;
"""

resp = requests.get(overpass_url, params={"data": pois_query})
resp.raise_for_status()
pois_data = resp.json()

pois_geojson = json2geojson(pois_data)
pois = gpd.GeoDataFrame.from_features(pois_geojson["features"], crs="EPSG:4326")

# all should already be points, but we keep a small safety check
pois = pois.to_crs("EPSG:3857")
pois = pois[pois.geometry.geom_type == "Point"].copy()

print(f"Downloaded {len(pois)} point POIs")

# --- STEP 2: Parse 'tags' and extract POI type ---
print("Extracting POI types...")

def extract_poi_type(tag_val):
    # accepts dict or string
    try:
        if isinstance(tag_val, dict):
            tags = tag_val
        elif isinstance(tag_val, str):
            tags = ast.literal_eval(tag_val)
        else:
            return "invalid"

        for key in ["shop", "amenity", "leisure", "tourism", "office", "craft"]:
            if key in tags:
                return f"{key}:{tags[key]}"
        return "other"
    except Exception:
        return "invalid"

# make sure we have a 'tags' column
if "tags" not in pois.columns:
    tag_cols = [c for c in pois.columns if c not in ["geometry", "id", "@id"]]
    pois["tags"] = pois[tag_cols].to_dict(orient="records")

pois["poi_type"] = pois["tags"].apply(extract_poi_type)

# --- STEP 3: Generate hex grid covering all POIs ---
print("🔷 Generating hex grid covering POI extent...")

def generate_hex_grid(bounds, edge_length):
    xmin, ymin, xmax, ymax = bounds
    a = edge_length
    h = np.sqrt(3) * a
    dx = 3 / 2 * a
    dy = h

    cols = int((xmax - xmin) / dx) + 2
    rows = int((ymax - ymin) / dy) + 2

    base_hex = Polygon(
        [(a * np.cos(np.radians(angle)), a * np.sin(np.radians(angle)))
         for angle in range(0, 360, 60)]
    )

    hexes = []
    for row in range(rows):
        for col in range(cols):
            x = xmin + col * dx
            y = ymin + row * dy
            if col % 2 == 1:
                y += dy / 2
            hexes.append(translate(base_hex, xoff=x, yoff=y))

    return gpd.GeoDataFrame(geometry=hexes, crs="EPSG:3857")

hex_grid = generate_hex_grid(pois.total_bounds, hex_size)

# assign hex_id ONCE on the full grid
hex_grid["hex_id"] = np.arange(len(hex_grid))

# --- STEP 4: Spatial join POIs → hexes ---
print("Assigning POIs to hexes...")
joined = gpd.sjoin(pois, hex_grid[["hex_id", "geometry"]], how="inner", predicate="within")

# Keep only hexes that contain at least one POI
hex_with_pois = hex_grid[hex_grid["hex_id"].isin(joined["hex_id"].unique())].copy()

# --- STEP 5: Aggregate POI counts ---
print("Aggregating POI counts per hex...")

# total POIs per hex
poi_counts_total = joined.groupby("hex_id").size().reset_index(name="n_pois_total")

# POIs per hex by type
poi_counts_by_type = joined.groupby(["hex_id", "poi_type"]).size().unstack(fill_value=0)
poi_counts_by_type.reset_index(inplace=True)

# Merge counts to hex grid
hex_with_pois = hex_with_pois.merge(poi_counts_total, on="hex_id", how="left")
hex_with_pois = hex_with_pois.merge(poi_counts_by_type, on="hex_id", how="left")

# Fill missing with 0
count_columns = ["n_pois_total"] + list(poi_counts_by_type.columns.difference(["hex_id"]))
hex_with_pois[count_columns] = hex_with_pois[count_columns].fillna(0).astype(int)

# --- STEP 6: Export polygons to GeoPackage ---
print(f"Saving {len(hex_with_pois)} POI hexes to {output_gpkg}")
hex_with_pois.to_file(output_gpkg, layer=layer_name, driver="GPKG")

# --- STEP 7: Export centroids, KEEPING attributes ---
print("📍 Exporting centroids...")
centroids = hex_with_pois.copy()
centroids["geometry"] = centroids.geometry.centroid
centroids.set_geometry("geometry", inplace=True)
centroids_path = output_gpkg.replace(".gpkg", "_centroids.gpkg")
centroids.to_file(centroids_path, layer="hex_centroids_with_pois", driver="GPKG")

print("Done! POI hexes with correct counts created.")
