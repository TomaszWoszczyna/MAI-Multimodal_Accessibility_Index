import geopandas as gpd
gpd.options.io_engine = "fiona"
import os


# --- FILE PATHS ---
base_path = "main_folder_path"
netascore_path = os.path.join(base_path, "NetAScore.gpkg_path")
csv_out = os.path.join(base_path, "lts_by_osmid.csv")

# --- STEP 1: Load NetAScore layer with osm_ids
print("Loading NetAScore...")
gdf = gpd.read_file(netascore_path, layer="edge")

# --- STEP 2: Map NetAScore → LTS categories
def netascore_to_lts(score):
    if score >= 0.75:
        return 1
    elif score >= 0.5:
        return 2
    elif score >= 0.25:
        return 3
    else:
        return 4

gdf["lts"] = gdf["index_bike_ft"].apply(netascore_to_lts)

# --- STEP 3: Export osm_id + LTS to CSV for r5r
gdf[["osm_id", "lts"]].to_csv(csv_out, index=False)
print(f"LTS CSV for r5r saved to:\n{csv_out}")