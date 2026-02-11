# MAI-Multimodal_Accessibility_Index
Code repository for MAI - Multimodal Accessibility Index
**Master’s thesis project** implementing an infrastructure-aware multimodal accessibility index (MAI) using **open data** (OSM, GTFS) and **open-source routing** (R5 via `r5r`). The workflow compares an **LTS-constrained** cycling scenario (NetAScore → LTS proxy, `max_lts`) with an **unconstrained** baseline and evaluates **ΔMAI** (constrained − unconstrained) using paired tests and spatial statistics (Moran’s I, Getis–Ord Gi*).


## Repository content
- `script/hex_creation.py`
creates Origin hex polygons + centroids from OSM/Overpass.

- `script/poi_hex.py`
retrieves POIs from OSM using Overpass Api and saves them into hexes.

- `script/lts_netascore_creation.py`
converts NetAScore edge scores to LTS classes and exports `lts_by_osmid.csv`.

- `R/mai_pipeline.R`
Runs routing + MAI scoring + Outputs Geopackages and stats.

> The provided scripts are configured for **Salzburg** as an example.  
> For a new city, duplicate them and update the parameters/queries at the top.



## 0) Overview of the workflow

1. Choose a city
2. Download input data:
   - OSM network (https://www.geofabrik.de)
   - GTFS feed for the city
   - Elevation raster (DEM), optional but recommended
4. Run NetAScore for the selected city - run official instructions: https://github.com/plus-mobilitylab
5. Run 3 Python preprocessing scripts:
   - hex grid creation
   - POIS extraction 
   - NetAScore to LTS assignment

6. Run the R pipeline:
   - set travel time threshold T
   - set CRS handling
   - set departure time for transit routing
  
## 1) Requirements

1.1 Software
- Docker (for NetAScore)
- Python 3.10+
- R 4.2+
- Java

1.2 Python packages
- geopandas
- shapely
- rasterio
- requests
- osm2geojson
- numpy
- pandas

1.3 R packages
- r5r
- sf
- data.table
- dplyr

## 2) Input data structure
   Create a folder and put following data inside:
   - OSM.pbf - OSM extract for the routing engine (r5r)
   - GTFS.zip - required by r5r
   - netascore_city.gpkg - output from NEtAScore (Edge layer)
   - lts_by_osmid.csv - output from the python script
   - hex_building_centroids.gpkg
   - hex_building.gpkg
   - hex_poi_centroids.gpkg
   - DEM.tif

## 3) Step-by-step: run the pipeline
1. Pick a city (e.g., `Salzburg`, `Cracow` etc.) and download **OSM.pbf** network
2. Download a **GTFS.zip** for the selected city.
3. Download an **elevation raster** for the selected city (I used a 5m DEM)
4. Run **NetAScore** for your selected city, instructions here: https://github.com/plus-mobilitylab/netascore?tab=readme-ov-file
5. Run the provided **3 python scripts**.
6. Open **mai_pipeline.R** and follow the instructions there.

# 3.1) User parameters for mai_pipeline.R
- `T` = time threshold, time limit for travel
- `dep_time` = departure datetime (must match GTFS service)
- `k_nearest` = how many closest destinations are used for calculating medan duration
- `crs_metric` - must be a projected CRS in meters, selected for a specified area of interest

## 4) Outputs
The R script writes to the path specified at the beginning of the script (data_path)
- `mai.constrained.gpkg` - main MAI layer 
- `mai_comparison_const_unconstr.gpkg` - constrained vs unconstrained comparison 
- `route_comparison_sameOD.gpkg` - route comparison
- `delta_mai_hotspots.gpkg` - spatial statistics for ΔMAI 
- `mai_statistics_output.txt` - text log of statistical tests 
  
## 5) Credits
- OpenStreetMap: https://www.openstreetmap.org/
- Overpass Api: https://overpass-turbo.eu/index.html
- NetAScore: https://github.com/plus-mobilitylab/netascore
- r5r: https://ipeagit.github.io/r5r/



