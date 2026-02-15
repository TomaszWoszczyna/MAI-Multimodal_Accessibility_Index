# =============================================================
# Infrastructure-Aware Multimodal Accessibility (MAI) Pipeline
# =============================================================

# --- LIBRARIES ---
library(rJava)
library(r5r)
library(sf)
library(dplyr)
library(data.table)
library(scales)
library(lubridate)
library(tidytransit)
library(units)
library(magrittr)
library(tidyr)
library(progressr)
library(purrr)
library(spdep)
library(spatialreg)
library(corrr)
library(broom)
library(GWmodel)
library(ggplot2)


# =============================================================
# USER PARAMETERS
# =============================================================
T         <- 2          # travel time budget in minutes
k_nearest <- 10          # for duration indicator only
dep_time  <- as.POSIXct("2025-05-19 07:00:00")  # make sure GTFS has service

max_lts_val <- 1         # your constraint threshold
bike_speed_kmh <- 12
walk_speed_kmh <- 4

tol_min <- 0.5           # tolerance for transit_used classification (minutes)

# NetAScore scoring batches
neta_batch_size <- 20000

# Visual sample export
vis_n_per_type <- 50

# =============================================================
# FILE PATHS 
# =============================================================
data_path       <- "main_folder_path"
centroids_path  <- file.path(data_path, "100m_hex_building_centroids.gpkg")
hexpoly_path    <- file.path(data_path, "100m_hex_building.gpkg")
poi_path        <- file.path(data_path, "100m_hex_poi_centroids.gpkg")


# GTFS feeds 
gtfs_files <- c(
  file.path(data_path, "your_gtfs_file.zip")
)

netascore_path  <- file.path(data_path, "your_NetAScore.gpkg")
lts_csv_path    <- file.path(data_path, "lts_by_osmid.csv")

# =============================================================
# CRS SETTINGS (USER SETS EPSG NUMBER BEFORE RUNNING)
# =============================================================

crs_metric <- 32632  # USER sets ANY projected CRS EPSG (meters), e.g. 32632/32633/32634/3035/31256 etc.




# =============================================================
# HELPERS
# =============================================================
minmax01 <- function(x, minv, maxv) {
  x <- as.numeric(x)
  if (isTRUE(maxv == minv)) {
    out <- rep(0, length(x))
    out[is.na(x)] <- NA_real_
    return(out)
  }
  out <- (x - minv) / (maxv - minv)
  out[is.na(x)] <- NA_real_
  out
}
clip01 <- function(x) pmin(1, pmax(0, x))
drop_sf <- function(x) if (inherits(x, "sf")) st_drop_geometry(x) else x


# =============================================================
# 0. ROUTING NETWORK & INPUTS
# =============================================================
r5r_network <- setup_r5(data_path, verbose = TRUE, overwrite = FALSE)

edge_lts <- read.csv(lts_csv_path)

hex_centroids <- st_read(centroids_path, layer = "hex_centroids", quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(id = as.character(hex_id))

hex_polygons <- st_read(hexpoly_path, layer = "hexes_with_buildings", quiet = TRUE) %>%
  mutate(id = as.character(hex_id))

pois <- st_read(poi_path, layer = "hex_centroids_with_pois", quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(
    id       = as.character(hex_id),
    poi_type = "general"
  )

poi_counts <- pois %>%
  st_drop_geometry() %>%
  select(to_id = id, poi_count = n_pois_total)

# -------------------------------------------------------------
# FAST coordinate lookup tables 
# -------------------------------------------------------------
coords_hex <- st_coordinates(hex_centroids)
hex_xy <- hex_centroids %>%
  st_drop_geometry() %>%
  transmute(id = as.character(id),
            lon = coords_hex[, 1],
            lat = coords_hex[, 2])

coords_pois <- st_coordinates(pois)
pois_xy <- pois %>%
  st_drop_geometry() %>%
  transmute(id = as.character(id),
            lon = coords_pois[, 1],
            lat = coords_pois[, 2])

# =============================================================
# 1. CONSTRAINED MULTIMODAL ACCESSIBILITY
# =============================================================

ttmatrix <- travel_time_matrix(
  r5r_network,
  origins            = hex_centroids,
  destinations       = pois,
  mode               = c("BICYCLE", "TRANSIT"),
  departure_datetime = dep_time,
  max_trip_duration  = T,
  max_bike_time      = T,
  bike_speed         = bike_speed_kmh,
  verbose            = FALSE,
  new_lts            = edge_lts,
  max_lts            = max_lts_val
)

ttmatrix_walk <- travel_time_matrix(
  r5r_network,
  origins            = hex_centroids,
  destinations       = pois,
  mode               = c("WALK", "TRANSIT"),
  departure_datetime = dep_time,
  max_trip_duration  = T,
  max_walk_time      = T,
  walk_speed         = walk_speed_kmh,
  verbose            = TRUE
)

# --- 1.1 CUMULATIVE OPPORTUNITIES (reachable POIs) ---
pois_bike <- ttmatrix %>%
  select(from_id, to_id) %>%
  distinct()

pois_walk <- ttmatrix_walk %>%
  select(from_id, to_id) %>%
  distinct()

all_pois <- bind_rows(pois_bike, pois_walk) %>%
  distinct(from_id, to_id)

cumulative_access_total <- all_pois %>%
  left_join(poi_counts, by = "to_id") %>%
  group_by(from_id) %>%
  summarise(n_pois_total = sum(poi_count, na.rm = TRUE), .groups = "drop")

# Duration indicator = median travel_time_p50 among k nearest destinations (bike+transit)
access_summary <- ttmatrix %>%
  filter(!is.na(travel_time_p50)) %>%
  group_by(from_id) %>%
  summarise(
    n_dest_reached  = n(),
    duration_median = {
      x <- sort(as.numeric(travel_time_p50))
      if (length(x) >= k_nearest) median(x[1:k_nearest]) else T
    },
    .groups = "drop"
  )

# =============================================================
# 2. DETOUR RATIO + TRANSIT USED CLASSIFICATION (FAST, MATRIX-BASED)
# =============================================================

valid_pairs_bt_constr <- ttmatrix %>%
  filter(!is.na(travel_time_p50), as.numeric(travel_time_p50) <= T) %>%
  transmute(from_id, to_id, tt_bt = as.numeric(travel_time_p50)) %>%
  distinct()

message("Computing bike-only matrices for detour + transit-used classification ...")

tt_bike_only_constr <- travel_time_matrix(
  r5r_network,
  origins            = hex_centroids,
  destinations       = pois,
  mode               = "BICYCLE",
  departure_datetime = dep_time,
  max_trip_duration  = T,
  max_bike_time      = T,
  bike_speed         = bike_speed_kmh,
  verbose            = FALSE,
  new_lts            = edge_lts,
  max_lts            = max_lts_val
)

tt_bike_only_fast <- travel_time_matrix(
  r5r_network,
  origins            = hex_centroids,
  destinations       = pois,
  mode               = "BICYCLE",
  departure_datetime = dep_time,
  max_trip_duration  = T,
  max_bike_time      = T,
  bike_speed         = bike_speed_kmh,
  verbose            = FALSE
)

bike_pair_times <- valid_pairs_bt_constr %>%
  left_join(tt_bike_only_constr %>%
              select(from_id, to_id, tt_bike_lts = travel_time_p50) %>%
              mutate(tt_bike_lts = as.numeric(tt_bike_lts)),
            by = c("from_id","to_id")) %>%
  left_join(tt_bike_only_fast %>%
              select(from_id, to_id, tt_bike_fast = travel_time_p50) %>%
              mutate(tt_bike_fast = as.numeric(tt_bike_fast)),
            by = c("from_id","to_id"))

detour_df <- bike_pair_times %>%
  filter(!is.na(tt_bike_lts), !is.na(tt_bike_fast)) %>%
  mutate(
    detour_ratio = tt_bike_lts / pmax(tt_bike_fast, 1e-6),
    detour_ratio = pmax(detour_ratio, 1)
  ) %>%
  group_by(from_id) %>%
  summarise(detour_ratio = median(detour_ratio, na.rm = TRUE), .groups = "drop")

valid_pairs_bt_constr <- bike_pair_times %>%
  mutate(
    transit_used = is.na(tt_bike_lts) | (tt_bt + tol_min < tt_bike_lts)
  ) %>%
  select(from_id, to_id, tt_bt, tt_bike_lts, transit_used)

pairs_transit_constr <- valid_pairs_bt_constr %>%
  filter(transit_used) %>%
  select(from_id, to_id)

pairs_bike_constr <- valid_pairs_bt_constr %>%
  filter(!transit_used) %>%
  select(from_id, to_id)

message("Constrained OD split: transit_used = ", nrow(pairs_transit_constr),
        " | bike_only = ", nrow(pairs_bike_constr))

# =============================================================
# 3. PUBLIC TRANSPORT QUALITY (ÖV-GÜTEKLASSE)
# =============================================================

compute_stop_freq_one_feed <- function(path_zip) {
  gtfs <- read_gtfs(path_zip)
  
  gtfs_calendar   <- gtfs$calendar
  gtfs_stop_times <- gtfs$stop_times
  gtfs_trips      <- gtfs$trips
  gtfs_stops      <- gtfs$stops
  
  active_service_ids <- gtfs_calendar %>%
    filter(monday == 1) %>%   # adjust if needed
    pull(service_id)
  
  stop_departures <- gtfs_stop_times %>%
    left_join(gtfs_trips, by = "trip_id") %>%
    filter(service_id %in% active_service_ids) %>%
    mutate(
      departure_secs = period_to_seconds(hms(as.character(departure_time))),
      in_peak = departure_secs >= 25200 & departure_secs <= 32400
    ) %>%
    filter(in_peak)
  
  stop_freq <- stop_departures %>%
    group_by(stop_id) %>%
    summarise(freq_peak = n(), .groups = "drop")
  
  list(stops = gtfs_stops, stop_freq = stop_freq)
}

pt_components <- map(gtfs_files, compute_stop_freq_one_feed)

stop_freq_all <- bind_rows(map(pt_components, "stop_freq")) %>%
  group_by(stop_id) %>%
  summarise(freq_peak = sum(freq_peak, na.rm = TRUE), .groups = "drop")

stops_all <- bind_rows(map(pt_components, "stops")) %>%
  filter(stop_id %in% stop_freq_all$stop_id) %>%
  distinct(stop_id, .keep_all = TRUE)

stop_sf <- stops_all %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

hex_centroids_proj <- st_transform(hex_centroids, crs_metric)
stop_sf_proj       <- st_transform(stop_sf, crs_metric)
nearest_stop       <- st_nearest_feature(hex_centroids_proj, stop_sf_proj)

hex_pt <- hex_centroids %>%
  mutate(
    nearest_stop_id = stop_sf$stop_id[nearest_stop],
    stop_freq       = stop_freq_all$freq_peak[
      match(stop_sf$stop_id[nearest_stop], stop_freq_all$stop_id)
    ],
    distance_m      = st_distance(hex_centroids_proj,
                                  stop_sf_proj[nearest_stop, ],
                                  by_element = TRUE) %>% drop_units(),
    ov_class = case_when(
      distance_m <= 300 & stop_freq >= 16 ~ "A",
      distance_m <= 300 & stop_freq >= 8  ~ "B",
      distance_m <= 300 & stop_freq >= 4  ~ "C",
      distance_m <= 300 & stop_freq >= 2  ~ "D",
      distance_m <= 300                   ~ "E",
      distance_m <= 500 & stop_freq >= 16 ~ "B",
      distance_m <= 500 & stop_freq >= 8  ~ "C",
      distance_m <= 500 & stop_freq >= 4  ~ "D",
      distance_m <= 500                   ~ "E",
      distance_m <= 1000 & stop_freq >= 4 ~ "D",
      distance_m <= 1000                  ~ "E",
      TRUE                                ~ "G"
    ),
    ov_score = recode(
      ov_class,
      A = 1, B = 2, C = 3, D = 4, E = 5, F = 6, G = 7
    )
  )

pt_scores <- hex_pt %>%
  select(id, ov_score) %>%
  st_drop_geometry()

# =============================================================
# 4. NETASCORE OVERLAY
# =============================================================

neta_edges <- st_read(netascore_path, layer = "edge", quiet = TRUE)

# LUT: osm_id -> (neta score, length)
neta_lut <- neta_edges %>%
  st_drop_geometry() %>%
  transmute(
    osm_id = as.character(osm_id),
    neta   = as.numeric(index_bike_ft),
    len_m  = as.numeric(length)
  ) %>%
  filter(!is.na(osm_id), !is.na(neta), !is.na(len_m), len_m > 0) %>%
  group_by(osm_id) %>%
  summarise(
    neta  = weighted.mean(neta, w = len_m, na.rm = TRUE),
    len_m = sum(len_m, na.rm = TRUE),
    .groups = "drop"
  )

setDT(neta_lut); setkey(neta_lut, osm_id)

compute_od_netascore_from_bike_itins <- function(bike_itins_df) {
  bike_itins_df <- drop_sf(bike_itins_df)
  
  seg_w <- bike_itins_df %>%
    distinct(from_id, to_id, option, segment, distance)
  
  dt <- as.data.table(bike_itins_df)
  
  # list column safety
  if (!is.list(dt$osm_id_list)) {
    dt[, osm_id_list := strsplit(as.character(osm_id_list), ";|,|\\s+")]
  }
  
  dt_exp <- dt[, .(osm_id = as.character(unlist(osm_id_list))),
               by = .(from_id, to_id, option, segment)]
  
  dt_exp <- neta_lut[dt_exp, on = "osm_id"]
  
  seg_neta <- dt_exp[!is.na(neta) & !is.na(len_m),
                     .(seg_neta = weighted.mean(neta, w = len_m, na.rm = TRUE)),
                     by = .(from_id, to_id, option, segment)]
  
  seg_neta <- merge(
    seg_neta,
    as.data.table(seg_w),
    by = c("from_id","to_id","option","segment"),
    all.x = TRUE
  )
  
  best_opt <- as.data.table(bike_itins_df) %>%
    as_tibble() %>%
    distinct(from_id, to_id, option, total_duration) %>%
    group_by(from_id, to_id) %>%
    slice_min(total_duration, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(from_id, to_id, option)
  
  seg_neta <- merge(seg_neta, as.data.table(best_opt),
                    by = c("from_id","to_id","option"),
                    all = FALSE)
  
  seg_neta[, w := as.numeric(distance)]
  seg_neta[is.na(w) | w <= 0, w := 1]
  
  od <- seg_neta[, .(
    netascore_weighted = weighted.mean(seg_neta, w = w, na.rm = TRUE)
  ), by = .(from_id, to_id)]
  
  as_tibble(od)
}

score_pairs_netascore <- function(pairs_df, mode_vec, constrained = FALSE, batch_size = 20000) {
  pairs_df <- as_tibble(pairs_df) %>% distinct(from_id, to_id)
  if (nrow(pairs_df) == 0) {
    return(tibble(from_id = character(), to_id = character(), netascore_weighted = numeric()))
  }
  
  idx <- split(seq_len(nrow(pairs_df)),
               ceiling(seq_len(nrow(pairs_df)) / batch_size))
  
  out <- vector("list", length(idx))
  
  for (i in seq_along(idx)) {
    message("NetAScore batch ", i, " / ", length(idx),
            " (", length(idx[[i]]), " OD)")
    
    pb <- pairs_df[idx[[i]], , drop = FALSE]
    
    o <- pb %>%
      left_join(hex_xy, by = c("from_id" = "id")) %>%
      transmute(id = from_id, lon = lon, lat = lat)
    
    d <- pb %>%
      left_join(pois_xy, by = c("to_id" = "id")) %>%
      transmute(id = to_id, lon = lon, lat = lat)
    
    ok_coords <- is.finite(o$lon) & is.finite(o$lat) & is.finite(d$lon) & is.finite(d$lat)
    if (!all(ok_coords)) {
      o <- o[ok_coords, , drop = FALSE]
      d <- d[ok_coords, , drop = FALSE]
      pb <- pb[ok_coords, , drop = FALSE]
    }
    
    if (nrow(pb) == 0) {
      out[[i]] <- tibble(from_id = character(), to_id = character(), netascore_weighted = numeric())
      next
    }
    
    args <- list(
      r5r_network,
      origins            = o,
      destinations       = d,
      mode               = mode_vec,
      departure_datetime = dep_time,
      max_trip_duration  = T,
      max_bike_time      = T,
      bike_speed         = bike_speed_kmh,
      all_to_all         = FALSE,
      verbose            = FALSE,
      drop_geometry      = FALSE,
      osm_link_ids       = TRUE
    )
    
    if (constrained) {
      args$new_lts <- edge_lts
      args$max_lts <- max_lts_val
    }
    
    it <- do.call(detailed_itineraries, args)
    
    if (is.null(it) || nrow(it) == 0) {
      out[[i]] <- tibble(from_id = character(), to_id = character(), netascore_weighted = numeric())
      rm(it); gc()
      next
    }
    
    bike_it <- it %>%
      filter(mode == "BICYCLE") %>%
      select(from_id, to_id, option, segment, distance, total_duration, osm_id_list) %>%
      st_drop_geometry()
    
    if (!"option" %in% names(bike_it))  bike_it$option  <- 1L
    if (!"segment" %in% names(bike_it)) bike_it$segment <- 1L
    
    if (nrow(bike_it) == 0) {
      out[[i]] <- tibble(from_id = character(), to_id = character(), netascore_weighted = numeric())
      rm(it, bike_it); gc()
      next
    }
    
    out[[i]] <- compute_od_netascore_from_bike_itins(bike_it)
    
    rm(it, bike_it); gc()
  }
  
  bind_rows(out) %>%
    distinct(from_id, to_id, .keep_all = TRUE)
}

# ---- 4A) constrained neta ----
message("NetAScore constrained: transit-used pairs (multimodal) ...")
leg_scores_bt_constr <- score_pairs_netascore(
  pairs_transit_constr,
  mode_vec    = c("BICYCLE", "TRANSIT"),
  constrained = TRUE,
  batch_size  = neta_batch_size
)

message("NetAScore constrained: bike-only pairs ...")
leg_scores_bike_constr <- score_pairs_netascore(
  pairs_bike_constr,
  mode_vec    = "BICYCLE",
  constrained = TRUE,
  batch_size  = neta_batch_size
)

leg_scores_all <- bind_rows(leg_scores_bt_constr, leg_scores_bike_constr) %>%
  distinct(from_id, to_id, .keep_all = TRUE)

mean_netascore_from <- leg_scores_all %>%
  group_by(from_id) %>%
  summarise(mean_netascore = mean(netascore_weighted, na.rm = TRUE), .groups = "drop")

# =============================================================
# 5. UNCONSTRAINED MULTIMODAL + NETASCORE 
# =============================================================

message("Computing UNCONSTRAINED multimodal accessibility (same definitions)...")

ttmatrix_unconstr <- travel_time_matrix(
  r5r_network,
  origins            = hex_centroids,
  destinations       = pois,
  mode               = c("BICYCLE", "TRANSIT"),
  departure_datetime = dep_time,
  max_trip_duration  = T,
  max_bike_time      = T,
  bike_speed         = bike_speed_kmh,
  verbose            = FALSE
)

valid_pairs_bt_u <- ttmatrix_unconstr %>%
  filter(!is.na(travel_time_p50), as.numeric(travel_time_p50) <= T) %>%
  transmute(from_id, to_id, tt_bt = as.numeric(travel_time_p50)) %>%
  distinct()

# Use bike-only-fast already computed (tt_bike_only_fast)
valid_pairs_bt_u <- valid_pairs_bt_u %>%
  left_join(tt_bike_only_fast %>%
              select(from_id, to_id, tt_bike = travel_time_p50) %>%
              mutate(tt_bike = as.numeric(tt_bike)),
            by = c("from_id","to_id")) %>%
  mutate(
    transit_used = is.na(tt_bike) | (tt_bt + tol_min < tt_bike)
  )

pairs_transit_u <- valid_pairs_bt_u %>%
  filter(transit_used) %>%
  select(from_id, to_id)

pairs_bike_u <- valid_pairs_bt_u %>%
  filter(!transit_used) %>%
  select(from_id, to_id)

message("Unconstrained OD split: transit_used = ", nrow(pairs_transit_u),
        " | bike_only = ", nrow(pairs_bike_u))

message("NetAScore unconstrained: transit-used pairs (multimodal) ...")
leg_scores_bt_u <- score_pairs_netascore(
  pairs_transit_u,
  mode_vec    = c("BICYCLE", "TRANSIT"),
  constrained = FALSE,
  batch_size  = neta_batch_size
)

message("NetAScore unconstrained: bike-only pairs ...")
leg_scores_bike_u <- score_pairs_netascore(
  pairs_bike_u,
  mode_vec    = "BICYCLE",
  constrained = FALSE,
  batch_size  = neta_batch_size
)

leg_scores_all_u <- bind_rows(leg_scores_bt_u, leg_scores_bike_u) %>%
  distinct(from_id, to_id, .keep_all = TRUE)

mean_netascore_from_u <- leg_scores_all_u %>%
  group_by(from_id) %>%
  summarise(mean_netascore = mean(netascore_weighted, na.rm = TRUE), .groups = "drop")

# --- Unconstrained reachables for cumulative access
pois_bike_u <- ttmatrix_unconstr %>%
  select(from_id, to_id) %>%
  distinct()

# Use SAME walk set as constrained
all_pois_u <- bind_rows(pois_bike_u, pois_walk) %>%
  distinct(from_id, to_id)

cumulative_access_total_u <- all_pois_u %>%
  left_join(poi_counts, by = "to_id") %>%
  group_by(from_id) %>%
  summarise(n_pois_total = sum(poi_count, na.rm = TRUE), .groups = "drop")

access_summary_u <- ttmatrix_unconstr %>%
  filter(!is.na(travel_time_p50)) %>%
  group_by(from_id) %>%
  summarise(
    n_dest_reached  = n(),
    duration_median = {
      x <- sort(as.numeric(travel_time_p50))
      if (length(x) >= k_nearest) median(x[1:k_nearest]) else T
    },
    .groups = "drop"
  )

detour_df_u <- hex_centroids %>%
  st_drop_geometry() %>%
  transmute(from_id = id, detour_ratio = 1)

# =============================================================
# 6. BUILD INDICATORS + SCORE + EXPORT MAI_CONSTRAINED
# =============================================================

raw_constr <- cumulative_access_total %>%
  left_join(access_summary,      by = "from_id") %>%
  left_join(detour_df,           by = "from_id") %>%
  left_join(pt_scores,           by = c("from_id" = "id")) %>%
  left_join(mean_netascore_from, by = "from_id") %>%
  transmute(
    id = from_id,
    scenario = "constrained",
    duration_median,
    n_pois_total,
    detour_ratio,
    ov_score,
    mean_netascore
  )

raw_unconstr <- cumulative_access_total_u %>%
  left_join(access_summary_u,      by = "from_id") %>%
  left_join(detour_df_u,           by = "from_id") %>%
  left_join(pt_scores,             by = c("from_id" = "id")) %>%
  left_join(mean_netascore_from_u, by = "from_id") %>%
  transmute(
    id = from_id,
    scenario = "unconstrained",
    duration_median,
    n_pois_total,
    detour_ratio,
    ov_score,
    mean_netascore
  )

both_raw <- bind_rows(raw_constr, raw_unconstr) %>%
  ungroup() %>%
  mutate(
    duration_median  = as.numeric(duration_median),
    n_pois_total     = as.numeric(n_pois_total),
    detour_ratio     = as.numeric(detour_ratio),
    ov_score         = as.numeric(ov_score),
    mean_netascore   = as.numeric(mean_netascore)
  )

mins <- both_raw %>%
  summarise(
    duration_min = min(.data$duration_median, na.rm = TRUE),
    duration_max = max(.data$duration_median, na.rm = TRUE),
    pois_min     = min(.data$n_pois_total,  na.rm = TRUE),
    pois_max     = max(.data$n_pois_total,  na.rm = TRUE),
    det_min      = min(.data$detour_ratio,  na.rm = TRUE),
    det_max      = max(.data$detour_ratio,  na.rm = TRUE)
  )

dur_min <- mins$duration_min[[1]]; dur_max <- mins$duration_max[[1]]
poi_min <- mins$pois_min[[1]];     poi_max <- mins$pois_max[[1]]
det_min <- mins$det_min[[1]];      det_max <- mins$det_max[[1]]

both_scored <- both_raw %>%
  mutate(
    duration_median = tidyr::replace_na(.data$duration_median, T),
    n_pois_total    = tidyr::replace_na(.data$n_pois_total,  0),
    detour_ratio    = tidyr::replace_na(.data$detour_ratio, det_max),
    ov_score        = tidyr::replace_na(.data$ov_score, 7),
    mean_netascore  = clip01(tidyr::replace_na(.data$mean_netascore, 0)),
    
    duration_rescaled = clip01(minmax01(.data$duration_median, dur_min, dur_max)),
    n_pois_rescaled   = clip01(minmax01(.data$n_pois_total,  poi_min, poi_max)),
    detour_rescaled   = clip01(minmax01(.data$detour_ratio,  det_min, det_max)),
    
    duration_norm = 1 - duration_rescaled,
    n_pois_norm   = n_pois_rescaled,
    pt_score_norm = (7 - ov_score) / 6,
    detour_norm   = 1 - detour_rescaled,
    neta_norm     = mean_netascore,
    
    MAI = rowMeans(across(c(duration_norm, n_pois_norm, pt_score_norm, detour_norm, neta_norm)))
  )

mai_constr_tbl <- both_scored %>%
  filter(scenario == "constrained") %>%
  select(id, MAI, duration_norm, n_pois_norm, pt_score_norm, detour_norm, neta_norm,
         duration_median, n_pois_total, detour_ratio, ov_score, mean_netascore)

mai_constr_geom <- hex_polygons %>%
  left_join(mai_constr_tbl, by = "id") %>%
  mutate(
    MAI            = replace_na(MAI, 0),
    duration_norm  = replace_na(duration_norm, 0),
    n_pois_norm    = replace_na(n_pois_norm, 0),
    pt_score_norm  = replace_na(pt_score_norm, 0),
    detour_norm    = replace_na(detour_norm, 0),
    neta_norm      = replace_na(neta_norm, 0),
    duration_median= replace_na(duration_median, T),
    n_pois_total   = replace_na(n_pois_total, 0),
    detour_ratio   = replace_na(detour_ratio, det_max),
    ov_score       = replace_na(ov_score, 7),
    mean_netascore = replace_na(mean_netascore, 0)
  )

st_write(
  mai_constr_geom,
  file.path(data_path, "mai_constrained.gpkg"),
  layer = "MAI_CONSTRAINED",
  delete_layer = TRUE
)

message("Wrote MAI_CONSTRAINED to: ", file.path(data_path, "mai_constrained.gpkg"))

# =============================================================
# 7. ROUTE COMPARISON EXPORT (same OD pairs for both scenarios)
# =============================================================

# ---- helper: sample OD pairs safely (unique pairs, reproducible)
sample_pairs <- function(pairs_df, n = 50, seed = 60) {
  pairs_df <- as_tibble(pairs_df) %>%
    distinct(from_id, to_id)
  
  if (nrow(pairs_df) == 0) return(pairs_df)
  
  set.seed(seed)
  
  if (nrow(pairs_df) <= n) {
    return(pairs_df)
  }
  
  pairs_df %>% slice_sample(n = n)
}
# params
vis_n_per_type <- 50
bike_speed_kmh <- 12
max_lts_val    <- 1

# ---- 7.1) Build a pool of OD pairs that exist in BOTH scenarios 
pairs_pool <- inner_join(
  valid_pairs_bt_constr %>% select(from_id, to_id) %>% distinct(),
  valid_pairs_bt_u      %>% select(from_id, to_id) %>% distinct(),
  by = c("from_id", "to_id")
)

# ---- 7.2) Sample OD pairs ONCE
pairs_cmp_s <- sample_pairs(pairs_pool, n = vis_n_per_type, seed = 60)

# Attach "transit_used" flags from BOTH scenarios
flags <- full_join(
  valid_pairs_bt_constr %>% select(from_id, to_id, transit_used_constr = transit_used),
  valid_pairs_bt_u      %>% select(from_id, to_id, transit_used_unconstr = transit_used),
  by = c("from_id", "to_id")
)

pairs_cmp_s <- pairs_cmp_s %>%
  left_join(flags, by = c("from_id","to_id")) %>%
  mutate(pair_id = paste0(from_id, "__", to_id))

# ---- 7.3) Helper: keep ONLY best option per OD
keep_best_option <- function(it_df) {
  it_df <- it_df %>%
    mutate(
      from_id = as.character(from_id),
      to_id   = as.character(to_id),
      option  = if ("option" %in% names(.)) option else 1L,
      total_duration = as.numeric(total_duration)
    )
  
  best <- it_df %>%
    st_drop_geometry() %>%
    distinct(from_id, to_id, option, total_duration) %>%
    group_by(from_id, to_id) %>%
    slice_min(total_duration, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(from_id, to_id, option)
  
  it_df %>% inner_join(best, by = c("from_id","to_id","option"))
}

# ---- 7.4) Core: compute BOTH constrained + unconstrained for same OD sample
get_routes_compare <- function(pairs_df, mode_vec, keep_modes = NULL, label = "compare") {
  pairs_df <- as_tibble(pairs_df) %>% distinct(from_id, to_id, .keep_all = TRUE)
  if (nrow(pairs_df) == 0) return(st_sf())
  
  o <- pairs_df %>%
    left_join(hex_xy, by = c("from_id" = "id")) %>%
    transmute(id = from_id, lon = lon, lat = lat)
  
  d <- pairs_df %>%
    left_join(pois_xy, by = c("to_id" = "id")) %>%
    transmute(id = to_id, lon = lon, lat = lat)
  
  ok <- is.finite(o$lon) & is.finite(o$lat) & is.finite(d$lon) & is.finite(d$lat)
  o <- o[ok, , drop = FALSE]
  d <- d[ok, , drop = FALSE]
  pairs_df <- pairs_df[ok, , drop = FALSE]
  if (nrow(pairs_df) == 0) return(st_sf())
  
  base_args <- list(
    r5r_network,
    origins            = o,
    destinations       = d,
    mode               = mode_vec,
    departure_datetime = dep_time,
    max_trip_duration  = T,
    max_bike_time      = T,
    bike_speed         = bike_speed_kmh,
    all_to_all         = FALSE,
    verbose            = FALSE,
    drop_geometry      = FALSE,
    osm_link_ids       = FALSE
  )
  
  it_u <- do.call(detailed_itineraries, base_args)
  if (!is.null(it_u) && nrow(it_u) > 0) {
    it_u <- it_u %>% mutate(scenario = "unconstrained")
    it_u <- keep_best_option(it_u)
  } else it_u <- NULL
  
  args_c <- base_args
  args_c$new_lts <- edge_lts
  args_c$max_lts <- max_lts_val
  
  it_c <- do.call(detailed_itineraries, args_c)
  if (!is.null(it_c) && nrow(it_c) > 0) {
    it_c <- it_c %>% mutate(scenario = "constrained")
    it_c <- keep_best_option(it_c)
  } else it_c <- NULL
  
  if (is.null(it_u) && is.null(it_c)) return(st_sf())
  
  out <- bind_rows(it_c, it_u) %>%
    mutate(
      from_id = as.character(from_id),
      to_id   = as.character(to_id),
      pair_id = paste0(from_id, "__", to_id),
      sample_type = label
    ) %>%
    left_join(
      pairs_df %>% select(from_id, to_id, pair_id, transit_used_constr, transit_used_unconstr),
      by = c("from_id","to_id","pair_id")
    )
  
  if (!is.null(keep_modes) && "mode" %in% names(out)) out <- out %>% filter(mode %in% keep_modes)
  if (!inherits(out, "sf")) out <- st_as_sf(out)
  out
}

# ---- 7.5) Compute comparison routes
vis_cmp_bike <- get_routes_compare(
  pairs_cmp_s, mode_vec = "BICYCLE", keep_modes = "BICYCLE", label = "compare_bike_only"
)

vis_cmp_multi <- get_routes_compare(
  pairs_cmp_s, mode_vec = c("BICYCLE","TRANSIT"), keep_modes = NULL, label = "compare_multimodal"
)

dissolve_routes <- function(x) {
  if (!inherits(x, "sf") || nrow(x) == 0) return(st_sf())
  x %>%
    group_by(pair_id, scenario, from_id, to_id, transit_used_constr, transit_used_unconstr) %>%
    summarise(
      total_duration = suppressWarnings(min(as.numeric(total_duration), na.rm = TRUE)),
      .groups = "drop"
    )
}

vis_cmp_bike_one  <- dissolve_routes(vis_cmp_bike)
vis_cmp_multi_one <- dissolve_routes(vis_cmp_multi)

# ---- 7.6) Export
vis_cmp_gpkg <- file.path(data_path, "route_comparison_sameOD.gpkg")
if (file.exists(vis_cmp_gpkg)) file.remove(vis_cmp_gpkg)

write_if_nonempty <- function(x, layer_name) {
  if (!inherits(x, "sf") || nrow(x) == 0) return(invisible(NULL))
  st_write(st_transform(x, crs_route), vis_cmp_gpkg,
           layer = layer_name, delete_layer = TRUE, quiet = TRUE)
}

write_if_nonempty(vis_cmp_bike,      "COMPARE_BIKE_segments")
write_if_nonempty(vis_cmp_bike_one,  "COMPARE_BIKE_oneLine")
write_if_nonempty(vis_cmp_multi,     "COMPARE_MULTI_segments")
write_if_nonempty(vis_cmp_multi_one, "COMPARE_MULTI_oneLine")

message("Wrote route comparison (same OD) to: ", vis_cmp_gpkg)


# =============================================================
# 8. HYPOTHESIS 1 – COMPARABLE PAIRED TESTS
#    + SAVE ALL STATS OUTPUT TO TXT
# =============================================================

stats_txt <- file.path(data_path, "mai_statistics_output.txt")

# capture console output to file + still show in console
sink(stats_txt, split = TRUE)
cat("=============================================================\n")
cat("MAI STATISTICS OUTPUT\n")
cat("Timestamp: ", format(Sys.time()), "\n")
cat("Data path: ", data_path, "\n")
cat("=============================================================\n\n")

# Requires: both_scored (id, scenario, MAI) and hex_polygons (id, geometry)
mai_compare <- both_scored %>%
  select(id, scenario, MAI) %>%
  tidyr::pivot_wider(names_from = scenario, values_from = MAI) %>%
  mutate(mai_diff = constrained - unconstrained)

# Use COMPLETE pairs for statistical inference (don’t replace NA with 0 for tests)
mai_compare_complete <- mai_compare %>%
  filter(is.finite(constrained), is.finite(unconstrained), is.finite(mai_diff))

cat("-------------------------------------------------------------\n")
cat("H1 – COMPARABLE PAIRED TESTS\n")
cat("-------------------------------------------------------------\n")
cat("H1 – number of paired hexes used: ", nrow(mai_compare_complete), "\n\n")

cat("H1 – Paired t-test (complete pairs):\n")
print(
  t.test(mai_compare_complete$constrained,
         mai_compare_complete$unconstrained,
         paired = TRUE)
)
cat("\n")

cat("H1 – Wilcoxon signed-rank test (complete pairs):\n")
print(
  wilcox.test(mai_compare_complete$constrained,
              mai_compare_complete$unconstrained,
              paired = TRUE)
)
cat("\n")

mai_summary <- mai_compare_complete %>%
  summarise(
    mean_constr   = mean(constrained),
    mean_unconstr = mean(unconstrained),
    mean_diff     = mean(mai_diff),
    abs_diff      = mean(abs(mai_diff)),
    sd_diff       = sd(mai_diff)
  )

cat("H1 – Summary stats:\n")
print(mai_summary)
cat("\n")

d_paired <- mai_summary$mean_diff / mai_summary$sd_diff
cat("H1 – Effect size (paired Cohen's d): ", round(d_paired, 3), "\n\n")

# Plot stays the same (not saved to txt; only console output is)
ggplot(mai_compare_complete, aes(x = mai_diff)) +
  geom_histogram(bins = 40) +
  labs(
    title = "ΔMAI per hexagon (constrained − unconstrained)",
    x     = "ΔMAI",
    y     = "Count"
  ) +
  theme_minimal()


mai_compare_geom <- hex_polygons %>%
  left_join(mai_compare, by = "id") %>%
  mutate(
    constrained   = tidyr::replace_na(constrained, 0),
    unconstrained = tidyr::replace_na(unconstrained, 0),
    mai_diff      = dplyr::coalesce(mai_diff, constrained - unconstrained)
  )

st_write(
  mai_compare_geom,
  file.path(data_path, "mai_comparison_constr_unconstr.gpkg"),
  layer = "MAI_COMPARISON",
  delete_layer = TRUE
)

# =============================================================
# 9. H2 – ΔMAI spatial clustering + systematic differences 
# =============================================================

cat("-------------------------------------------------------------\n")
cat("H2 – ΔMAI SPATIAL CLUSTERING + SYSTEMATIC DIFFERENCES\n")
cat("-------------------------------------------------------------\n\n")

# -----------------------------
# 9.1) Build ΔMAI table (if needed)
# -----------------------------
if (!exists("mai_compare")) {
  mai_compare <- both_scored %>%
    select(id, scenario, MAI) %>%
    tidyr::pivot_wider(names_from = scenario, values_from = MAI) %>%
    mutate(mai_diff = constrained - unconstrained)
}

# -----------------------------
# 9.2) Join ΔMAI to geometry
# -----------------------------
delta_sf <- hex_polygons %>%
  left_join(
    mai_compare %>%
      transmute(
        id,
        MAI_constr   = constrained,
        MAI_unconstr = unconstrained,
        delta_mai    = mai_diff
      ),
    by = "id"
  )

# -----------------------------
# 9.3) Project + make valid
# -----------------------------
delta_sf_proj <- st_transform(delta_sf, crs_metric) %>%
  st_make_valid()

# -----------------------------
# 9.4) COMPLETE-CASE subset for inference
# -----------------------------
delta_cc <- delta_sf_proj %>%
  filter(is.finite(delta_mai))

cat("H2 – n hexes (total):         ", nrow(delta_sf_proj), "\n")
cat("H2 – n hexes (complete cases):", nrow(delta_cc), "\n\n")

if (nrow(delta_cc) < 10) {
  sink()
  stop("Too few complete cases for H2 spatial statistics. Check why delta_mai is mostly NA.")
}

# -----------------------------
# 9.5) Spatial weights on COMPLETE cases only
# -----------------------------
nb_cc <- poly2nb(delta_cc, queen = TRUE, snap = 10)
lw_cc <- nb2listw(nb_cc, style = "W", zero.policy = TRUE)

# -----------------------------
# 9.6) Global Moran’s I on ΔMAI
# -----------------------------
moran_delta <- moran.test(delta_cc$delta_mai, lw_cc, zero.policy = TRUE)
cat("H2 – Global Moran's I for ΔMAI (complete cases):\n")
print(moran_delta)
cat("\n")

# -----------------------------
# 9.7) Local Getis-Ord Gi* on z-standardised ΔMAI
# -----------------------------
x  <- delta_cc$delta_mai
sx <- sd(x, na.rm = TRUE)
mx <- mean(x, na.rm = TRUE)

delta_cc$delta_z <- if (is.finite(sx) && sx > 0) (x - mx) / sx else rep(0, length(x))
delta_cc$gi_star_z <- as.numeric(localG(delta_cc$delta_z, lw_cc, zero.policy = TRUE))
delta_cc$gi_p <- 2 * pnorm(-abs(delta_cc$gi_star_z))

delta_cc <- delta_cc %>%
  mutate(
    gi_class = case_when(
      is.na(gi_star_z) ~ "no data",
      gi_p < 0.01 & gi_star_z > 0 ~ "hotspot (p<0.01)",
      gi_p < 0.05 & gi_star_z > 0 ~ "hotspot (p<0.05)",
      gi_p < 0.01 & gi_star_z < 0 ~ "coldspot (p<0.01)",
      gi_p < 0.05 & gi_star_z < 0 ~ "coldspot (p<0.05)",
      TRUE ~ "not significant"
    )
  )

cat("H2 – Getis-Ord Gi* (NO FDR)\n")
cat("Gi* z-score summary:\n")
print(summary(delta_cc$gi_star_z))
cat("\nGi raw p-value summary:\n")
print(summary(delta_cc$gi_p))
cat("\nGi class counts:\n")
print(table(delta_cc$gi_class, useNA = "ifany"))
cat("\n\n")

# -----------------------------
# 9.8) ΔMAI vs distance-to-center (Spearman)
# -----------------------------
center <- st_centroid(st_union(st_geometry(delta_cc)))
pts <- st_point_on_surface(delta_cc)
dist_m <- st_distance(pts, center)
delta_cc$dist_km <- as.numeric(units::drop_units(dist_m)) / 1000

df_dist <- delta_cc %>%
  st_drop_geometry() %>%
  filter(is.finite(delta_mai), is.finite(dist_km))

spearman_dist <- cor.test(df_dist$delta_mai, df_dist$dist_km,
                          method = "spearman", exact = FALSE)

cat("H2 – Spearman: ΔMAI vs distance to center (km):\n")
print(spearman_dist)
cat("\n")

rho <- unname(spearman_dist$estimate)

ggplot(df_dist, aes(x = dist_km, y = delta_mai)) +
  geom_bin2d(bins = 60) +
  stat_summary_bin(fun = median, bins = 40, geom = "line", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "ΔMAI vs distance to center",
    subtitle = paste0("Spearman ρ = ", round(rho, 3)),
    x = "Distance to center (km)",
    y = "ΔMAI (constrained − unconstrained)"
  ) +
  theme_minimal()

# -----------------------------
# 9.9) Optional: ΔMAI vs PT class (ov_score)
# -----------------------------
if (exists("pt_scores")) {
  delta_pt <- delta_cc %>%
    left_join(pt_scores, by = "id") %>%
    mutate(ov_score = as.numeric(ov_score))
  
  df_pt <- delta_pt %>%
    st_drop_geometry() %>%
    filter(is.finite(delta_mai), is.finite(ov_score))
  
  if (nrow(df_pt) > 10) {
    spearman_pt <- cor.test(df_pt$delta_mai, df_pt$ov_score,
                            method = "spearman", exact = FALSE)
    cat("H2 – Spearman: ΔMAI vs PT class (ov_score):\n")
    print(spearman_pt)
    cat("\n")
    
    kruskal_pt <- kruskal.test(delta_mai ~ as.factor(ov_score), data = df_pt)
    cat("H2 – Kruskal-Wallis: ΔMAI differs by PT score class:\n")
    print(kruskal_pt)
    cat("\n")
    
    ggplot(df_pt, aes(x = as.factor(ov_score), y = delta_mai)) +
      geom_boxplot() +
      labs(
        title = "ΔMAI by public transport quality class (ov_score)",
        x = "ov_score (1=best … 7=worst)",
        y = "ΔMAI (constrained − unconstrained)"
      ) +
      theme_minimal()
  } else {
    cat("H2 – PT tests skipped: too few complete cases after join with pt_scores.\n\n")
  }
}

# -----------------------------
# 9.10) Join results back to FULL grid + export
# -----------------------------
delta_sf_proj_out <- delta_sf_proj %>%
  left_join(
    delta_cc %>%
      st_drop_geometry() %>%
      select(id, delta_z, gi_star_z, gi_p, gi_class, dist_km),
    by = "id"
  ) %>%
  mutate(delta_mai_map = replace_na(delta_mai, 0))

if (exists("pt_scores")) {
  delta_sf_proj_out <- delta_sf_proj_out %>%
    left_join(pt_scores, by = "id")
}

out_file <- file.path(data_path, "delta_mai_hotspots.gpkg")
st_write(st_transform(delta_sf_proj_out, crs_route),
         out_file, layer = "DELTA_MAI_STATS",
         delete_layer = TRUE)

cat("H2 export written to: ", out_file, "\n\n")

cat("=============================================================\n")
cat("END OF MAI STATISTICS OUTPUT\n")
cat("Saved to: ", stats_txt, "\n")
cat("=============================================================\n")

sink()  # IMPORTANT: stop writing to file

message("Saved statistics output to: ", stats_txt)






