# =========================================================
# Standalone hex shot chart inspired by toddwschneider/ballr
# No Shiny | Using hoopR | Robust version
# =========================================================

# packages <- c(
#   "ggplot2",
#   "dplyr",
#   "tibble",
#   "hexbin",
#   "viridis",
#   "scales",
#   "hoopR"
# )
# 
# to_install <- setdiff(packages, rownames(installed.packages()))
# if (length(to_install) > 0) install.packages(to_install)

library(ggplot2)
library(dplyr)
library(tibble)
library(hexbin)
library(viridis)
library(scales)
library(hoopR)

# -----------------------------
# Helpers
# -----------------------------
percent_formatter <- function(x) scales::percent(x, accuracy = 1)
points_formatter  <- function(x) scales::comma(x, accuracy = 0.01)

round_any_base <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# -----------------------------
# Court theme
# -----------------------------
court_themes <- list(
  light = list(
    # court = "#fffcf2",
    court = "white",
    lines = "#999999",
    text  = "#222222",
    made  = "#00bfc4",
    missed = "#f8766d",
    hex_border_size = 0.3,
    hex_border_color = "#cccccc"
  ),
  dark = list(
    court = "#000004",
    lines = "#999999",
    text  = "#f0f0f0",
    made  = "#00bfc4",
    missed = "#f8766d",
    hex_border_size = 0,
    hex_border_color = "#000000"
  )
)

# -----------------------------
# Court geometry
# -----------------------------
circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
  angles <- seq(0, 2 * pi, length.out = npoints)
  tibble(
    x = center[1] + radius * cos(angles),
    y = center[2] + radius * sin(angles)
  )
}

width <- 50
height <- 94 / 2
key_height <- 19
inner_key_width <- 12
outer_key_width <- 16
backboard_width <- 6
backboard_offset <- 4
neck_length <- 0.5
hoop_radius <- 0.75
hoop_center_y <- backboard_offset + neck_length + hoop_radius
three_point_radius <- 23.75
three_point_side_radius <- 22
three_point_side_height <- 14

# -----------------------------
# Court plot
# -----------------------------
plot_court <- function(court_theme = court_themes$dark, use_short_three = FALSE) {
  tp_radius <- if (use_short_three) 22 else three_point_radius
  tp_side_height <- if (use_short_three) 0 else three_point_side_height
  
  court_points <- tibble(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points <- bind_rows(
    court_points,
    tibble(
      x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
      y = c(0, key_height, key_height, 0),
      desc = "outer_key"
    ),
    tibble(
      x = c(-backboard_width / 2, backboard_width / 2),
      y = c(backboard_offset, backboard_offset),
      desc = "backboard"
    ),
    tibble(
      x = c(0, 0),
      y = c(backboard_offset, backboard_offset + neck_length),
      desc = "neck"
    )
  )
  
  foul_circle <- circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top <- foul_circle %>%
    filter(y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom <- foul_circle %>%
    filter(y < key_height) %>%
    mutate(
      angle = atan2(y - key_height, x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop <- circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted <- circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle <- circle_points(center = c(0, hoop_center_y), radius = tp_radius) %>%
    filter(y >= tp_side_height, y >= hoop_center_y)
  
  three_point_line <- tibble(
    x = c(
      three_point_side_radius,
      three_point_side_radius,
      three_point_circle$x,
      -three_point_side_radius,
      -three_point_side_radius
    ),
    y = c(
      0,
      tp_side_height,
      three_point_circle$y,
      tp_side_height,
      0
    ),
    desc = "three_point_line"
  )
  
  court_points <- bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines,
      linewidth = 0.5
    ) +
    # coord_fixed(xlim = c(-25, 25), ylim = c(0, 35)) +
    coord_cartesian(xlim = c(-25, 25), ylim = c(0, 35)) +
    theme_minimal(base_size = 12) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.position = "bottom",
      legend.key = element_blank()
    )
}

# -----------------------------
# Hexbin helpers
# -----------------------------
hex_bounds <- function(x, binwidth) {
  x <- safe_numeric(x)
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    stop("No valid coordinate values available for hexbin bounds.")
  }
  
  rng <- range(x, na.rm = TRUE)
  
  if (!all(is.finite(rng))) {
    stop("Hexbin bounds could not be computed.")
  }
  
  if (rng[1] == rng[2]) {
    rng <- rng + c(-binwidth / 2, binwidth / 2)
  }
  
  c(
    round_any_base(rng[1], binwidth, floor) - 1e-6,
    round_any_base(rng[2], binwidth, ceiling) + 1e-6
  )
}

calculate_hex_coords <- function(shots, binwidths) {
  shots_clean <- shots %>%
    mutate(
      loc_x = safe_numeric(loc_x),
      loc_y = safe_numeric(loc_y),
      shot_made_numeric = safe_numeric(shot_made_numeric),
      shot_value = safe_numeric(shot_value)
    ) %>%
    filter(
      is.finite(loc_x),
      is.finite(loc_y),
      is.finite(shot_made_numeric),
      is.finite(shot_value),
      !is.na(shot_zone_range),
      !is.na(shot_zone_area)
    )
  
  if (nrow(shots_clean) == 0) {
    stop("No valid shot coordinates available after cleaning.")
  }
  
  xbnds <- hex_bounds(shots_clean$loc_x, binwidths[1])
  ybnds <- hex_bounds(shots_clean$loc_y, binwidths[2])
  
  x_range <- diff(xbnds)
  y_range <- diff(ybnds)
  
  if (!is.finite(x_range) || x_range <= 0) {
    stop("Invalid x bounds for hexbin.")
  }
  
  if (!is.finite(y_range) || y_range <= 0) {
    stop("Invalid y bounds for hexbin.")
  }
  
  xbins <- max(1, round(x_range / binwidths[1]))
  ybins <- max(1, round(y_range / binwidths[2]))
  
  hb <- hexbin::hexbin(
    x = shots_clean$loc_x,
    y = shots_clean$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots_clean <- shots_clean %>%
    mutate(hexbin_id = hb@cID)
  
  hexbin_stats <- shots_clean %>%
    group_by(hexbin_id) %>%
    dplyr::summarise(
      hex_attempts = dplyr::n(),
      hex_pct = mean(shot_made_numeric, na.rm = TRUE),
      hex_points_scored = sum(shot_made_numeric * shot_value, na.rm = TRUE),
      hex_points_per_shot = mean(shot_made_numeric * shot_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  hexbin_ids_to_zones <- shots_clean %>%
    group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    dplyr::summarise(attempts = dplyr::n(), .groups = "drop") %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    slice(1) %>%
    ungroup() %>%
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats <- inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  sx <- hb@xbins / diff(hb@xbnds)
  sy <- (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx <- 1 / (2 * sx)
  dy <- 1 / (2 * sqrt(3) * sy)
  
  origin_coords <- hexbin::hexcoords(dx, dy)
  hex_centers <- hexbin::hcell2xy(hb)
  
  hexbin_coords <- bind_rows(lapply(seq_len(hb@ncells), function(i) {
    tibble(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

calculate_hexbins_from_shots <- function(
    shots,
    league_averages,
    binwidths = c(1, 1),
    min_radius_factor = 0.6,
    fg_diff_limits = c(-0.12, 0.12),
    fg_pct_limits = c(0.2, 0.7),
    pps_limits = c(0.5, 1.5)
) {
  if (nrow(shots) == 0) {
    stop("shots has zero rows.")
  }
  
  if (nrow(league_averages) == 0) {
    stop("league_averages has zero rows.")
  }
  
  zone_stats <- shots %>%
    mutate(
      shot_made_numeric = safe_numeric(shot_made_numeric),
      shot_value = safe_numeric(shot_value)
    ) %>%
    filter(
      !is.na(shot_zone_range),
      !is.na(shot_zone_area),
      is.finite(shot_made_numeric),
      is.finite(shot_value)
    ) %>%
    group_by(shot_zone_range, shot_zone_area) %>%
    dplyr::summarise(
      zone_attempts = dplyr::n(),
      zone_pct = mean(shot_made_numeric, na.rm = TRUE),
      zone_points_scored = sum(shot_made_numeric * shot_value, na.rm = TRUE),
      zone_points_per_shot = mean(shot_made_numeric * shot_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  league_zone_stats <- league_averages %>%
    mutate(
      fga = safe_numeric(fga),
      fgm = safe_numeric(fgm)
    ) %>%
    filter(
      !is.na(shot_zone_range),
      !is.na(shot_zone_area),
      is.finite(fga),
      is.finite(fgm),
      fga > 0
    ) %>%
    group_by(shot_zone_range, shot_zone_area) %>%
    dplyr::summarise(
      league_pct = sum(fgm, na.rm = TRUE) / sum(fga, na.rm = TRUE),
      .groups = "drop"
    )
  
  hex_data <- calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys <- c("shot_zone_area", "shot_zone_range")
  
  hex_data <- hex_data %>%
    inner_join(zone_stats, by = join_keys) %>%
    inner_join(league_zone_stats, by = join_keys)
  
  if (nrow(hex_data) == 0) {
    stop("No hex data remained after joining with zone stats and league averages.")
  }
  
  max_hex_attempts <- max(hex_data$hex_attempts, na.rm = TRUE)
  
  hex_data <- hex_data %>%
    mutate(
      radius_factor = min_radius_factor +
        (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
      adj_x = center_x + radius_factor * (x - center_x),
      adj_y = center_y + radius_factor * (y - center_y),
      bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
      bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
      bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2])
    )
  
  list(
    hex_data = hex_data,
    fg_diff_limits = fg_diff_limits,
    fg_pct_limits = fg_pct_limits,
    pps_limits = pps_limits
  )
}

generate_hex_chart <- function(
    hex_data,
    base_court,
    court_theme = court_themes$dark,
    metric = c("fg_diff", "fg_pct", "pps"),
    alpha_range = c(0.85, 0.98)
) {
  if (length(hex_data) == 0) {
    return(base_court)
  }
  
  metric <- match.arg(metric)
  
  if (metric == "fg_diff") {
    fill_var <- "bounded_fg_diff"
    fill_limit <- hex_data$fg_diff_limits
    fill_label <- "FG% vs. League Avg"
    label_formatter <- percent_formatter
  } else if (metric == "fg_pct") {
    fill_var <- "bounded_fg_pct"
    fill_limit <- hex_data$fg_pct_limits
    fill_label <- "FG%"
    label_formatter <- percent_formatter
  } else {
    fill_var <- "bounded_points_per_shot"
    fill_limit <- hex_data$pps_limits
    fill_label <- "Points Per Shot"
    label_formatter <- points_formatter
  }
  
  base_court +
    geom_polygon(
      data = hex_data$hex_data,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id,
        fill = .data[[fill_var]],
        alpha = hex_attempts
      ),
      linewidth = court_theme$hex_border_size,
      color = court_theme$hex_border_color
    ) +
    scale_fill_viridis_c(
      name = fill_label,
      limits = fill_limit,
      labels = label_formatter,
      guide = guide_colorbar(barwidth = 15)
    ) +
    scale_alpha_continuous(
      guide = "none",
      range = alpha_range,
      trans = "sqrt"
    ) +
    theme(
      legend.text = element_text(size = rel(0.7))
    )
}

# -----------------------------
# Fetch shots with hoopR
# -----------------------------
fetch_shots_by_player_id_and_season <- function(
    player_id,
    season,
    season_type = "Regular Season"
) {
  res <- hoopR::nba_shotchartdetail(
    player_id = as.character(player_id),
    season = season,
    season_type = season_type,
    context_measure = "FGA"
  )
  
  if (!is.list(res)) {
    stop("hoopR::nba_shotchartdetail() did not return a list.")
  }
  
  if (!all(c("Shot_Chart_Detail", "LeagueAverages") %in% names(res))) {
    stop("Expected 'Shot_Chart_Detail' and 'LeagueAverages' in hoopR response.")
  }
  
  shots <- res$Shot_Chart_Detail %>%
    as_tibble() %>%
    rename_with(tolower) %>%
    mutate(
      loc_x = safe_numeric(loc_x),
      loc_y = safe_numeric(loc_y),
      shot_distance = safe_numeric(shot_distance),
      shot_made_flag = safe_numeric(shot_made_flag),
      shot_attempted_flag = safe_numeric(shot_attempted_flag),
      loc_x = -loc_x / 10,
      loc_y = loc_y / 10 + hoop_center_y,
      shot_made_numeric = shot_made_flag,
      shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2)
    ) %>%
    filter(
      is.finite(loc_x),
      is.finite(loc_y),
      !is.na(shot_zone_range),
      !is.na(shot_zone_area)
    )
  
  league_averages <- res$LeagueAverages %>%
    as_tibble() %>%
    rename_with(tolower) %>%
    mutate(
      fga = safe_numeric(fga),
      fgm = safe_numeric(fgm),
      fg_pct = safe_numeric(fg_pct)
    ) %>%
    filter(
      !is.na(shot_zone_range),
      !is.na(shot_zone_area),
      is.finite(fga),
      is.finite(fgm),
      fga > 0
    )
  
  if (nrow(shots) == 0) {
    stop("No valid shots returned after cleaning.")
  }
  
  if (nrow(league_averages) == 0) {
    stop("No valid league averages returned after cleaning.")
  }
  
  list(
    player = shots,
    league_averages = league_averages
  )
}

# -----------------------------
# Example usage
# -----------------------------
# Stephen Curry player_id = 201939
# res <- fetch_shots_by_player_id_and_season(
#   player_id = 201939,
#   season = "2015-16",
#   season_type = "Regular Season"
# )
# 
# 
# # write_csv(shots, "curry-shots.csv")
# 
# # library(readr)
# # write_rds(res, "curry_res.rds")
# # res <- read_rds("curry_res.rds")
# 
# library(dplyr)
# 
# shots <- res$player
# league_avg <- res$league_averages
# 
# hex_res <- calculate_hexbins_from_shots(
#   shots = shots,
#   league_averages = league_avg,
#   binwidths = c(1, 1),
#   min_radius_factor = 0.55
# )
# 
# # write_csv(hex_res$hex_data, "curry-shots-hex.csv")
# 
# library(ggplot2)
# p <- plot_court(court_theme = court_themes$light) |>
#   generate_hex_chart(
#     hex_data = hex_res,
#     court_theme = court_themes$light,
#     metric = "fg_diff"   # "fg_diff", "fg_pct", or "pps"
#   ) +
#   ggtitle("Stephen Curry 2015-16 Shot Chart")
# 
# print(p)
# 
# ggsave("curry_hex_chart.png", p, width = 8, height = 6, dpi = 300)
# 
# 
# # com headshot:
# library(png)
# library(grid)
# 
# headshot_url <- hoopR::nba_playerheadshot(player_id = "201939")
# 
# tmp_png <- tempfile(fileext = ".png")
# download.file(headshot_url, destfile = tmp_png, mode = "wb")
# 
# player_img <- png::readPNG(tmp_png)
# player_grob <- grid::rasterGrob(player_img, interpolate = TRUE)
# 
# p <- plot_court(court_theme = court_themes$light) |>
#   generate_hex_chart(
#     hex_data = hex_res,
#     court_theme = court_themes$light,
#     metric = "fg_diff"
#   ) +
#   ggtitle("Stephen Curry 2015-16 Shot Chart") +
#   annotation_custom(
#     grob = player_grob,
#     xmin = -25, xmax = -18,
#     ymin = 27, ymax = 39
#   )
# 
# print(p)
# ggsave("curry_hex_chart2-v2.png", p, width = 8, height = 6, dpi = 300)
