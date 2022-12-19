
if (!require("remotes")) install.packages("remotes")
remotes::install_github("gaelso/oNFI")
oNFI::shiny_optimize_NFI()

#devtools::load_all()
library(oNFI)
library(sf)
library(tidyverse)
library(terra)

sf_aoi_wgs84 <- sf_aoi <- st_read("data/gadm41_IDN_1.json")

sf_aoi$NAME_1


## Develop subnational island based boundaries
sf_sumatra <- sf_aoi %>%
  filter(NAME_1 %in% c("Aceh", "SumateraUtara", "Riau", "SumateraBarat", "Jambi", "SumateraSelatan", "Lampung")) %>%
  summarise()

# sf::st_is_valid(sf_sumatra)
#
# sf_sumatra <- sf::st_make_valid(sf_sumatra)
#
# ggplot() +
#   geom_sf(data = sf_sumatra, fill = NA)
#
# sf::st_write(sf_sumatra, "data/gadm41_IDN_sumatra.geojson")

# Bali Nusa Tenggara
# Jawa
# Kalimantan
# Maluku
# Papua
# Sulawesi
# Sumatera




sf_aoi <- sf_sumatra

# sf_aoi <- st_read("data/TimorLeste.geoJSON")
# sf_aoi_wgs84 <- sf_aoi %>% st_transform(crs = 4326)

ggplot() +
  geom_sf(data = sf_aoi)

rs_san <- get_santoro(
  path_data   = "data",
  sf_aoi      = sf_aoi,
  progress_id = NULL,
  session     = NULL,
  tile_name   = NULL,
  url = "globbiomass.org/wp-content/uploads/GB_Maps/"
)

df_san <- make_df(rs = rs_san)


gr2 <- ggplot() +
  geom_tile(data = df_san, aes(x = .data$x, y = .data$y, fill = .data$agb_santoro)) +
  scale_fill_viridis_c(direction = -1) +
  geom_sf(data = sf_aoi, fill = NA, col = "darkred", size = 1) +
  theme_bw() +
  theme(legend.key.height = unit(2, "cm")) +
  add_ggspatial(font = "LoraIt") +
  labs(x = "", y = "", fill = "AGB (ton/ha)", title = "Santoro et al. 2018 aboveground biomass") +
  coord_sf(crs = 4326)
gr2

cv_san <- get_CV_AGB(rs = rs_san, agb_min = 2) %>%
  dplyr::mutate(
    source = "Santoro et al. 2018"
  )

## ---

rs_avi <- get_avitabile(
  path_data = "data",
  progress_id = NULL,
  session = NULL,
  sf_aoi = sf_aoi,
  url = "http://lucid.wur.nl/storage/downloads/high-carbon-ecosystems/Avitabile_AGB_Map.zip"
)

df_avi <- make_df(rs = rs_avi)

cv_avi <- get_CV_AGB(rs = rs_avi, agb_min = 2)


area_aoi <- round(as.numeric(sf::st_area(sf_aoi)) / 1000^2)


## Detailed run
path_data  <-  "data"
santoro_tiles    <- get_santoro_tiles(sf_aoi = sf_aoi_wgs84)
santoro_filelist <- list.files(file.path(path_data, "Santoro_agb"), pattern = "_agb.tif")
santoro_files    <- santoro_filelist[match(paste0(santoro_tiles, ".tif"), santoro_filelist)]

## Load files
tictoc::tic()
rs_list <- purrr::map(seq_along(santoro_files), function(x){

  rs <- terra::rast(file.path(paste0(path_data, "/Santoro_agb"), santoro_files[x]))

  if (!is.null(sf_aoi_wgs84)) {

    check <- terra::intersect(terra::ext(rs), terra::vect(sf_aoi_wgs84))

    if (!is.null(check)) {

      rs_tmp1 <- terra::crop(rs, terra::vect(sf_aoi_wgs84))
      rs_out <- terra::mask(rs_tmp1, terra::vect(sf_aoi_wgs84))

    } else {

      rs_out <- NULL

    }

  } else {

    rs_out <- rs

  }

  rs_out

})

tictoc::toc()

length(rs_list)

for (i in seq_along(rs_list)) {

  plot(rs_list[[i]])

}

rs_coll <- terra::sprc(rs_list)
rs_out <- terra::merge(rs_coll)

names(rs_out) <- "agb_santoro"

plot(rs_out)

df_out <- make_df(rs = rs_out)

ggplot() +
  geom_tile(data = df_out, aes(x = x, y = y , fill = agb_santoro)) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw() +
  coord_sf(crs = 4326)


#
# ## Merging elements
# if (length(rs_list) == 1) {
#   rs_out <- rs_list[[1]]
# } else if (length(rs_list) == 2) {
#   rs_out <- terra::merge(rs_list[[1]], rs_list[[2]])
# } else if (length(rs_list) == 3) {
#   rs_tmp <- terra::merge(rs_list[[1]], rs_list[[2]])
#   rs_out <- terra::merge(rs_tmp, rs_list[[3]])
#   rs_tmp <- NULL
# } else if (length(rs_list) == 4) {
#   rs_tmp1 <- terra::merge(rs_list[[1]], rs_list[[2]])
#   rs_tmp2 <- terra::merge(rs_list[[3]], rs_list[[4]])
#   rs_out  <- terra::merge(rs_tmp1, rs_tmp2)
#   rs_tmp1 <- NULL
#   rs_tmp2 <- NULL
# }

rm(rs_list, rs_coll)

names(rs_out) <- "agb_santoro"

plot(rs_out)

## Check
terra::ncell(rs_out)

rs_proj <- terra::project(rs_out, "ESRI:54017", method = "near")
rs_res  <- terra::res(rs_proj)[1]^2 / 100^2

# #tt <- terra::cellSize(rs_out)
#
# san_ext <- terra::ext(rs_out)
#
# san_ext[2] <- floor(san_ext[2] / 2)
#
# sub1 <- terra::crop(rs_out, ext(san_ext[1], floor((san_ext[1] + san_ext[2])/2), san_ext[3], san_ext[4]))
# sub2 <- terra::crop(rs_out, ext(floor((san_ext[1] + san_ext[2])/2), san_ext[2], san_ext[3], san_ext[4]))
#
# df1 <- sub1 %>% terra::as.data.frame(xy = TRUE, na.rm = T) %>%
#   dplyr::as_tibble()
#
# df2 <- sub2 %>% terra::as.data.frame(xy = TRUE, na.rm = T) %>%
#   dplyr::as_tibble()

rs_out_clamp <- terra::clamp(rs_out, lower=5, value=FALSE)

terra::global(rs_out, fun = "mean", na.rm = TRUE)
terra::global(rs_out, fun = "sd", na.rm = TRUE)

terra::global(rs_out_clamp, fun = "mean", na.rm = TRUE)
terra::global(rs_out_clamp, fun = "sd", na.rm = TRUE)


fct <- round(0.01 / terra::res(rs_out_clamp)[1], 3)

tt <- terra::aggregate(rs_out, fact = fct)
tt

plot(tt)

terra::global(tt, fun = "mean", na.rm = TRUE)
terra::global(tt, fun = "sd", na.rm = TRUE)

df1 <- tt %>% terra::as.data.frame(xy = TRUE, na.rm = T) %>%
  dplyr::as_tibble()

ggplot() +
  geom_tile(data = df1, aes(x = x, y = y, fill = agb_santoro)) +
  geom_sf(data = sf_aoi, fill = NA) +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(crs = 4326) +
  theme_bw()

##
rs_avi <- get_avitabile(
  path_data   = "data",
  sf_aoi      = sf_aoi,
  progress_id = NULL,
  session     = NULL
  )

plot(rs_avi)


## Test cat

floor((139 - 20) / 40) * 40 + 20
floor((141 - 20) / 40) * 40 + 20


aoi_bbox <- sf_aoi %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_bbox()

x_start <- as.numeric(floor((aoi_bbox$xmin - 20) / 40) * 40 + 20)
x_end   <- as.numeric(floor((aoi_bbox$xmax - 20) / 40) * 40 + 20)

y_start <- as.numeric(ceiling(aoi_bbox$ymax / 40) * 40)
y_end   <- as.numeric(ceiling(aoi_bbox$ymin / 40) * 40)

nb_hz <- (abs(x_end - x_start) / 40 + 1)
nb_vt <- (abs(y_end - y_start) / 40 + 1)

tic()
tile_names <- matrix("", ncol = nb_vt, nrow = nb_hz)

for (i in 1:nb_hz) {
  for (j in 1:nb_vt) {

    x1 <- x_start + 40 * (i - 1)
    y1 <- y_start + 40 * (j - 2)

    x_chr <- if_else(x1 == 0, "00", if_else(x1 < 100, paste0("0", as.character(abs(x1))), as.character(abs(x1))))
    y_chr <- if_else(y1 == 0, "00", as.character(abs(y1)))
    x_dir <- if_else(x1 < 0, "W", "E")
    y_dir <- if_else(y1 < 0, "S", "N")

    tile_names[i, j] <- paste0(y_dir, y_chr, x_dir, x_chr, "_agb")

  }
}
toc()

res_san <- calc_santoro(rs = rs_out, agb_min = 5)

ggplot() +
  geom_tile(data = res_san$df, aes(x = x, y = y, fill = agb_santoro)) +
  geom_sf(data = sf_aoi, fill = NA) +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(crs = 4326) +
  theme_bw()


