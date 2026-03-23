# make new shapefiles for NAFO regions
# from here:
# https://github.com/PopulationEcologyDivision/Mar.data/blob/master/data/NAFOSubunits_sf.rda

library(sf)
library(dplyr)
library(ggplot2)
source("global.R")
sf_use_s2(F)

# manually add a line to global_cols2 for 4XSS
global_cols2 <- rbind(global_cols2,
                      c("4XSS","WSS","Western Scotian Shelf", "orange"))

old_areas <- readRDS("data/derived_data/azmp_area.rds")
old_areas %>% distinct(area)
old_areas %>%
  filter(area %in% c("4X","4XSS")) %>%
  ggplot() +
  geom_sf() +
  facet_wrap(~area)

#load("data/NAFOSubunits_sf_new.rda")
load("data/NAFOSubunitsLnd_sf.rda")
land <- NAFOSubunitsLnd_sf %>% filter(NAFO == "<LAND>")
not_land <- NAFOSubunitsLnd_sf %>% filter(NAFO != "<LAND>")
NAFOSubunitsLnd_sf %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = land, fill = "purple", alpha = .5,
          linewidth = 0) +
  coord_sf(xlim = c(-70, -50),
           ylim = c(40, 55))

not_land %>%
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-70, -50),
           ylim = c(40, 55)) +
  theme(panel.background = element_rect(fill = alpha("purple",.5)),
        panel.grid = element_blank())

NAFOSubunitsLnd_sf %>% ggplot() + geom_sf()
st_is_valid(NAFOSubunitsLnd_sf) %>% table() # some are not valid.
NAFOSubunitsLnd_sf <- st_make_valid(NAFOSubunitsLnd_sf)
# get 4W ---------
nafo_4w <- NAFOSubunitsLnd_sf %>%
  filter(NAFO_1 == "4W") %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(region = "4W") %>%
  relocate(region) %>%
  rename(geometry = x)
nafo_4w %>% ggplot() + geom_sf()

# get 4X ---------
NAFOSubunitsLnd_sf %>% distinct(NAFO_1)
nafo_4x <- NAFOSubunitsLnd_sf %>%
  filter(NAFO_1 == "4X") %>%
  # note that internal polygons didn't merge correctly
  # (internal lines were left over) so we'll buffer, then un-buffer later.
  st_buffer(dist = 0.0001) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(region = "4X") %>%
  relocate(region) %>%
  rename(geometry = x) %>%
  st_buffer(dist = -0.0001)
nafo_4x %>% ggplot() + geom_sf()

# get 4Vn -------
NAFOSubunitsLnd_sf %>% distinct(NAFO_2)
nafo_4Vn <- NAFOSubunitsLnd_sf %>%
  filter(NAFO_2 == "4VN") %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(region = "4Vn") %>%
  relocate(region) %>%
  rename(geometry = x)
nafo_4Vn %>% ggplot() + geom_sf()

# get 4Vs -------
NAFOSubunitsLnd_sf %>% distinct(NAFO_2)
nafo_4Vs <- NAFOSubunitsLnd_sf %>%
  filter(NAFO_2 == "4VS") %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(region = "4Vs") %>%
  relocate(region) %>%
  rename(geometry = x)
nafo_4Vs %>% ggplot() + geom_sf()

# get 4XSS ---------
# NOTE: I don't know what this one is, but it's in the AZMP data
# I'll just make 4X instead. ASK!
NAFOSubunitsLnd_sf %>% distinct(NAFO) %>% arrange(NAFO)
nafo_4xss <- NAFOSubunitsLnd_sf %>%
  filter(NAFO_1 == "4X") %>%
  # note that internal polygons didn't merge correctly
  # (internal lines were left over) so we'll buffer, then un-buffer later.
  st_buffer(dist = 0.0001) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(region = "4XSS") %>%
  relocate(region) %>%
  rename(geometry = x) %>%
  st_buffer(dist = -0.0001)
nafo_4xss %>% ggplot() + geom_sf()


nafo_merge <- rbind(nafo_4Vn, nafo_4Vs, nafo_4w, nafo_4x, nafo_4xss) %>%
  left_join(global_cols2)

nafo_merge %>%
  ggplot() +
  geom_sf(aes(color = region),
          fill = "transparent",
          linewidth = 1)


nafo_merge %>%
  st_buffer(-.03) %>%
  ggplot() +
  geom_sf(aes(color = region),
          fill = "transparent",
          linewidth = 1)

# simplify
nafo_merge_simplify <- nafo_merge %>% rmapshaper::ms_simplify(keep_shapes = T)

nafo_merge_simplify %>%
  st_buffer(-.03) %>%
  ggplot() +
  geom_sf(aes(color = region),
          fill = "transparent",
          linewidth = 1)

i <- 5
nrow(st_coordinates(nafo_merge[i,]))
nrow(st_coordinates(nafo_merge_simplify[i,]))

nafo_merge %>% st_area()
nafo_merge_simplify %>% st_area()

nafo_merge %>% st_is_valid()
nafo_merge_simplify %>% st_is_valid()

nafo_merge_simplify <- nafo_merge_simplify %>% st_make_valid()

nafo_merge_simplify %>%
  st_buffer(-.03) %>%
  ggplot() +
  geom_sf(aes(color = region),
          fill = "transparent",
          linewidth = 1)

saveRDS(nafo_merge_simplify,
        file = "data/derived_data/nafo_regions_correct.rds")

test <- readRDS( "data/derived_data/nafo_regions_correct.rds")

test %>%
  ggplot() +
  geom_sf()

nrow(st_coordinates(test[5,]))
nrow(st_coordinates(old_areas[,]))

sf <- readRDS("data/derived_data/azmp_area.rds") %>%
  filter(area %in% c("4Vn","4Vs","4W","4X"))  %>%
  rename(region = area) %>%
  left_join(global_cols2)

