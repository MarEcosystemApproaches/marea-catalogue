
# this code is adapted from the azmpmap() function from the azmpdata R package
# here: https://github.com/casaultb/azmpdata/blob/master/R/azmpmap.R

# here, we use this function to extract and save area shapes for relevant AZMP regional summarizations

##### NOTE: edit this to add eco_indicators polygons !!


# libraries ---------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(azmpdata)
library(marea)



# get all marea AZMP regions ----------------------------------------------
marea_azmp_datasets <- marea_metadata() %>% select(Dataset)
print(marea_azmp_datasets)

get(data(list = "azmp_bottom_temperature", package = 'marea'))
marea_sbt_areas <- unique(azmp_bottom_temperature@data$region)
rm(azmp_bottom_temperature)

get(data(list = "azmp_salinity", package = "marea"))
marea_salinity_areas <- unique(azmp_salinity@data$region)
rm(azmp_salinity)

get(data(list = "azmp_satellite_temperature", package = 'marea'))
marea_sat_temp_areas <- unique(azmp_satellite_temperature@data$region)
rm(azmp_satellite_temperature)

get(data(list = "azmp_stratification", package = 'marea'))
marea_strat_areas <- unique(azmp_stratification@data$region)
rm(azmp_stratification)

get(data(list = "azmp_surface_temperature", package = 'marea'))
marea_surface_temp_areas <- unique(azmp_surface_temperature@data$region)
rm(azmp_surface_temperature)

get(data(list = "eco_indicators", package = "marea"))
marea_eco_indicators_areas <- unique(eco_indicators@data$region)
rm(eco_indicators)

all_marea_areas <- unique(c(
  marea_sbt_areas,
  marea_salinity_areas,
  marea_sat_temp_areas,
  marea_strat_areas,
  marea_surface_temp_areas,
  marea_eco_indicators_areas
))
# there are 18 shapefiles that we'll need in total.

all_marea_areas_df <- data.frame(
  area = all_marea_areas
)
all_marea_areas_df$azmp_bottom_temperature <- all_marea_areas %in% marea_sbt_areas
all_marea_areas_df$azmp_salinity <- all_marea_areas %in% marea_salinity_areas
all_marea_areas_df$azmp_satellite_temperature <- all_marea_areas %in% marea_sat_temp_areas
all_marea_areas_df$azmp_stratification <- all_marea_areas %in% marea_strat_areas
all_marea_areas_df$azmp_surface_temperature <- all_marea_areas %in% marea_surface_temp_areas
all_marea_areas_df$eco_indicators <- all_marea_areas %in% marea_eco_indicators_areas

# notes:
# marea azmp salinity, stratification, and surface temperature are only in one polygon
# whereas azmp sbt is in 12 regions, and satelite temperature is in 6 regions
marea_sat_temp_areas %in% marea_sbt_areas
# and only 5 of the marea satellite temperatures are the same as sbt area temperatures
rm(marea_salinity_areas, marea_sat_temp_areas, marea_sbt_areas, marea_strat_areas, marea_surface_temp_areas)


# get spatial data from DFO ftp link --------------------------------------
quiet = T
urlCsvExtract <- function(url=NULL){
  tryCatch(
    {
      utils::read.csv(text = RCurl::getURL(url, connecttimeout = 10))
    },
    error=function(cond){
      return(-1)
    }
  )
}
regtab_att = urlCsvExtract('ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/lookup/polygons/polygons_attributes.csv')
regtab_geo = urlCsvExtract('ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/lookup/polygons/polygons_geometry.csv')

#drop rows that show up in attributes that are complete duplicates (ignoring 'record' field)
regtab_att <- regtab_att[!duplicated(regtab_att[!names(regtab_att) %in% c("record")]), ]

AZMP <- merge(regtab_att, regtab_geo, all.x = T, by="record")



# convert to sf objects ---------------------------------------------------
AZMP_sf = azmpdata::df2sf(input = AZMP, PID = "record", type.field = "type", ORD = "vertice", point.IDs = c("station"), poly.IDs = c("nafo","area", "section"), quiet=T)
AZMP_sf$area <- AZMP_sf$vertice <- NULL
rm(list=c("AZMP", "regtab_att", "regtab_geo"))




# get an inventory of all data that has been collected, and associate it with the various locations
areaInventory <- area_indexer(doParameters = F)
fldNames <- c("area", "section", "station")
areaInventory[fldNames][is.na(areaInventory[fldNames])] <- -99
areaInventory[fldNames] <- lapply(areaInventory[fldNames], toupper)
all_datafiles_Names <- toupper(c(unique(areaInventory$area),unique(areaInventory$section),unique(areaInventory$station)))
areaInventory_core <- unique(areaInventory[,c("area","section", "station")])
areaInventory_data <- unique(areaInventory[,c("area","section", "station", "datafile")])
areaInventory_data <- stats::aggregate(list(datafiles = areaInventory_data$datafile),
                                       list(area = areaInventory_data$area,
                                            section = areaInventory_data$section,
                                            station = areaInventory_data$station), paste, collapse="</dd><dd>")
areaInventory_data$datafiles <- paste0("<dd>",areaInventory_data$datafiles,"</dd>")


areaInventory_year <- unique(areaInventory[,c("area","section", "station", "year")])
rm(areaInventory)
areaInventory_year <- as.data.frame(as.list(stats::aggregate(
  x = list(year = areaInventory_year$year),
  by = list(
    area = areaInventory_year$area,
    section = areaInventory_year$section,
    station = areaInventory_year$station
  ),
  FUN = function(x) c(MIN = min(x),
                      MAX = max(x))
)))

areaInventory_year$years <- paste0("<dd>",areaInventory_year$year.MIN," to ",areaInventory_year$year.MAX,"</dd>")
areaInventory_year$year.MIN <- areaInventory_year$year.MAX <- NULL

#compare areas between ftp and data files
all_AZMP_sf_Names<- toupper(unique(AZMP_sf$sname))
if(!quiet){
  diff1 <- setdiff(all_datafiles_Names, all_AZMP_sf_Names)
  diff1 <- diff1[!is.na(diff1)]
  diff2 <- setdiff(all_AZMP_sf_Names, all_datafiles_Names)
  diff2 <- diff2[!is.na(diff2)]
  if (length(diff1)>0)message("\nThese named areas from azmp package data files can't be associated with data in the ftp spatial objects.\n\t", paste(diff1, collapse=", "))
  if (length(diff2)>0)message("\nThese named areas from the ftp spatial objects can't be associated with data in the azmp package data files.\n\t", paste(diff2, collapse=", "))
}

AZMP_sf$mergeName <- toupper(AZMP_sf$sname)

AZMP_sf_stations <- AZMP_sf[AZMP_sf$type == "station",]
AZMP_sf_sections <- AZMP_sf[AZMP_sf$type == "section",]
AZMP_sf_areas <- AZMP_sf[AZMP_sf$type %in% c("area","nafo"),]

AZMP_sf_stations<- merge(AZMP_sf_stations, areaInventory_core[areaInventory_core$station!=-99,c("station", "section","area")], all.x =T, by.x = "mergeName", by.y="station")
colnames(AZMP_sf_stations)[colnames(AZMP_sf_stations)=="mergeName"] <- "station"
AZMP_sf_sections<- merge(AZMP_sf_sections, areaInventory_core[areaInventory_core$section!=-99 & areaInventory_core$station ==-99,c("station", "section","area")], all.x =T, by.x = "mergeName", by.y="section")
colnames(AZMP_sf_sections)[colnames(AZMP_sf_sections)=="mergeName"] <- "section"
AZMP_sf_areas<- merge(AZMP_sf_areas, areaInventory_core[areaInventory_core$area!=-99,c("station", "section","area")], all.x =T, by.x = "mergeName", by.y="area")
colnames(AZMP_sf_areas)[colnames(AZMP_sf_areas)=="mergeName"] <- "area"

AZMP_sf<- rbind.data.frame(AZMP_sf_stations, AZMP_sf_sections, AZMP_sf_areas)
AZMP_sf %>% st_drop_geometry() %>% distinct(type)
AZMP_sf %>% st_drop_geometry() %>% distinct(sname)
AZMP_sf %>%
  filter(type != "station",
         type != "section") %>%
  ggplot() +
  geom_sf(alpha = .3) +
  facet_wrap(~type) +
  geom_sf_label(aes(label = sname))

# save version before ESS and WSS get removed
AZMP_sf_save <- AZMP_sf


AZMP_sf <- mergeAZMP_sfAZMP_sf <- merge(AZMP_sf, areaInventory_data)
AZMP_sf <- merge(AZMP_sf, areaInventory_year)
#isolate the various NAFO-related data
AZMP_NAFO <- AZMP_sf[AZMP_sf$type=="nafo",]
NAFO_gen <- c("4V", "4W", "4X")
NAFO_det <- c("4VN", "4VS", "4W", "4X")
otherNAFO <-AZMP_NAFO[!AZMP_NAFO$sname %in% c(NAFO_gen,NAFO_det),]

#isolate  the data that is of type = area, and group it
AZMP_areas <- unique(AZMP_sf[AZMP_sf$type == "area",])
GE <- c("E Georges Bank", "Georges Basin","Lurcher Shoal", "Misaine Bank","Emerald Basin","Misaine Bank")
SS <- c("CSS", "ESS","WSS", "GB","Cabot Strait")
SSB <-c("scotian_shelf_box")
SSG <-c("scotian_shelf_grid")
RS <- c("CS_remote_sensing","LS_remote_sensing")
otherAreas <-AZMP_areas[!AZMP_areas$sname %in% c(GE,SS,SSB,SSG,RS),]

# now we have two sets of shapefiles
# 1. AZMP NAFO zones
AZMP_NAFO %>% st_drop_geometry() %>% filter(stringr::str_detect(datafiles,"Derived_Annual"))
AZMP_NAFO %>% st_drop_geometry() %>% distinct(area)
# AZMP areas
AZMP_areas %>% st_drop_geometry() %>% distinct(sname)

all_marea_areas_df$geometry <- NULL

all_marea_areas_df_split <- all_marea_areas_df %>% split(f = all_marea_areas_df$area)


for(i in 1:length(all_marea_areas_df_split)){

  print(all_marea_areas_df_split[[i]]$area)

  if(all_marea_areas_df_split[[i]]$area %in% AZMP_areas$sname){

    all_marea_areas_df_split[[i]] <- all_marea_areas_df_split[[i]] %>%
      left_join(AZMP_areas %>% select(-area) %>% rename(area = sname) %>%
                  select(area, geometry)) %>%
      mutate(area_type = "AZMP_area")

    print("added from AZMP_areas")

  } else if(all_marea_areas_df_split[[i]]$area %in% AZMP_NAFO$sname){


    all_marea_areas_df_split[[i]] <- all_marea_areas_df_split[[i]] %>%
      left_join(AZMP_NAFO %>% select(-area) %>% rename(area = sname) %>%
                  select(area, geometry)) %>%
      mutate(area_type = "AZMP_NAFO")

    print("added from AZMP_NAFO")
  } else {
    print("area not matched")
  }

}

all_marea_areas_df_split %>% bind_rows() %>% pull(area)

all_marea_areas_df_split %>% bind_rows() %>%
  count(sf::st_is_empty(geometry))
# we successfully matched 11 of 18 here. Manually match the others
all_marea_areas_df_split %>% bind_rows() %>%
  mutate(empty = sf::st_is_empty(geometry)) %>%
  filter(empty == T)
empty_areas <- all_marea_areas_df_split %>% bind_rows() %>%
  mutate(empty = sf::st_is_empty(geometry)) %>%
  filter(empty == T) %>%
  pull(area)


# 1. 4Vn didn't match because it's 4VN in the AZMP_NAFO dataset
all_marea_areas_df_split[[empty_areas[1]]] <- all_marea_areas_df_split[[empty_areas[1]]] %>%
  left_join(AZMP_NAFO %>% select(sname, geometry) %>%
              rename(area = sname) %>%
              mutate(area = recode(area,
                                   "4VN" = "4Vn",
                                   "4VS" = "4Vs"))) %>%
  mutate(area_type = "AZMP_NAFO")
# 2. 4Vs didn't match because it's 4VS in the AZMP_NAFO dataset
all_marea_areas_df_split[[empty_areas[2]]] <- all_marea_areas_df_split[[empty_areas[2]]] %>%
  left_join(AZMP_NAFO %>% select(sname, geometry) %>%
              rename(area = sname) %>%
              mutate(area = recode(area,
                                   "4VN" = "4Vn",
                                   "4VS" = "4Vs"))) %>%
  mutate(area_type = "AZMP_NAFO")

# 5. 4XeGoM+BoF didn't match because that area doesn't exist.
all_marea_areas_df_split[[empty_areas[3]]]
# it appears that 4X is separated into "4XeGoM+BoF", "4XeGoMBoF", and "4XSS"
# I'm going to assume that those indicate as follows:
# 4XSS = subset of 4X inside scotian shelf region (source page 25 here: https://publications.gc.ca/collections/collection_2024/mpo-dfo/Fs97-18-380-eng.pdf)
# 4XeGoMBoF = subset of 4X inside GoM/BoF regions ??
# 4XeGoM+BoF = subset of 4X inside GoM + all of BoF ??
# try to create each manually here:



# first, 4XSS -------------------------------------------------------------
NAFO_4X <- AZMP_NAFO %>% filter(area == "4X")
SS_area <- AZMP_areas %>% filter(sname %in% c("scotian_shelf_grid","scotian_shelf_box"))

SS_area %>%
ggplot() +
  geom_sf( fill = "lightgreen", alpha = .5) +
  facet_wrap(~area) +
  geom_sf(data = NAFO_4X %>% select(-area), fill = "purple", alpha = .5)

# 4XSS seems to be the overlap between scotian_shelf_grid and 4x.
NAFO_4XSS <- st_intersection(
  NAFO_4X,
  SS_area %>% filter(sname == "scotian_shelf_grid") %>% sf::st_geometry()
) %>%
  mutate(area = "4XSS") %>%
  select(area, geometry)

all_marea_areas_df_split[[empty_areas[5]]]  <- all_marea_areas_df_split[[empty_areas[5]]] %>%
  left_join(NAFO_4XSS) %>%
  mutate(area_type = "AZMP_NAFO_subdivision")



# second, 4XGoMBoF --------------------------------------------------------
# we're going to define 4XGoMBoF as the opposite of above:
# this is the 4X shape that is NOT inside SS grid
ggplot() +
  geom_sf(data = NAFO_4X, fill = "purple", alpha = .5) +
  geom_sf(data = SS_area %>% filter(sname == "scotian_shelf_grid"),
          fill = "lightgreen", alpha = .5) +
  geom_sf(data = SS_area %>% filter(sname == "scotian_shelf_grid")%>% st_bbox() %>% st_as_sfc(),
          color = "black", fill = "transparent", linetype = "dashed")

NAFO_4XGoMBoF <- st_difference(
  NAFO_4X,
  SS_area %>% filter(sname == "scotian_shelf_grid")%>% st_bbox() %>% st_as_sfc()
) %>%
  mutate(area = "4XeGoMBoF") %>%
  select(area, geometry)

all_marea_areas_df_split[[empty_areas[4]]]  <- all_marea_areas_df_split[[empty_areas[4]]] %>%
  left_join(NAFO_4XGoMBoF) %>%
  mutate(area_type = "AZMP_NAFO_subdivision")




# third, 4XGoM+BoF --------------------------------------------------------
# we're going to define 4XGoM+BoF as 4xGoM + other shapes inside BoF,
# which basically just includes Lurcher Shoal
test_bind <- rbind(
  NAFO_4XGoMBoF ,
  AZMP_areas %>% filter(sname == "Lurcher Shoal") %>% select(area, geometry)
)

# because there's a gap between these two shapes,
# we'll add a buffer to close the gap, then merge,
# then remove the buffer
NAFO_4XGoMandBoF <- test_bind %>%
  st_buffer(.15) %>%
  st_union() %>%
  st_buffer(-.15) %>%
  st_as_sf() %>%
  mutate(area = "4XeGoM+BoF") %>%
  rename(geometry = x) %>%
  st_as_sf()

# check how well it overlaps original
ggplot() +
  geom_sf(data = NAFO_4XGoMandBoF, fill = "lightgreen") +
  geom_sf(data = test_bind,
          fill = "transparent", color = "darkred", linewidth = 1)
# ok, good enough

all_marea_areas_df_split[[empty_areas[3]]]  <- all_marea_areas_df_split[[empty_areas[3]]] %>%
  left_join(NAFO_4XGoMandBoF) %>%
  mutate(area_type = "AZMP_NAFO_subdivision")


# ESS
# NOTE: I think this shapefile is incorrect!
all_marea_areas_df_split[[empty_areas[6]]] <- all_marea_areas_df_split[[empty_areas[6]]] %>%
  left_join(AZMP_sf_save %>% select(area,geometry)) %>%
  mutate(area_type = "DFO_zone")

# WSS
# NOTE: I think this shapefile is incorrect!
all_marea_areas_df_split[[empty_areas[7]]] <- all_marea_areas_df_split[[empty_areas[7]]] %>%
  left_join(AZMP_sf_save %>% select(area,geometry)) %>%
  mutate(area_type = "DFO_zone")


# recheck missing values --------------------------------------------------
all_marea_areas_df_split %>% bind_rows() %>%
  mutate(empty = sf::st_is_empty(geometry)) %>%
  filter(empty == T)
# none empty


# bind
all_marea_areas_df_bind <- all_marea_areas_df_split %>%
  bind_rows() %>%
  st_as_sf()

all_marea_areas_df_bind %>% glimpse()
all_marea_areas_df_bind %>% pull(area)

all_marea_areas_df_bind %>%
  ggplot() +
  geom_sf(alpha = .1,
          fill = "purple") +
  facet_wrap(~area_type)

# set WGS84 crs -- ASSUMED
st_crs(all_marea_areas_df_bind) <- "EPSG:4326"

all_marea_areas_df_bind %>%
  ggplot() +
  geom_sf(alpha = .1,
          fill = "purple") +
  facet_wrap(~area_type)

# save this as rds object
saveRDS(all_marea_areas_df_bind,
         file = "data/derived_data/azmp_area.rds")
rm(list = ls())

test <- readRDS("data/derived_data/azmp_area.rds")
# -------------------------------------------------------------------------


# azmpdata code for plotting; not needed here-------------------------------


# -------------------------------------------------------------------------



m <- leaflet::leaflet()
m <- leaflet::addTiles(m)

m <- leaflet::addWMSTiles(map = m,
                          group = "Bathymetry",
                          baseUrl = "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}.png",
                          layers = "1", options = leaflet::WMSTileOptions(format = "image/png", transparent = T))
m <- leaflet::addPolygons(map = m, data = AZMP_NAFO[AZMP_NAFO$sname %in% NAFO_gen,],
                          group= "NAFO (gen)", label =~lname,
                          color = 'grey',weight = 1.5,
                          fillColor = sf::sf.colors(12, categorical = TRUE),
                          labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                          popup = ~paste0("NAFO (gen):", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # "<br>Parameter(s) collected here:<br>",parameters
                          ))

m <- leaflet::addPolygons(map = m, data = AZMP_NAFO[AZMP_NAFO$sname %in% NAFO_det,],
                          group= "NAFO (det)", label =~lname,
                          color = 'grey',weight = 1.5,
                          fillColor = sf::sf.colors(12, categorical = TRUE),
                          labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                          popup = ~paste0("NAFO (det):", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # , "<br>Parameter(s) collected here:<br>",parameters
                          ))
m <- leaflet::addPolygons(map = m, data = AZMP_areas[AZMP_areas$sname %in% GE,],
                          group= "General Areas", label =~lname,
                          labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                          popup = ~paste0("General Areas:", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # , "<br>Parameter(s) collected here:<br>",parameters
                          ))
m <- leaflet::addPolygons(map = m, data = AZMP_areas[AZMP_areas$sname %in% SS,],
                          group= "SS Areas", label =~lname,
                          labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                          popup = ~paste0("SS Areas:", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # ,"<br>Parameter(s) collected here:<br>",parameters
                          ))
m <- leaflet::addPolygons(map = m, data = AZMP_areas[AZMP_areas$sname %in% SSB,],
                          group= "SS Box", label =~lname,
                          labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                          popup = ~paste0("SS Box:", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # ,"<br>Parameter(s) collected here:<br>",parameters
                          ))
m <- leaflet::addPolygons(map = m, data = AZMP_areas[AZMP_areas$sname %in% SSG,],
                          group= "SS Grid", label =~lname,
                          labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                          popup = ~paste0("SS Grid:", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # , "<br>Parameter(s) collected here:<br>",parameters
                          ))
m <- leaflet::addPolygons(map = m, data = AZMP_areas[AZMP_areas$sname %in% RS,],
                          group= "Remote Sensing", label =~lname,
                          labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                          popup = ~paste0("Remote Sensing:", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # , "<br>Parameter(s) collected here:<br>",parameters
                          ))
if (nrow(otherAreas)>0){
  m <- leaflet::addPolygons(map = m, data = otherAreas,
                            group= "OtherAreas", label =~lname,
                            color="red", weight = 1.5,
                            popup = ~paste0("Section:", lname," (",sname,")<br>",
                                            "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                            "<br>Year(s) Data Collected here:<br>", years
                                            # ,"<br>Parameter(s) collected here:<br>",parameters
                            ))
  overlayGroups <- c(overlayGroups,"OtherAreas")
}
if (nrow(otherNAFO)>0){
  m <- leaflet::addPolygons(map = m, data = otherNAFO,
                            group= "OtherNAFO", label =~lname,
                            color="red", weight = 1.5,
                            popup = ~paste0("Section:", lname," (",sname,")<br>",
                                            "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                            "<br>Year(s) Data Collected here:<br>", years
                                            # , "<br>Parameter(s) collected here:<br>",parameters
                            ))
  overlayGroups <- c(overlayGroups,"OtherNAFO")
}
m <- leaflet::addPolygons(map = m, data = AZMP_sf[AZMP_sf$type=="section",],
                          group= "Sections", label = ~sname,
                          color="red", weight = 1.5,
                          popup = ~paste0("Section:", lname," (",sname,")<br>",
                                          "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                          "<br>Year(s) Data Collected here:<br>", years
                                          # , "<br>Parameter(s) collected here:<br>",parameters
                          ))
m <- leaflet::addCircleMarkers(map = m, data = AZMP_sf[AZMP_sf$type=="station",],
                               group= "Stations", label =~sname,
                               color = "red",weight = 2, radius = 4,
                               labelOptions = leaflet::labelOptions(noHide = T, textOnly = TRUE),
                               options = leaflet::markerOptions(zIndexOffset = 99),
                               popup = ~paste0("Station:", lname," (",sname,")<br><br>Depth:", ifelse(is.numeric(depth),paste0(depth," m"), NA),"<br>",
                                               "<br>Relevant AZMP datafile(s):<br>", datafiles,
                                               "<br>Year(s) Data Collected here:<br>", years
                                               # ,"<br>Parameter(s) collected here:<br>",parameters
                               ))
