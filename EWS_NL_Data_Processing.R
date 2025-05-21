################################
##   EWS NL DATA PROCESSING   ##
################################

require(readr)
require(dplyr)

#############################################
##  Read in current year's PC Mapper data  ##
#############################################

ews.2025 <- read.csv("V:/Sackville/Wildlife/Gamebird Management/Surveys/Breeding Surveys/EWS/EWS NL/EWS NL DATA 2025/Transcribed Data/EWS_NL_2025_Observations.csv",
                     encoding = "latin1")

##deal with lat-lon. Change from Deg Min Sec to Decimal Degrees

#deal with latitude
df <- as.data.frame(substr(ews.2025$Latitude, 3, 4))

names(df) <- "deg"

df$deg <- as.numeric(df$deg)

df$min.sec <- substr(ews.2025$Latitude, 6, 100)

df$min.sec <- gsub("\"", "", df$min.sec)

df$min <- as.numeric(sub("'.*", "", df$min.sec))/60

df$sec <- as.numeric(str_split(df$min.sec, "'", simplify = TRUE)[,2])/3600

df$lat <- df$deg + df$min + df$sec

#bind to obs file
ews.2025$lat <- df$lat


#deal with longitude
df <- as.data.frame(substr(ews.2025$Longitude, 3, 4))

names(df) <- "deg"

df$deg <- as.numeric(df$deg)

df$min.sec <- substr(ews.2025$Longitude, 6, 100)

df$min.sec <- gsub("\"", "", df$min.sec)

df$min <- as.numeric(sub("'.*", "", df$min.sec))/60

df$sec <- as.numeric(str_split(df$min.sec, "'", simplify = TRUE)[,2])/3600

df$lon <- (df$deg + df$min + df$sec) * -1

#bind to obs file
ews.2025$lon <- df$lon

#kick out the 9999 obs

ews.2025 <- ews.2025[!ews.2025$spp %in% "9999",]

ews.2025 <-select(ews.2025,
                  Feature.ID,
                  plot,
                  year,
                  month,
                  day,
                  spp,
                  mal,
                  fem,
                  unk,
                  mxSppFlk,
                  breedType,
                  breedCnt,
                  detect,
                  Date,
                  lat,
                  lon)

write.csv(ews.2025, 
          "EWS_NL_2025_Observations_Clean.csv",
          row.names = F)


#########################
##   Leaflet Mapping   ##
#########################

require(leaflet)
require(leafpop)
require(sf)

#setwd("C:/Users/englishm/Documents/EWS/Shiny App/app/")

#load in plots
plots <- st_read("C:/Users/englishm/Documents/EWS/NL/Shiny App/app/data/EWS_NL_Plots.gdb")  #update directory when running locally, see data folder

plots <- st_transform(plots, 4326)



leaflet(options = leafletOptions (minZoom = 2, maxZoom = 16)) %>%  #you can adjust your zoom range as necessary
  # addProviderTiles("Esri.OceanBasemap",group="OceanBasemap")  %>%  #we select these three basemaps but you can select the ones you like.
  #addProviderTiles("Esri.WorldImagery",group="WorldImagery")  %>%
  addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
  setView(-62.654, 49.373, zoom = 6) %>%                           #this sets where the map is first centered, so adjust as necessary.
  
  # addLayersControl(baseGroups = c("OceanBasemap","WorldImagery","TopoMap"),
  #                  overlayGroups = "Dataset",
  #                  position = "topleft",
  #                  options = layersControlOptions(collapsed = FALSE)) %>%
  # 
  
  addPolygons(data = plots,
              color = "purple",
              fillOpacity = 0.15,
              opacity = 1,
              weight = 1,
              popup = popupTable(plots, zcol = c("plot", "plot_name", "utm"), row.numbers = F, feature.id = F)) %>%
  
  
  addCircleMarkers(data = ews.2025,
                   #radius = ~log(coei$Total),
                   lng = ews.2025$lon,
                   lat = ews.2025$lat,
                   fillOpacity = 0.5,
                   # fillColor = ~pal(Year), 
                   color = "green",
                   weight = 1,
                   #group = as.character(mydata.sf.m$Year),
                   popup = popupTable(ews.2025, zcol = c("plot", "spp", "mal", "fem", "unk", "Date"), row.numbers = F, feature.id = F))


###################################################
##  Read in EWS NL observation data master file  ##
###################################################

ews.sf <- read_csv(file = "C:/Users/englishm/Documents/EWS/Shiny App/EWS_NL_24-06-2024.csv", col_types = cols(observation =  col_character()))

ews.sf <-  ews.sf[order(ews.sf$year),] 


#remove 9999 species
ews.sf <- filter(ews.sf,
                 species != "9999")

ews.sf$tot <- ews.sf$male + ews.sf$female + ews.sf$unknown


#clean up some species names
ews.sf$species <- gsub("wipt", "WIPT", ews.sf$species) #Willow Ptarmigan

ews.sf$species <- gsub("PTAR", "UNPT", ews.sf$species) #Unknown Ptarmigan

ews.sf$species <- gsub("MRAT", "MUSK", ews.sf$species) #Muskrat

ews.sf$species <- gsub("PHAL", "UNPH", ews.sf$species) #Unknown Phalarope

ews.sf$species <- gsub("OSPY", "OSPR", ews.sf$species) #Osprey

ews.sf$species <- gsub("OPSR", "OSPR", ews.sf$species) #Osprey

ews.sf$species <- gsub("lodg", "LODG", ews.sf$species) #Beaver Lodge

ews.sf$species <- gsub("UTER", "UNTE", ews.sf$species) #Unknown Tern

ews.sf$species <- gsub("TERN", "UNTE", ews.sf$species) #Unknown Tern

ews.sf$species <- gsub("UIDI", "UNDI", ews.sf$species) #Unknown Diver

ews.sf$species <- gsub("NORA", "CORA", ews.sf$species) #Common Raven

ews.sf$species <- gsub("UNSC", "USCO", ews.sf$species) #Unknown Scoter

ews.sf$species <- gsub("HADU", "HARD", ews.sf$species) #Harlequin Duck

ews.sf$species <- gsub("WISN", "COSN", ews.sf$species) #Common Snipe

ews.sf$species <- gsub("NOBI", "AMBI", ews.sf$species) #American Bittern

ews.sf$species <- gsub("BLBI", "BLBD", ews.sf$species) #Blackbird

ews.sf$species <- gsub("BLBR", "BLBD", ews.sf$species) #Blackbird

ews.sf$species <- gsub("GOHA", "NOGO", ews.sf$species) #Northern Goshawk

unique(ews.sf$species)

#complete the cases of all species-plot-year matrix ##THIS IS WRONG BECAUSE IT DOESNT TAKE IN THE ROTATIONAL DESIGN

ews.sf <- ews.sf %>% 
  complete(species, plot)

ews.sf <- ews.sf %>% 
  rowwise() %>% 
  mutate(tot = sum(male, female, unknown, na.rm = TRUE))


ews.sf <-  ews.sf[ews.sf$lat > 0,]
ews.sf <-  ews.sf[ews.sf$lat < 70,]

#write.csv
write.csv(ews.sf, "EWS_NL_2024_AllSpecies_noTIP.csv")


###############################
##  Read in conditions file  ##
###############################

ews.nl.cond <- read.csv("C:/Users/englishm/Documents/EWS/NL/Processed Data/EWS_NL_Conditions_1990-2024.csv", strip.white = T)










##############################
##   create species lists   ##
##############################


#all species
all.sp <- unique(ews.sf$species) 
all.sp <- all.sp[order(all.sp)]


waterfowl <- c("ABDU", 
               "AGWT",
               "AMWI",
               "BAGO",
               "BLSC",
               "BUFF",
               "BWTE",
               "CAGO", 
               "COGO", 
               "COLO", 
               "COME",
               "GRSC",
               "HARD",
               "HOME",
               "LESC",
               "LTDU",
               "MALL",
               "NOPI",
               "NSHO",
               "RBME", 
               "RNDU",
               "RTLO",
               "SUSC",
               "USCA",
               "WODU",
               "WWSC")


#waterbirds
waterbirds <- c("COLO",
                "DCCO",
                "GBBG",
                "HERG",
                "RBGU",
                "RTLO",
                "TERN",
                "UNPH")

shorebirds <- c("COSN",
                "SOSA",
                "SPSA",
                "UNYE")

raptors <- c("BAEA",
             "GHOW",
             "NOHA",
             "OSPR",
             "RTHA")

mammals <- c("BLBE",
             "BEAV",
             "CARI",
             "COYT",
             "MINK",
             "MUSK",
             "MOOS",
             "PORC",
             "RFOX",
             "RIOT",
             "WOLF")


##########################
##   TIP Calculations   ##
##########################

#read in tip
tip.code.df = read.csv(file = "C:/Users/englishm/Documents/EWS/Phenology/Indicateur_couples_newABDU.csv", sep=",", header=T)
group.code.df = read.csv(file = "C:/Users/englishm/Documents/EWS/Phenology/GroupsID.csv",  stringsAsFactors= FALSE)


#remove ghost levels
tip.obs <- ews.sf[ews.sf$species %in% group.code.df$species,]

#tip.obs <- ews.sf

tip.obs$species <- as.factor(tip.obs$species)


tip.obs$obscode <- paste(tip.obs$male, tip.obs$female, tip.obs$unknown, tip.obs$tot, sep="-")

tip.obs$species.id <- sapply(1:nrow(tip.obs), function(k){as.character(group.code.df$group[group.code.df$species==tip.obs$species[k]])})

tip.obs$TIP <- sapply(1:nrow(tip.obs), function(k){tip.code.df[match(tip.obs$obscode[k],tip.code.df$obscode),tip.obs$species.id[k]]})

################################################################################
#                         make columns for groups
################################################################################

tip.obs$TIP[is.na(tip.obs$TIP)] <- 0
tip.obs$groups <- ifelse (tip.obs$TIP == 0, tip.obs$tot, 0)


#filter out some columns we dont need for the APP:
tip.obs <- select(tip.obs,
                  index,
                  survey,
                  year,
                  month,
                  day,
                  plot,
                  species,
                  male,
                  female,
                  unknown,
                  tot,
                  TIP,
                  groups,
                  mxSpptemp,
                  breed_type,
                  breed_cnt,
                  det_obs,
                  comment,
                  lat,
                  lon)


#write out TIP data:
write.csv(tip.obs, "EWS_NL_AppData_2024-06-24.csv")

# convert ews to a sf
ews.sf <- st_as_sf(tip.obs, 
                   coords = c("lon", "lat"), 
                   crs = 4326, 
                   agr = "constant", 
                   remove = FALSE)

ews.sf <-  ews.sf[ews.sf$lat > 0,]
ews.sf <-  ews.sf[ews.sf$lat < 70,]


st_write(ews.sf, layer = "EWS_Obs", dsn = "EWS_NL_AppData_2024-06-24.gdb", driver = "OpenFileGDB", append = F)
