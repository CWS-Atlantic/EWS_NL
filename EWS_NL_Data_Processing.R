################################
##   EWS NL DATA PROCESSING   ##
################################

require(readr)
require(dplyr)
require(sf)
require(stringr)

#############################################
##  Read in current year's PC Mapper data  ##
#############################################

ews.2025 <- read.csv("V:/Sackville/Wildlife/Gamebird Management/Surveys/Breeding Surveys/EWS/EWS NL/EWS NL DATA 2025/Transcribed Data/EWS_NL_2025_Observations.csv",
                     encoding = "latin1")

#deal with the June 3 dates:

june3 <- ews.2025[ews.2025$day == 3,]

june3$lat <- as.numeric(substr(june3$Latitude, 3, 9))

june3$lon <- as.numeric(substr(june3$Longitude, 3, 8))


june3.sf <- st_as_sf(june3, 
                     coords = c("lon", "lat"),
                     crs = 32620,
                     agr = "constant",
                     remove = FALSE)

june3.sf <- st_transform(june3.sf, 4326)

june3.coords <- st_coordinates(june3.sf)

june3$lat <- june3.coords[,2]
june3$lon <- june3.coords[,1]


##deal with lat-lon. Change from Deg Min Sec to Decimal Degrees

#kick out June 3 data because theyre fixed above

ews.2025 <- ews.2025[!ews.2025$day == 3,]

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

######################################
##  Work with the whole data frame  ##
######################################

#bind back in the june3 data:
ews.2025 <- rbind(june3, ews.2025)

#order based on date
ews.2025 <- ews.2025[order(ews.2025$month, ews.2025$day),]


#kick out the 9999 obs

ews.2025 <- ews.2025[!ews.2025$spp %in% "9999",]


#check species names
sort(unique(ews.2025$spp))

#remove whitespace
ews.2025$spp <- gsub(" ", "", ews.2025$spp)

#edit some species names

ews.2025$spp <- gsub("PTAR", "UNPT", ews.2025$spp) #Unknown Ptarmigan

ews.2025$spp <- gsub("TERN", "UNTE", ews.2025$spp) #Unknown Tern

#create total column
ews.2025$tot <- ews.2025$mal + ews.2025$fem + ews.2025$unk

ews.zeros <- ews.2025[ews.2025$tot == 0,]

range(ews.2025$tot)

#kick out other bad detections
ews.2025 <- ews.2025[!ews.2025$detect == 9999,]

#########################
##  Format for export  ##
#########################

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

##local file
#ews.sf <- read_csv(file = "C:/Users/englishm/Documents/EWS/Shiny App/EWS_NL_24-06-2024.csv", col_types = cols(observation =  col_character()))

##shared drive
ews.sf <- read_csv(file = "V:/Sackville/Wildlife/Gamebird Management/Surveys/Breeding Surveys/EWS/EWS NL/Master Data/EWS_NL_Observations_1990-2025.csv", col_types = cols(observation =  col_character()))


ews.sf <-  ews.sf[order(ews.sf$Year),] 


#remove 9999 species
ews.sf <- filter(ews.sf,
                 Species_E != "9999")

ews.sf$tot <- ews.sf$Males + ews.sf$Females + ews.sf$Unknown


#clean up some species names
ews.sf$Species_E <- gsub("wipt", "WIPT", ews.sf$Species_E) #Willow Ptarmigan

ews.sf$Species_E <- gsub("PTAR", "UNPT", ews.sf$Species_E) #Unknown Ptarmigan

ews.sf$Species_E <- gsub("MRAT", "MUSK", ews.sf$Species_E) #Muskrat

ews.sf$Species_E <- gsub("PHAL", "UNPH", ews.sf$Species_E) #Unknown Phalarope

ews.sf$Species_E <- gsub("OSPY", "OSPR", ews.sf$Species_E) #Osprey

ews.sf$Species_E <- gsub("OPSR", "OSPR", ews.sf$Species_E) #Osprey

ews.sf$Species_E <- gsub("lodg", "LODG", ews.sf$Species_E) #Beaver Lodge

ews.sf$Species_E <- gsub("UTER", "UNTE", ews.sf$Species_E) #Unknown Tern

ews.sf$Species_E <- gsub("TERN", "UNTE", ews.sf$Species_E) #Unknown Tern

ews.sf$Species_E <- gsub("UIDI", "UNDI", ews.sf$Species_E) #Unknown Diver

ews.sf$Species_E <- gsub("NORA", "CORA", ews.sf$Species_E) #Common Raven

ews.sf$Species_E <- gsub("UNSC", "USCO", ews.sf$Species_E) #Unknown Scoter

ews.sf$Species_E <- gsub("HADU", "HARD", ews.sf$Species_E) #Harlequin Duck

ews.sf$Species_E <- gsub("WISN", "COSN", ews.sf$Species_E) #Common Snipe

ews.sf$Species_E <- gsub("NOBI", "AMBI", ews.sf$Species_E) #American Bittern

ews.sf$Species_E <- gsub("BLBI", "BLBD", ews.sf$Species_E) #Blackbird

ews.sf$Species_E <- gsub("BLBR", "BLBD", ews.sf$Species_E) #Blackbird

ews.sf$Species_E <- gsub("GOHA", "NOGO", ews.sf$Species_E) #Northern Goshawk

ews.sf$Species_E <- gsub("GTBH", "GBHE", ews.sf$Species_E) #Great Blue Heron


sort(unique(ews.sf$Species_E))

####################################
##  Complete species-plot matrix  ##
####################################

################# BROKEN AT THE MOMENT #######################

##complete the cases of all species-plot-year matrix ##THIS IS WRONG BECAUSE IT DOESNT TAKE IN THE ROTATIONAL DESIGN
#
# ews.sf <- ews.sf %>% 
#   complete(species, plot)
#
# ews.sf <- ews.sf %>% 
#   rowwise() %>% 
#   mutate(tot = sum(male, female, unknown, na.rm = TRUE))

#remove any extra points outside of range - this shouldn't kick any out if the data are CLEAN
ews.sf <-  ews.sf[ews.sf$LatObs > 0,]
ews.sf <-  ews.sf[ews.sf$LatObs < 70,]

# #write.csv
# write.csv(ews.sf, "EWS_NL_2024_AllSpecies_noTIP.csv")


###############################
##  Read in conditions file  ##
###############################

ews.nl.cond <- read.csv("C:/Users/englishm/Documents/EWS/NL/Processed Data/EWS_NL_Conditions_1990-2024.csv", strip.white = T)










##############################
##   create species lists   ##
##############################


# #not necessary at this stage
# 
# #all species
# all.sp <- unique(ews.sf$species) 
# all.sp <- all.sp[order(all.sp)]
# 
# 
# waterfowl <- c("ABDU", 
#                "AGWT",
#                "AMWI",
#                "BAGO",
#                "BLSC",
#                "BUFF",
#                "BWTE",
#                "CAGO", 
#                "COGO", 
#                "COLO", 
#                "COME",
#                "GRSC",
#                "HARD",
#                "HOME",
#                "LESC",
#                "LTDU",
#                "MALL",
#                "NOPI",
#                "NSHO",
#                "RBME", 
#                "RNDU",
#                "RTLO",
#                "SUSC",
#                "USCA",
#                "WODU",
#                "WWSC")
# 
# 
# #waterbirds
# waterbirds <- c("COLO",
#                 "DCCO",
#                 "GBBG",
#                 "HERG",
#                 "RBGU",
#                 "RTLO",
#                 "TERN",
#                 "UNPH")
# 
# shorebirds <- c("COSN",
#                 "SOSA",
#                 "SPSA",
#                 "UNYE")
# 
# raptors <- c("BAEA",
#              "GHOW",
#              "NOHA",
#              "OSPR",
#              "RTHA")
# 
# mammals <- c("BLBE",
#              "BEAV",
#              "CARI",
#              "COYT",
#              "MINK",
#              "MUSK",
#              "MOOS",
#              "PORC",
#              "RFOX",
#              "RIOT",
#              "TIWO")


##########################
##   TIP Calculations   ##
##########################

#read in tip
tip.code.df = read.csv(file = "C:/Users/englishm/Documents/EWS/Phenology/Indicateur_couples_newABDU.csv", sep=",", header=T)
group.code.df = read.csv(file = "C:/Users/englishm/Documents/EWS/Phenology/GroupsID.csv",  stringsAsFactors= FALSE)


#subset df based on TIP-only species
tip.obs <- ews.sf[ews.sf$Species_E %in% group.code.df$species,]

x <- tip.obs[is.na(tip.obs$Year),] # 0 results, good

non.tip.obs <- ews.sf[!ews.sf$Species_E %in% group.code.df$species,]

x <- non.tip.obs[is.na(non.tip.obs$Year),] # 152 results, why???, now 19, now 2, now 0

#tip.obs <- ews.sf

tip.obs$Species_E <- as.factor(tip.obs$Species_E)


tip.obs$obscode <- paste(tip.obs$Males, tip.obs$Females, tip.obs$Unknown, tip.obs$tot, sep="-")

tip.obs$species.id <- sapply(1:nrow(tip.obs), function(k){as.character(group.code.df$group[group.code.df$species==tip.obs$Species_E[k]])})

tip.obs$TIP <- sapply(1:nrow(tip.obs), function(k){tip.code.df[match(tip.obs$obscode[k],tip.code.df$obscode),tip.obs$species.id[k]]})

################################################################################
#                         make columns for groups
################################################################################

tip.obs$TIP[is.na(tip.obs$TIP)] <- 0
tip.obs$groups <- ifelse (tip.obs$TIP == 0, tip.obs$tot, 0)

non.tip.obs$TIP <- NA

#filter out some columns we dont need for the APP:
non.tip.obs <- select(non.tip.obs,
                  #index,
                  #survey,
                  Year,
                  Month,
                  Day,
                  Plot,
                  Species_E,
                  LatObs,
                  LongObs,
                  Males,
                  Females,
                  Unknown,
                  tot,
                  TIP,
                  #groups,
                  #mxSpptemp,
                  BreedType,
                  BreedCnt,
                  Detection,
                  Comment)


#filter out some columns we dont need for the APP:
tip.obs <- select(tip.obs,
                  #index,
                  #survey,
                  Year,
                  Month,
                  Day,
                  Plot,
                  Species_E,
                  LatObs,
                  LongObs,
                  Males,
                  Females,
                  Unknown,
                  tot,
                  TIP,
                  #groups,
                  #mxSpptemp,
                  BreedType,
                  BreedCnt,
                  Detection,
                  Comment)


#bind the dataframes back together

ews.sf.app <- rbind(tip.obs, non.tip.obs)

#organize DF

ews.sf.app <-  ews.sf.app[order(ews.sf.app$Year, ews.sf.app$Plot),] 


#rename specific column:
names(ews.sf.app)[names(tip.obs) == 'tot'] <- 'total'


#write out TIP data:
write.csv(ews.sf.app, "EWS_NL_AppData_2025-06-11.csv")

# convert ews to a sf
ews.sf.app <- st_as_sf(ews.sf.app, 
                       coords = c("LongObs", "LatObs"), 
                       crs = 4326, 
                       agr = "constant", 
                       remove = FALSE)

ews.sf.app <-  ews.sf.app[ews.sf.app$LatObs > 0,]
ews.sf.app <-  ews.sf.app[ews.sf.app$LatObs < 70,]


st_write(ews.sf.app, layer = "EWS_Obs", dsn = "EWS_NL_AppData_2025-06-11.gdb", driver = "OpenFileGDB", append = F)
