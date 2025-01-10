
################################################################################################
#	This program runs in Program R.  It creates a matrix of 0s and -1s the represents the        #
#   plot coverage by year then reads the input file NL_EWS.txt and inserts the 0s for each     #   
#   species. It then outputs files for further processing.                                     #
#                                                                                              #     
#   The user must edit a few lines of code to add a new year of data.                          #
#				1:  The last year of the survey must be entered in the field <lastyr>                  #
#				2:  2 lines of code must be added to add the <lastyr> of coverage to the plot          #
#     				cover matrix. I have built in code the builds the plots upto 2015.               #
#                   the code just has to be "uncommented" each year.                           #
#                   e.g. to add 2015  -- uncomment the following 2 lines:                      #
#						#pt15 = rbind(A,D)     	                                                           #
#                       #plotcove[pt10,22]=1                                                   #  
#                                                                                              #
#   Depends:  The following R-packages must be installed:                                      #
#                	- DoBy                                                                       #
#                   - reshape                                                                  # 
#                                                                                              #
#	The following files are required to run the program:                                         #
#		Data	:  ******.txt; 	data to be processed.  Should have the following fields with         #
#								matching cases: Plot, YEAR, Species, MAL, FEM, UNK.  Missing                   #
#								data must be filled with "NA".                                                 # 
#		Species	:  species.dat;	AOU species codes and Grouping codes for Indicated Pair (IP)       #
#								calculations.  See below.                                                      #
# 				:  Plotbuilder_NL.r; Creates the NL plot rotations and adds 0's to the               #
#                               dataset with inserted 0's.                                     #
#                                                                                              #
#       		:  NL_EWS.r; 	This program.                                                        #   
#                                                                                              # 
#   Output files:  EWSIP.txt; Indicated pairs by species with species as variable              # 
#                  EWSIP2.txt; Indicated pairs with species as variables.                      #    
#                  EWSTOT.txt; Totals by species with species as variable                      # 
#                  EWSTOT2.txt; Totals with species as variables.                              #
#                  EWSIPusca.txt; Indicated Pairs for the scaups                               #
#                  EWSIPPuddle.txt; Indicated Pairs for the puddle ducks                       #
#                  EWSIPblml.txt; Indicated Pairs for the Blacks, Mallars and hybrids          #
#                  EWSIPdiver.txt; Indicated Pairs for the divers                              #
#                                                                                              #    
#   Created by:    Scott Gilliland                                                             # 
#				   Ver 1.0 	2011 04  15                                                                #
#                                                                                              #    
################################################################################################

library(dplyr)


# enter last year of survey into "lastyr"
lastyr = 2024

firstyr = 1990
yrs = seq(firstyr,lastyr,by=1) 	# vector of years
  t = length(yrs)  				      # number of years
pts = seq(1,52,by=1)			      # vector of potential plots
  n = length(pts)				        # number of plots


#spc = length(species$SPP)

# Construct a vector or years by plots

plotcove=array(0,dim=c(n,t))


#  This code sets up an index (ptyr) of the plots for each
#  year than inserts a 1 into (plotcove) for plots that were 
#  surveyed. 

# original 25 plots plus old DnD plots selected for NAP geese

pt90 = cbind(1:25)
plotcove[pt90,1]=1

pt91 = pt90
plotcove[pt91,2]=1

pt92 = rbind(pt90,48)
plotcove[pt92,3]=1

pt93 = pt90
plotcove[pt93,4]=1

pt94 = rbind(pt90,45)
plotcove[pt94,5]=1

# reduced plot selection for 95 after program review

pt95 = cbind(1,5,6,9,11,14,16,17,18,19,20,22,23,46,47,48)
plotcove[pt95,6]=1

# rotational groups 

A  = rbind(1,5,6,10,14,16,17,21,24,38)
B  = rbind(7,19,28,31,33,34,36,37)
C  = rbind(3,4,13,15,20,25,26,27,29,30,35)
D  = rbind(2,8,9,11,12,18,22,23,32)

# rotational groups plus old DnD plots selected for NAP Geese
pt96 = rbind(A,B,45,46,47,48)
plotcove[pt96,7]=1

pt97 = rbind(B,C,45,46,47,48)
plotcove[pt97,8]=1

pt98 = rbind(A,D,45,46,47,48)
plotcove[pt98,9]=1

pt99 = rbind(C,D,45,46,47,48)
plotcove[pt99,10]=1

pt00 = rbind(A,B,45,46,47,48)
plotcove[pt00,11]=1

# In 2001, NAP plots added into rotation
A = rbind(A,43,46)
B = rbind(B,40,41,48)
C = rbind(C,42,47)
D = rbind(D,39,44,45)

pt01 = rbind(B,C)
plotcove[pt01,12]=1

pt02 = rbind(C,D)
plotcove[pt02,13]=1

pt03 = rbind(A,D)
plotcove[pt03,14]=1

# In 2004, add south coast plots into rotation

A = rbind(A,49)
B = rbind(B,50)
C = rbind(C,51)
D = rbind(D,52)

pt04 = rbind(A,B)
plotcove[pt04,15]=1

pt05 = rbind(B,C)
plotcove[pt05,16]=1

pt06 = rbind(C,D)
plotcove[pt06,17]=1

pt07 = rbind(A,D)
plotcove[pt07,18]=1

pt08 = rbind(A,B)
plotcove[pt08,19]=1

pt09 = rbind(B,C)
plotcove[pt09,20]=1

pt10 = rbind(C,D)
plotcove[pt10,21]=1

pt11 = rbind(A,D)
plotcove[pt11,22]=1

pt12 = rbind(A,B)
plotcove[pt12,23]=1

pt13 = rbind(B,C)
plotcove[pt13,24]=1

pt14 = rbind(C,D)
plotcove[pt14,25]=1

pt15 = rbind(A,D)
pt15=pt15[-which(pt15==10 | pt15==39)] # dropped plots for 2015
plotcove[pt15,26]=1

pt16 = rbind(A,B)
plotcove[pt16,27]=1

pt17 = rbind(B,C)
pt17=pt17[-which(pt17==48)] # dropped plot because I missed selection.
plotcove[pt17,28]=1

pt18 = rbind(C,D)
plotcove[pt18,29]=1

pt19 = rbind(D,A)
plotcove[pt19,29]=1

pt22 = rbind(A,B)
plotcove[pt22,30]=1

pt23 = rbind(B,C)
plotcove[pt23,31]=1

pt24 = rbind(C,D)
plotcove[pt24,32]=1

pt25 = rbind(D,A)
plotcove[pt25,33]=1

rm(pt90,pt91,pt92,pt93,pt94,pt95,pt96,pt97,pt98,pt99,pt00,pt01,pt02,pt03,pt04,pt05,pt06)
rm(pt07, pt08, pt09, pt10, pt11, pt12, pt13, pt14, pt15, pt16, pt17, pt18, pt19, pt22, pt23)



#########################
##   Leaflet Mapping   ##
#########################

require(leaflet)
require(leafpop)
require(sf)

#load in plots
plots <- st_read(dsn = "C:/users/englishm/Documents/EWS/NL/NL EWS Shapefiles/EWS_NL_Ver18.shp")  #update directory when running locally

plots <- st_transform(plots, 4326)

#subset plots for the given year
plots.24 <- plots[plots$PLOT_NUM %in% pt24,]

plots.25 <- plots[plots$PLOT_NUM %in% pt25,]

#get centroids for the contract
plots.24.cents <- st_centroid(plots.24)

#write.csv(plots.24.cents, "EWS_NL_Plots_2024.csv")

#st_write(plots.24, dsn = "EWS_NL_2024_Plots", driver = "ESRI Shapefile")


## Read in NL wetlands that need to be photographed - from Tyler Kydd for 2024
## Check and see if there are updates for 2025!

#Newfoundland
nf.wetlands <- st_read("C:/users/englishm/Documents/EWS/NL/2024/Newfoundland_Wetlands_EWS2024/Newfoundland_wetlands_Merge.shp")

nf.wetlands <- st_transform(nf.wetlands, 4326)

nf.wetlands$Id <- seq(1:length(nf.wetlands$Id))

#Labrador
lab.wetlands <- st_read("C:/users/englishm/Documents/EWS/NL/2024/Labrador_Wetlands_EWS2024/LabradorPoints.shp")

lab.wetlands <- st_transform(lab.wetlands, 4326)

lab.wetlands$Id <- seq(1:length(lab.wetlands$Id))


## Create leaflet map   


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
  
  addPolygons(data = plots.25,
              color = "purple",
              fillOpacity = 0.15,
              opacity = 1,
              weight = 1,
              popup = popupTable(plots.25, zcol = c("PLOT_NUM", "PLOT", "UTM"), row.numbers = F, feature.id = F)) %>%
  
  
  addCircleMarkers(data = nf.wetlands,
                   #radius = ~log(coei$Total),
                   #lng = wetlands$long_,
                   #lat = wetlands$lat,
                   fillOpacity = 0.5,
                   # fillColor = ~pal(Year), 
                   color = "green",
                   weight = 1,
                   #group = as.character(mydata.sf.m$Year),
                   popup = popupTable(nf.wetlands, zcol = c("Id"), row.numbers = F, feature.id = F)) %>%
  
  addCircleMarkers(data = lab.wetlands,
                   #radius = ~log(coei$Total),
                   #lng = wetlands$long_,
                   #lat = wetlands$lat,
                   fillOpacity = 0.5,
                   # fillColor = ~pal(Year), 
                   color = "orange",
                   weight = 1,
                   #group = as.character(mydata.sf.m$Year),
                   popup = popupTable(lab.wetlands, zcol = c("Id"), row.numbers = F, feature.id = F)) 
  
  
############################################
##   Old TIP processing and summarizing   ##  # DONT USE!
############################################

# ##  Create an output of the plot selection with species for each year in a table
# 
# library(doBy)
# library(reshape)
# 
# Plot=rep(pts,t)
# Year=sort(rep(yrs,n))
# 
# plotcove=plotcove-1
# plotcove[plotcove==-1]=NA
# plotcove.stk=t(array(plotcove,c(1,n*t*spc))) #stack the plots into a column
# plotcove.stk=data.frame(Plot,Year,plotcove.stk[,1])
# names(plotcove.stk)[3] <- "empty"
# # old delete? plotcove.stk=rename(plotcove.stk,c(plotcove.stk...1.="empty"))
# 
# 
# IPTOT.sum=(summaryBy(IP+TOT~YEAR+Plot+Species,data=EWS,FUN=sum,na.rm = TRUE))
# 
# SPPcove=(array(rep((species$SPP),n*t),c(spc,t))) #crate 
# SPP.stk=array(SPPcove,c(n*t*spc,1))
# SPP.stk=data.frame(SPP.stk[,1])
# # SPP.stk=rename(SPP.stk,c(SPP.stk...1.="SPP"))
# # SPP.stk=orderBy(~SPP,data=SPP.stk)
# 
# names(SPP.stk)[1] <- "SPP"
# # old delete SPP.stk=rename(SPP.stk,c(SPP.stk...1.="SPP"))
# SPP.stk=arrange(SPP.stk,SPP)
# # old delete SPP.stk=orderBy(~SPP,data=SPP.stk)
# 
# empty=data.frame(plotcove.stk$Plot,plotcove.stk$Year,plotcove.stk$empty,SPP.stk$SPP)
# IPTOT.sum$link=paste(IPTOT.sum$YEAR,IPTOT.sum$Plot,IPTOT.sum$Species,sep="_")
# empty$link=paste(empty$plotcove.stk.Year, empty$plotcove.stk.Plot,empty$SPP.stk.SPP,sep="_")
# 
# 
# EWSIPTOT=merge(empty,IPTOT.sum,by="link",all.x=TRUE)
# EWSIPTOT=orderBy(~SPP.stk.SPP+plotcove.stk.Year+plotcove.stk.Plot,data=EWSIPTOT)
# 
# j=is.na(EWSIPTOT$IP.sum)
# EWSIPTOT$IP.sum[j]=EWSIPTOT$plotcove.stk.empty[j]
# EWSIPTOT$TOT.sum[j]=EWSIPTOT$plotcove.stk.empty[j]
# # EWSIPTOT=rename(EWSIPTOT,c(plotcove.stk.Year ="Year", plotcove.stk.Plot="plot", SPP.stk.SPP="Spp", IP.sum="IP", TOT.sum="TOT" ))
# names(EWSIPTOT)[3] <- "Year"
# names(EWSIPTOT)[2] <- "plot"
# names(EWSIPTOT)[5] <- "Spp"
# names(EWSIPTOT)[9] <- "IP"
# names(EWSIPTOT)[10] <- "TOT"
# 
# #  oldEWSIPTOT=rename(EWSIPTOT,c(plotcove.stk.Year ="Year", plotcove.stk.Plot="plot", SPP.stk.SPP="Spp", IP.sum="IP", TOT.sum="TOT" ))  # might not work used reshape rename
# 
# EWSIPTOT$region="NF"
# EWSIPTOT$region[EWSIPTOT$plot>19 & EWSIPTOT$plot<26]="LB"
# EWSIPTOT$region[EWSIPTOT$plot>35 & EWSIPTOT$plot<49]="LB"
# EWSIPTOT$type="BDJV"
# EWSIPTOT$type[EWSIPTOT$plot>38]="NAP"
# 
# EWSIP=subset(EWSIPTOT,select=c(region, type, Year, plot, Spp, IP))
# EWSIP=na.omit(EWSIP)
# EWSIP$type2=paste(EWSIP$type, EWSIP$region,sep="")
# 
# 
# EWSTOT=subset(EWSIPTOT,select=c(region, type, Year, plot, Spp, TOT))
# EWSTOT=na.omit(EWSTOT)
# EWSTOT$type2=paste(EWSTOT$type, EWSTOT$region,sep="")
# 
# # General summaries by waterfowl SPP
# 
# EWSIP.ducks=subset(EWSIP,Spp=="ABDU"| Spp=="AGWT" | Spp=="CAGO" | Spp=="COGO" | Spp=="COLO" | Spp=="COME" | Spp=="RBME" | Spp=="RNDU" | Spp=="SUSC")
# 
# 
# EWSIP.ducks.sum=summaryBy(IP~Spp+Year+plot,data= EWSIP.ducks,FUN=sum,na.rm = TRUE)
# par(mfrow=c(3,1))
# plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="ABDU"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Black Ducks")
# 
# plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="AGWT"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Green-wings")
#  plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="CAGO"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Canada Geese")
#  
#  
# 
# 
# library(ggplot2)
# library(dplyr)
# 
# 
# EWSIP.ducks.sum= EWSIP.ducks %>%
#   group_by(Spp,Year) %>%
#   summarise(IP.yr=mean(IP),sd.yr=sd(IP),n.yr=n(),se.yr=sd.yr/sqrt(n.yr), ci.yr=qt(.975,df=n.yr-1)*se.yr)
# EWSIP.ducks.sum
# 
# ggplot(filter(EWSIP.ducks.sum, Spp=="ABDU"|Spp=="AGWT" | Spp=="CAGO" ), aes(x=Year,y=IP.yr))+ geom_errorbar(aes(ymin=IP.yr-ci.yr,ymax=IP.yr+ci.yr ),width=.3)+geom_line()+theme_bw() +facet_grid(Spp~.)+ylab("Density (Indicated Pairs/25 km^2)")
# 
# 
# ggplot(filter(EWSIP.ducks.sum, Spp=="COGO" | Spp=="COLO" | Spp=="COME" ), aes(x=Year,y=IP.yr))+ geom_errorbar(aes(ymin=IP.yr-ci.yr,ymax=IP.yr+ci.yr ),width=.3)+geom_line()+theme_bw() +facet_grid(Spp~.)+ylab("Density (Indicated Pairs/25 km^2)")
#  
# ggplot(filter(EWSIP.ducks.sum, Spp=="RBME" | Spp=="RNDU" | Spp=="SUSC"), aes(x=Year,y=IP.yr))+ geom_errorbar(aes(ymin=IP.yr-ci.yr,ymax=IP.yr+ci.yr ),width=.3)+geom_line()+theme_bw() +facet_grid(Spp~.)+ylab("Density (Indicated Pairs/25 km^2)")
# 
# 
# 
# summ.tab=select(EWSIP.ducks.sum,Spp,Year,IP.yr)
# sum.tab=cast(Year ~ Spp, data = summ.tab, value.var = "IP.yr")
# print(sum.tab, digits=2)
# print(colMeans(sum.tab), digits=2)
# 
#  
# 
# head(x)
# 
# par(mfrow=c(3,1))
# plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="COGO"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Golden-eyes")
# 
# plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="COLO"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Loons")
#  plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="RNDU"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Ring-necks")
#  
#  
# par(mfrow=c(3,1))
# plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="COME"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Common Mergansers")
# 
# plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="RBME"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Red-Breasted Mergansers")
#  plotmeans(IP.sum~Year,data=subset(EWSIP.ducks.sum,Spp=="SUSC"),n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Surf Scoters")
# #  Create subsets for groups of species
# 
# 
# 
# #  Scaups
# EWSIPusca=subset(EWSIP,Spp=="USCA" | Spp=="GRSC" | Spp=="LESC")
# EWSIPusca=(summaryBy(IP~Year,data= EWSIPusca,FUN=sum,na.rm = TRUE)
# 
# par(mfrow=c(3,1))
# plotmeans(IP.sum~Year,data=EWSIPusca,n.label=FALSE,ylab="Indicated Pairs")
#  legend("topleft","Scaups")
# 
# summaryBy(IP.sum~Year,data= EWSIPusca,FUN=mean,na.rm = TRUE)
# 
# # Black Duck, Mallard and Hybrids
# 
# 
# # Puddle Ducks
# #  ***** to do
# # Divers
# #  ***** to do
# 
# 
# EWSTOT=subset(EWSIPTOT,select=c(region, type, Year, plot, Spp, TOT))
# EWSTOT=na.omit(EWSTOT)
# 
# EWStemp=groupedData(TOT~type2 | plot, data=subset(EWSTOT,Year>2001 & Spp=="CAGO"))
# CAGO.lme=lme(TOT ~ type2 -1, data=EWStemp, random=~1 | plot)
# summary(CAGO.lme)
# 
# 
# 
## end
