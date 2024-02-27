#'---
#' title: "Reconstructing data for cSAR-GLOBIOMf integration with SRP LUC"
#' author: "Cindy Azuero"
#' date: "Dec 08, 2023"
#' ---
#' 
#' This script is to reconstruct the data for the model in order to:
#' (1) estimate on an expost basis the biodiversity impact of LUC from agricultural land and pastures to SRP
#' (2) data needed to integrate biodiversity impacts of shift ti SRP into the model.

#' Clean and organized for public repository: Feb 27, 2024
#-------------------------------------------------------------------------

rm(list=ls()) #Clean the environment

baseRoute<-getwd()

setwd(baseRoute)

#Date of the data I am going to create
date<- "Jan08_2024" # Have to keep the format
#Put 50 if 50kmx50km and 200 if 200km x 200km
resolution<-200

if (!dir.exists("InputFiles")) {
  dir.create("InputFiles")
}

#Route to save the created files
route<-paste0(baseRoute,"/InputFiles/Parameters(",date, ")_",resolution,"km")

#Check that the folder where the created files will be saved exist and if not create one
if (!dir.exists(route)) {
  dir.create(route)
}
#-------------------------------------------------------------------------

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#' Libraries

library(gdxrrw)
igdx("/Library/Frameworks/GAMS.framework/Versions/42/Resources")
library(gdxtools)

library(stringr) #for searching with partial matching
library(doBy)# to use summary_by
library(dplyr) # to use select

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#' Since the only affinities data I have is the one I constructed based on Chaudhary et al 2015,
#' I need to check how many ecoregions don't have the data for Agricultural land and grasslands
#' Agricultural land will take values from Annual-crops and Grasslands from Pastures

#### Which ecoregions from the ones in the model don't have data for affinities? ####

#' Here I am starting by reusing code from Chapter 2 for reading the h affinities calculated in 
#' excel and exporting them into a .csv in the format needed, did some slight changes
ecoInfo<-read.csv("./EcoregionsInfoSAR_804(v01 CGA 2024-jan-8).csv")
colnames(ecoInfo)<-ecoInfo[1,]
ecoInfo<-ecoInfo[-1,] #Erase 1st row
colnames(ecoInfo)[1]<-"Ecoregion" #this is eco_code

#### h(l,i,g) ####
#' Recall for Chapter 2, l is ecoregion, i is land use type and g is taxon
#' For GLOBIOMf project we need h(g,i,l)
U<-data.frame(LandUType=c("Extensive-forestry", "Intensive-forestry", "Annual-crops", "Permanent-crops", "Pastures", "Urban"))
G<-data.frame(Taxa=c("Mammals", "Birds", "Reptiles", "Amphibians", "Plants"))

hInfo<-ecoInfo[,c(1,26:30,43:47,60:64,77:81,94:98,111:115)]
hInfo[,-1]<-as.numeric(as.matrix(hInfo[,-1]))
colnames(hInfo)[1]<-"Ecoregion"

typesReorg<-as.matrix(U[c(2,1,3,4,5,6),])

for(i in 1:6){
  #browser()
  if(i==1){ #the first one
    index1<-2
    index2<-6
    h1<-reshape(hInfo[,c(1,index1:index2)], direction="long", varying = list(names(hInfo[,c(1,index1:index2)])[2:6]),v.names = "h",idvar=c("Ecoregion"),timevar="Taxa",times = as.matrix(G))
    h1$LandUType<-typesReorg[i]
    h1<-h1[,c(1,4,2,3)]
  }
  else{
    index1<-index1+5
    index2<-index2+5
    h2<-reshape(hInfo[,c(1,index1:index2)], direction="long", varying = list(names(hInfo[,c(1,index1:index2)])[2:6]),v.names = "h",idvar=c("Ecoregion"),timevar="Taxa",times = as.matrix(G))
    h2$LandUType<-typesReorg[i]
    h2<-h2[,c(1,4,2,3)]
    h1<-rbind(h1,h2)
  }
}

# #Just leave the ecoregions being used
# h_filtered<-h1[apply(as.matrix(h1$Ecoregion),1,function(x) all (x %in% L$Ecoregion)),]
rownames(h1)<-NULL

write.table(h1,file=paste(route,"h_Chaudhary2015.csv",sep="/"), sep=",",row.names = FALSE, col.names = FALSE)
#'------------------------------------------------------------------------------------------------------
#' Importing the h affinities used in Ch3 based on (Chaudhary et al., 2016)

#These are the h constructed for forest management in code Preparing input data_ecoregionLevel (v02 CGA 2022-sep-15).R
#
if(resolution==50){
  h_Ch3_gdx<- gdx("./InputFiles/Parameters(Sep15_2022)_50km/h50.gdx")
}else{#200 km  x 200 km
  h_Ch3_gdx<- gdx("./InputFiles/Parameters(Sep15_2022)_200km/h200.gdx")  
}

h_Ch3<-h_Ch3_gdx["h"]

#' Check which ecoregions were in Ch3 and not in Ch2

#' How many ecoregions are in Ch3?
print(paste("The number of ecoregions in h data for Ch3 were:",length(unique(h_Ch3$ecoregions)),sep=" "))

#' In the ecoregionsExclusions file is specified which, some where because GLOBIOM polygons did not cover them, 
#' or because they don't have a continent assigned. See the "Preparing input data_ecoregionLevel (v02 CGA 2022-sep-15).R"
#' for more info on this. Search for "Ecoregions Exclusions"

#' With the change I did, how many ecoregions are in h from Ch2?
print(paste("The number of ecoregions in h data for Ch2 were:",length(unique(h1$Ecoregion)),sep=" "))

#' Ok, the easiest will be if all the 779 ecoregions used for Ch3 have a corresponding value in h for Ch2.
#' Lets check?

inCh3_missing_Ch2<-unique(subset(select(h_Ch3, ecoregions), !(ecoregions %in% h1$Ecoregion)))

#The only ones missing are Lake and Rock and ice

#' Therefore, I am going to join the h with the format of the h from Ch3
#' it will be h(TAXA, ecoregions, allLUType,value)

#' First reorder columns in h1

h1_toJoin<-h1[,c(3,1,2,4)]
colnames(h1_toJoin)<-c("TAXA", "ecoregions", "allLUType", "value")

#' Leave only the ecoregions already included to avoid problems with syntax
h1_toJoin<-subset(h1_toJoin,(ecoregions %in% h_Ch3$ecoregions)) 

colnames(h_Ch3)[3]<-"allLUType"

all_h<-rbind(h_Ch3,h1_toJoin)

#' I didn't assign anything to Lake and "Rock and Ice" ecoregions, but these are not used

#Create gdx
write.gdx(paste(route,paste0("h",resolution,".gdx"),sep="/"), list(h=all_h))
