#'---
#' title: "Formating Data for GLOBIOMforest + cSAR -ECOREGION LEVEL"
#' author: "Cindy Azuero"
#' date: "Aug 3, 2022"
#' 
#' Clean and organized for public repository: Feb 27, 2024
#' ---

rm(list=ls()) #Clean the environment

baseRoute<-getwd()

setwd(baseRoute)


#Date of the results I am going to process
date<- "Sep15_2022" # Have to keep the format
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

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#' Libraries

library(readxl) # to be able to read from excel files
library(gdxtools) # package to manipulate GDX files in R

# If necessary, tell where is located GAMS.
#In my MAC
igdx("/Library/Frameworks/GAMS.framework/Versions/42/Resources")

#In windows the following may work
# igdx("C:/GAMS/35")
#igdx(dirname(Sys.which('gams'))) 

library(stringr) #for searching with partial matching
library(doBy)# to use summary_by

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#' ASSUMPTIONS/DECISIONS
#' > NA for CF, use the average between the CF of the same taxa and intensity,
#' between ecoregions

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#### Input data ####

# Continent mapping
countryContinent<-read_excel("Mapping country -continent (v01 CGA 2022-jul-15).xlsx","Mapping")

#--- (Chaudhary et al., 2016) biodiversity data----

# The raw data

rawData<-read_excel("Chaudhary_2016_Impact of forest management on species richness_SI-CGA-v2.xls", "Raw data")
rawData<-rawData[,-c(17:19)] #Eliminate last 3 columns

tableS4<-read_excel("Chaudhary_2016_Impact of forest management on species richness_SI-CGA-v2.xls", "Table S4_Import")

#### Define sets ####

taxa<-c("Mammals", "Birds", "Amphibians", "Reptiles", "Plants")
#_______________________________________________________________________________

#---Ecoregions----
#' Here I am going to use (Chaudhary & Brooks, 2018) data in supplementary file

ecoregion2018<-read_excel("Suppporting info-Chaudhary & Brooks 2018-es7b05570_si_001.xlsx","Table S2 Ecoregion and VS", skip=2)
colnames(ecoregion2018)[c(1:5)]<-c("eco_code", "Biome", "Realm", "EcoregionName", "HabitatType")

#Write ecoregions set in gdx

write.gdx(paste(route,"ecoregions.gdx",sep="/"), sets = list(ecoregions=data.frame(ecoregion2018$eco_code)))


#_______________________________________________________________________________

#---Sorg----
#' Indexed in (ecoregion, taxa)

onlySorg<-ecoregion2018[,c("eco_code", "Sorg_mammals", "Sorg_birds", "Sorg_amphibians", "Sorg_reptiles", "Sorg_plants")]
onlySorg_long<-reshape(data.frame(onlySorg), direction = "long", varying = list(names(onlySorg)[2:6]),v.names="Sorg", idvar=c("eco_code"), timevar = "TAXA", times=taxa)
rownames(onlySorg_long)<-NULL
colnames(onlySorg_long)[c(1,3)]<-c("ecoregion","value")


#Write Sorg parameter in gdx
write.gdx(paste(route,"Sorg.gdx",sep="/"), list(Sorg=onlySorg_long))
write.table(onlySorg_long,file=paste(route,"Sorg_Model_NI.csv",sep="/"), sep=",",row.names = FALSE, col.names = TRUE)
#_______________________________________________________________________________

#---VS----
#' Indexed in (ecoregion, taxa)

onlyVS<-ecoregion2018[,c("eco_code", "Mammal_VS", "Birds_VS", "Amphibians_VS", "Reptiles_VS", "Plants_VS")]
onlyVS_long<-reshape(data.frame(onlyVS), direction = "long", varying = list(names(onlyVS)[2:6]),v.names="VS", idvar=c("eco_code"), timevar = "TAXA", times=taxa)
rownames(onlyVS_long)<-NULL
colnames(onlyVS_long)[c(1,3)]<-c("ecoregion","value")


#Write Sorg parameter in gdx
write.gdx(paste(route,"VS.gdx",sep="/"), list(VS=onlyVS_long))
write.table(onlyVS_long,file=paste(route,"VS_Model_NI.csv",sep="/"), sep=",",row.names = FALSE, col.names = TRUE)

#_______________________________________________________________________________
#---CFs----
#' Indexed in (ecoregion, taxa,type_intensity)
#' Where type_intensity is the set that results of all combinations of 
#' broad land use types (managed forests,plantations, pasture, cropland, urban) and
#' management intensities (minimal use, light use, intensive use)
#' Will be the occupation caracterization factors from Table S3, using mean values
#' 
#' (Chaudhary et al., 2018) CFs units are (Potential species loss/m2)

#' Have to:
#' (1) Put the excel table into a long table
#' (2) Separate it into indices (ecoregion, taxa, landUseType, intensity)
#' (3) Extract forest
#' (4) Assign GLOBIOMf_managementTypeG according to mapping
#' (5) Deal with NaN
#' (6) Merge to obtain to GLOBIOMf_managementType using mapping 

conversion2<-10^7 #from Potential species loss/m2 to Potential species loss/thousand ha


assignL_Type<-function(type_intensity){
  if(grepl("clear.cut",type_intensity, fixed=TRUE) | grepl("selective.logging",type_intensity, fixed=TRUE) | grepl("RIL",type_intensity, fixed=TRUE)){
    return("managedForest")
  }
  else if(grepl("plantation",type_intensity, fixed=TRUE)){
    return("plantation")
  }
  else if(grepl("pasture",type_intensity, fixed=TRUE)){
    return("pasture")
  }
  else if(grepl("crop",type_intensity, fixed=TRUE)){
    return("cropland")
  }
  else if(grepl("urb",type_intensity, fixed=TRUE)){
    return("urban")
  }
  else{
    print(paste("Something happened assigning the broad land use type to row value",type_intensity, sep=" "))
  }
}

assignIntensity<-function(type_intensity){
  if(grepl("clear.cut",type_intensity, fixed=TRUE)|grepl("Int",type_intensity, fixed=TRUE)){ 
    return("int")
  }
  else if(grepl("selective.logging",type_intensity, fixed=TRUE)|grepl("Lt",type_intensity, fixed=TRUE)){
    return("lth")
  }
  else if(grepl("RIL",type_intensity, fixed=TRUE)|grepl("min",type_intensity, fixed=TRUE)){
    return("min")
  }

  else{
    print(paste("Something happened assigning the intensity to row value",type_intensity, sep=" "))
  }
}


broadTypes<-c("managedForest", "plantation","pasture", "cropland", "urban")
chaudharyIntensity<-c("min","lth", "int" )

CF_raw<-read_excel("Suppporting info-Chaudhary & Brooks 2018-es7b05570_si_001.xlsx","Table S3 Occupation CFs", skip=2,na="NaN")
colnames(CF_raw)[1]<-"eco_code"

CF<-data.frame()
col<-3

for(t in taxa){
  #' In each element of the loop (for each taxa) I will
  #' (1) Extract first 15 columns + identifiers
  #' (2)Turn to long table
  #' (3) Add taxa column
  #' (4) Add two columns LandUseType and Intensity according to the string in "type_intensity"
  #' (5) Join to the mother table
  #browser()
  
  taxaPiece<-data.frame(CF_raw[,c(1,2,(col:(col+14)))])
  taxaPiece_long<-reshape(taxaPiece, direction = "long", varying = list(names(taxaPiece)[3:17]),v.names="Mean_CF", idvar=c("eco_code", "ecoregion_name"), timevar = "type_intensity", times=names(taxaPiece)[3:17])
  rownames(taxaPiece_long)<-NULL
  taxaPiece_long["TAXA"]<-t
  
  # Create landUseType column
  taxaPiece_long["landUseType"]<-apply(as.matrix(taxaPiece_long$type_intensity),1,assignL_Type)
  # Create intensity column
  taxaPiece_long["intensity"]<-apply(as.matrix(taxaPiece_long$type_intensity),1,assignIntensity)
  
  CF<-rbind(CF,taxaPiece_long)
  
  col<-col+15
}

# Reorganize CF table

CF<-CF[,c(1,2,3,6,7,5,4)]

# Extract forest CFs

CF_forest<-CF[(CF$landUseType=="managedForest"),c(1,5,6,7)]


# Create mapping between intensity and GLOBIOMf_managementTypeG 
managementMapping2018<-data.frame(intensity=chaudharyIntensity,
                              GLOBIOMf_managementTypeG=c("CurL", "CurM", "CurH"))

#Add GLOBIOM broad management intensity to CF_forest
CF_forest<-merge(CF_forest,managementMapping2018, by="intensity")

#Reorder columns
CF_forest<-CF_forest[,c(2,3,1,5,4)]

#How many combinations of ecoregions, taxa and management type have NaN
sum(is.na(CF_forest$Mean_CF))

#' 4635

# Which combinations?
View(CF_forest[(is.na(CF_forest$Mean_CF)),])

# From which ecoregions?
View(data.frame(unique(CF_forest[(is.na(CF_forest$Mean_CF)),1])))
#' is 488 ecoregions. Too much to exclude them


#' For this NA, use the average between the CF of the same taxa and intensity,
#' between ecoregions

aggManagementMissing<-summaryBy(Mean_CF~ GLOBIOMf_managementTypeG+TAXA, FUN=mean, data=CF_forest[!is.na(CF_forest$Mean_CF),])

# Assign the average for NAs

# There is no information in the paper about what the NaN means (No data, don't apply?)

CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurH" &  CF_forest$TAXA=="Amphibians" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[1,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurH" &  CF_forest$TAXA=="Birds" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[2,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurH" &  CF_forest$TAXA=="Mammals" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[3,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurH" &  CF_forest$TAXA=="Plants" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[4,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurH" &  CF_forest$TAXA=="Reptiles" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[5,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurL" &  CF_forest$TAXA=="Amphibians" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[6,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurL" &  CF_forest$TAXA=="Birds" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[7,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurL" &  CF_forest$TAXA=="Mammals" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[8,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurL" &  CF_forest$TAXA=="Plants" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[9,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurL" &  CF_forest$TAXA=="Reptiles" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[10,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurM" &  CF_forest$TAXA=="Amphibians" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[11,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurM" &  CF_forest$TAXA=="Birds" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[12,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurM" &  CF_forest$TAXA=="Mammals" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[13,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurM" &  CF_forest$TAXA=="Plants" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[14,3]
CF_forest[(CF_forest$GLOBIOMf_managementTypeG=="CurM" &  CF_forest$TAXA=="Reptiles" & is.na(CF_forest$Mean_CF)),"Mean_CF"]<-aggManagementMissing[15,3]

# Verify all NA are resolved
View(CF_forest[(is.na(CF_forest$Mean_CF)),])


# Add GLOBIOMf_managementType, this excludes PriFor and Cur0. The biodiversity impact will be if changed to the other management.
# In a sense this assumes it is zero

mappingManagement2<-data.frame(GLOBIOMf_managementType=c("CurC", "CurNC", "CurC_L", "CurNC_L", "CurC_M", "CurNC_M","PriFor", "Cur0"),
                               GLOBIOMf_managementTypeG=c("CurH","CurH", "CurL","CurL","CurM", "CurM","PriFor","Cur0"))

CF_forest<-merge(CF_forest,mappingManagement2, by="GLOBIOMf_managementTypeG", all.x = TRUE)

# Reorganize columns
CF_forest<-CF_forest[,c(2,3,4,1,6,5)]

# Leave only the columnsneeded for GLOBIOMf
CF_forest<-CF_forest[,-c(3,4)]

# Change column names to be consistent with GLOBIOmf
colnames(CF_forest)<- c("ecoregions", "TAXA", "ForMngType", "value")

#Convert units to Potential species loss/thousand ha

CF_forest$value<-CF_forest$value*conversion2

#Write CF parameter in gdx
#' Note: when the gdx is imported, it has 20,324 and not the 24,120 rows.
#' It is because it is only including the nonzero entries. 

write.gdx(paste(route,"CF2018.gdx",sep="/"), list(CF=CF_forest))

#_______________________________________________________________________________
#---mapping_weight----
#' Indexed in (ecoregion, COUNTRY, AllColRow, AltiClass, SlpClass, SoilClass, AEZCLASS)

#--- Spatial units in GLOBIOM forest ----

# define a gdx
mygdx <- gdx('MappingSpatialUnits.gdx')

HARVEST_VAR_info<-mygdx["HARVEST_VAR", field="l"]
LUID_MAP<-mygdx["LUID_MAP"]

#### Define data resolution ####

if(resolution==50){
  
  spatialUnits<-HARVEST_VAR_info[,-c(7,8)]
  #Eliminate duplicates
  spatialUnits<-unique(spatialUnits)
  
  #Areas of polygons that correspond to the intersection between (countries, gridcell at 50 km x 50 km, and ecoregions)
  areasSpatialUnits<-read.csv(r"(ecoregion_country_50grid.txt)")
  
  
}else{#200 km  x 200 km
  
  spatialUnits<-unique(LUID_MAP[,c(1,2)])
  #Add AltiClass, SlpClass, SoilClass, AEZCLASS
  spatialUnits$AltiClass<-"Alti_Any"
  spatialUnits$SlpClass<-"Slp_Any"
  spatialUnits$SoilClass<-"Soil_Any"
  spatialUnits$AEZCLASS<-"Aez_Any"
  
  #Change name to be consistent with 0.5° resolution table
  colnames(spatialUnits)[c(1,2)]<-c("ANYREGION","AllColRow")
  
  #Areas of polygons that correspond to the intersection between (countries, gridcell at 200 km x 200 km, and ecoregions)
  areasSpatialUnits<-read_excel(r"(ecoregion_country_200grid.xls)")
  
}



#To only have the grid
spatialUnits_onlyGrid<-unique(spatialUnits$AllColRow)
#write.table(spatialUnits_onlyGrid,file=paste(route,"Grid.csv",sep="\\"), sep=",",row.names = FALSE, col.names = FALSE, quote=FALSE)


# Create csv for mapping of spatial units used
mappingSpatialUnits<-paste(spatialUnits$ANYREGION,spatialUnits$AllColRow,spatialUnits$AltiClass,spatialUnits$SlpClass, 
                           spatialUnits$SoilClass,spatialUnits$AEZCLASS, sep=".")


#write.table(mappingSpatialUnits,file=paste(route,"SpatialUnits.csv",sep="\\"), sep=",",row.names = FALSE, col.names = FALSE, quote=FALSE)



#Change area from ha to 1000 ha 
conversion<-1/1000

areasSpatialUnits$polyArea<-areasSpatialUnits$polyArea*conversion

areasEcoregions<-summaryBy(polyArea ~ eco_code, FUN=sum, data=areasSpatialUnits)

#' The areas I obtained are not consistent with the areas in the attribute table of the ecoregions shape file, when checking I found that the GLOBIOM grid does not
#' cover some islands or areas and therefore when intersecting, area will be lost. Therefore, the total area per ecoregion used here
#' will be the sum of the areas of the polygons that resulted from the intersection and not the ecoregion areas as in the ecoregions 
#' shapefile
#' 
#' Just in case, the ecoregions in original map are 827, after the intersection only 785. Seems to be because the grid does not cover some ecoregions
#' 

#Now, add the total area calculated for each ecoregion to each polygon
areasSpatialUnits<-merge(areasSpatialUnits,areasEcoregions, by="eco_code")
#Now create the weights for each polygon
areasSpatialUnits["weight"]<-areasSpatialUnits$polyArea/areasSpatialUnits$polyArea.sum

#verify there is no NA
print(paste("This is the amount of observations with NA in the weight column:", sum(is.na(areasSpatialUnits$weight))), sep=" ")

# Define mapping_weight

#' FID is the polygon identifier. NOTE that each ecoregion usually has more than one polygon 

if(resolution==50){
  mapping_weight<-areasSpatialUnits[,c("eco_code", "FID", "Field2","Field3", "polyArea", "polyArea.sum", "weight")]
}else{
  mapping_weight<-areasSpatialUnits[,c("eco_code", "FID","Field2","Field1_1", "polyArea", "polyArea.sum", "weight")]
}

colnames(mapping_weight)<-c("ecoregions", "FID", "COUNTRY", "AllColRow", "polyArea", "ecorArea","weight")

#Add the other indices that define the spatial unit, but remain constant in GLOBIOMf
mapping_weight$AltiClass<-"Alti_Any"
mapping_weight$SlpClass<-"Slp_Any"
mapping_weight$SoilClass<-"Soil_Any"
mapping_weight$AEZCLASS<-"Aez_Any"

# Reorder columns

mapping_weight<-mapping_weight[,c(1,2,3,4,8,9,10,11,5,6,7)]

#' This one has the weight of each polygon(intersection of ecoregions and GLOBIOMf spatial units) in the ecoregion

#' Aggregate the area between polygons in the same ecoregion and GLOBIOMf spatial units, that means over those
#' cases where the ecoregion had more than one polygon and therefore the are more than one polygon with this intersection

mapping_weight2<-summaryBy(polyArea ~ ecoregions + COUNTRY + AllColRow+ AltiClass+ SlpClass+ SoilClass+ AEZCLASS+ecorArea, FUN=sum, data=mapping_weight)

#Recalculate weight of spatial unit in ecoregion

mapping_weight2["weight"]<-mapping_weight2$polyArea.sum/mapping_weight2$ecorArea
colnames(mapping_weight2)[9]<-"polyAreaSum"
mapping_weight2["combExist"]<-1

# Put in long format with set dataItemsBio
mapping_weight2_long<-reshape(mapping_weight2, direction = "long", varying = list(names(mapping_weight2[8:11])),v.names = "value", 
                             idvar = c("ecoregions", "COUNTRY", "AllColRow", "AltiClass", "SlpClass", "SoilClass", "AEZCLASS"), timevar = "dataItemsBio", times = names(mapping_weight2[8:11]))
rownames(mapping_weight2_long)<-NULL



write.gdx(paste(route,paste0("mapping_weight",resolution,".gdx"),sep="/"), list(mapping_weight=mapping_weight2_long))

#_______________________________________________________________________________
#---h----
#' Indexed in (taxa (g) ,ecoregions (j),ForMngType (i) )
#' 
#' The h will be to calculate using the cSAR formula. Since I don't have (Chaudhary & Brooks, 2018)
#' affinity factors, I am going to use the (Chaudhary et al., 2016) ones processed
#' It will be assumed that the ecoregion take the value of h(g,j,i) of the continent to which it belongs
#' and the continent to which it belongs will be determined via the country. 

#' The parameter will be h(TAXA, COUNTRY, ALLCOLLROW,AltiClass,SlpClass, SoilClass, AEZCLASS, ForMngType) 


ForMngType<-c("PriFor","Cur0", "CurC_L", "CurNC_L", "CurC_M", "CurNC_M", "CurC", "CurNC")

# Redefine taxa because from (Chaudhary et al., 2016) there is no information on reptiles
taxa<-c("Mammals", "Birds", "Amphibians", "Plants")

# Extract from areasSpatialUnits a mapping of ecoregions with the countries in GLOBIOMf

#' Note that polyArea is the area of the FID polygon, and polyArea.sum is the area of the ecoregion to which
#' FID polygon belongs in areSpatialUnits

colnames(areasSpatialUnits)[30]<-"ecorArea"
eco_countries<-summaryBy(polyArea~ eco_code + Field2+ecorArea, FUN=sum, data=areasSpatialUnits)
colnames(eco_countries)[c(2,4)]<-c("COUNTRY","eco_countryArea")

ecoregionsAfghanistan_NotAccounted<-unique((eco_countries[eco_countries$COUNTRY=="Afghanistan" | eco_countries$COUNTRY=="NotAccounted",])[,1])

# Add the continent to eco_countries
eco_countries<-merge(eco_countries,countryContinent[,-3], by="COUNTRY")

# Create eco_continent
eco_continent<- summaryBy(eco_countryArea ~ eco_code+ecorArea+CONTINENT+CONTINENT_DATA, FUN=sum, data= eco_countries)
colnames(eco_continent)[5]<-"eco_continentArea"

eco_continent["weight"]<-eco_continent$eco_continentArea/eco_continent$ecorArea


h<-merge(merge(taxa,eco_continent),data.frame(ForMngType))
colnames(h)[c(1,2)]<-c("TAXA","ecoregions")


#Define forest management mapping

managementMapping<-data.frame(Chaudhary_managementType=c("Clear-cuting","Retention", "Selection system", "Selective logging", 
                                                         "Reduced impact logging", "Plantation-timber", "Plantation-fuel"),
                              GLOBIOMf_managementTypeG=c("CurH", "CurL", "CurL","CurM", "CurL","CurH","CurH"))


#' STEPS:
#' (1) Aggregate the h(Chaudhary_managementType, continent, taxa) to h(GLOBIOMf_managementTypeG, continent, taxa) 
#' by averaging between managements for each (continent, taxa)
#' (2) Add continent to h
#' (3) Create a function or way to indicate that if h$Continent==continentIn aggregated one AND (h$ForMngType=="CurC" OR h$ForMngType=="CurNC"), 
#' then takes the value of CUR for the respective continent.
#' 


colnames(tableS4)[1]<-"Chaudhary_managementType"
tableS4_2<-merge(tableS4,managementMapping, by="Chaudhary_managementType")

# Assumption same weight between different management types

aggManagement<-summaryBy(h~ GLOBIOMf_managementTypeG+Continent+Taxon, FUN=mean, data=tableS4_2)
colnames(aggManagement)[3]<-"TAXA"
#Change name of Continent var to CONTINENT_DATA for merge with h
colnames(aggManagement)[2]<-"CONTINENT_DATA"


#' The ones that are lost are because a continent is not defines for Afghanistan and NotAccounted.
#' This leaves 52,000


colnames(h)[8]<-"GLOBIOMf_managementType"

# Add GLOBIOMf_managementTypeG
h<-merge(h,mappingManagement2, by="GLOBIOMf_managementType")

# Add the h from the continent
h<-merge(h,aggManagement,by=c("GLOBIOMf_managementTypeG","TAXA","CONTINENT_DATA"), all.x = TRUE)

#Add that the h (affinity value) is one for PriFor and secondary forest (Cur0)
h[(h$GLOBIOMf_managementType=="PriFor" |h$GLOBIOMf_managementType=="Cur0"),"h.mean"]<-1

#verify there is no NA
print(paste("This is the amount of observations with NA in h column:", sum(is.na(h$h.mean))), sep=" ")

# Which combinations are the ones for which (Chaudhary et al., 2016) data is not available?
missing_h_combinations<-unique(h[(is.na(h$h.mean)),c("GLOBIOMf_managementTypeG", "TAXA", "CONTINENT_DATA")])
View(missing_h_combinations)


#' There are 65 combinations with no data
#' For these combinations, and following (Chaudhary et al., 2015), I will use the global average
#' for these ones. Global average between management types of same "intensity", taxa and continent, for each management type in GLOBIOMf.
#' 
#' Some combinations of particular concern are those regions for which a forest management type
#' does not apply. For example, selection system is for Europe and North America, but not for south america.
#' This could be addressed with some type of constraint in the model indicating that in those areas you cannot
#' use a specific management type. However, since management types in GLOBIOMf are aggregated in 3 levels of 
#' intensity, then this constraints cannot be directly added right now.
#' 

# Assumption same weight between different management types, only considering taxa included

aggManagementMissing_h<-summaryBy(h~ GLOBIOMf_managementTypeG, FUN=mean, data=tableS4_2[(tableS4_2$Taxon=="Mammals" |tableS4_2$Taxon=="Birds"| tableS4_2$Taxon=="Plants" | tableS4_2$Taxon=="Amphibians"),])

# Assign the average for NAs
h[((h$GLOBIOMf_managementType=="CurC_L" |h$GLOBIOMf_managementType=="CurNC_L") & is.na(h$h.mean)),"h.mean"]<-aggManagementMissing_h[2,2]
h[((h$GLOBIOMf_managementType=="CurC_M" |h$GLOBIOMf_managementType=="CurNC_M") & is.na(h$h.mean)),"h.mean"]<-aggManagementMissing_h[3,2]
h[((h$GLOBIOMf_managementType=="CurC" |h$GLOBIOMf_managementType=="CurNC") & is.na(h$h.mean)),"h.mean"]<-aggManagementMissing_h[1,2]

#verify there is no NA
print(paste("This is the amount of observations with NA in h column:", sum(is.na(h$h.mean))), sep=" ")


# Reorganize columns
h<-h[,c("TAXA","ecoregions", "ecorArea", "CONTINENT", "CONTINENT_DATA","eco_continentArea","weight", "GLOBIOMf_managementTypeG", 
        "GLOBIOMf_managementType", "h.mean")]


# Check if there are ecoregions in more than one continent
View(unique(h[,c("ecoregions","CONTINENT_DATA")]))

#' Yes, there are. The h to those ecoregions will be assigned as the weighted average, by area in the continent
#' of the h of the continent to which it belongs

#Aggregate the areas so I have the area per ecoregion in each continent

#Add columns with sumprod between weight and h.mean

h["sumProd"]<-h$weight*h$h.mean

h_continent<-summaryBy(sumProd~ TAXA+ecoregions+GLOBIOMf_managementType, FUN=sum, data=h)

#'With this average, the h for primary and secondary forest were changed, sometimes to values != 1
#'Here I just reassign it to correct it

#Add that the h (affinity value) is one for PriFor and secondary forest (Cur0)
h_continent[(h_continent$GLOBIOMf_managementType=="PriFor" |h_continent$GLOBIOMf_managementType=="Cur0"),"sumProd.sum"]<-1


#' This one has 779 ecoregions, 4 taxa, 8 forest management. All combinations result in 24,928
#' which is the number of rows
#' 
#' 38 ecoregions were in Afghanistan and NotAccounted
#' From those 
finalEcoregions<-unique(h_continent$ecoregions)

#' Which ecoregions from the 38 were not included because they were not in another country?

ommittedEcoregions<-setdiff(ecoregionsAfghanistan_NotAccounted, finalEcoregions)
#' Only 6 ecoregions in this set of omittedEcoregions
#' In areaSpatial units there are 785 and this comes from the intersection in the maps

# Format h for GAMS

colnames(h_continent)[c(3,4)]<-c("ForMngType", "value")

#Create gdx
write.gdx(paste(route,paste0("h",resolution,".gdx"),sep="/"), list(h=h_continent))

#_______________________________________________________________________________
#---Ecoregions Exclusions----

ecoregionsAt_table<-read_excel("EcoregionsAttributeTable.xls", "EcoregionsAttributeTable" )


ecoregionsExclusions<-unique(ecoregionsAt_table[,c("eco_code", "ECO_NAME")])


# Which ecoregions where lost after the intersection between GLOBIOM polygons and ecoregions?
arcGIS_int<-subset(ecoregionsExclusions, !(eco_code %in% areasSpatialUnits$eco_code))
arcGIS_int["arcGIS_Int"]<-1

#' ...Why lost? the GLOBIOM layer didn't cover them.

# Add the ecoregions lost in intersection
ecoregionsExclusions<-merge(ecoregionsExclusions, arcGIS_int[,c(1,3)], by="eco_code", all.x = TRUE)

#' areaSpatialUnits has 785 ecoregions (827-42 lost in arcGIS intersection)
#' eco_countries has 779

# Which ecoregions are lost when constructing eco_countries?
inEcoCountries<-subset(ecoregionsExclusions, !(eco_code %in% eco_countries$eco_code))
inEcoCountries["inEcoCountries"]<-1

# There are 48 of the original ecoregions that are not in eco_countries
# From these 6 were not excluded by arcGIS intersection

#'...Why lost? Afghanistan and Not accounted doesn't have a continent assigned. These countries
#'correspond to 38 ecoregions, but from those 38 only 6 ecoregions were exclusively in Afghanistan
#'or Not accounted and therefore excluded in eco_countries

ecoregionsExclusions<-merge(ecoregionsExclusions, inEcoCountries[,c("eco_code","inEcoCountries")], by="eco_code", all.x = TRUE)

#Print to csv
write.table(ecoregionsExclusions,file=paste(route,"Ecoregions_exclusions.csv",sep="/"), sep=",",row.names = FALSE)

#' With this, we explain why there are 779 in the databases imported to GAMS
