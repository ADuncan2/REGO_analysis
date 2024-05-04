library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(zoo)
library(data.table)
library(sf)
library(broom)
library(anytime)
library(quanteda)
library(stringdist)

#### IMPORTS ####
imports18 <- read.csv("data/combined_data/imports18.csv")
imports19 <- read.csv("data/combined_data/imports19.csv")
imports20 <- read.csv("data/combined_data/imports20.csv")
imports21 <- read.csv("data/combined_data/imports21.csv")
imports22 <- read.csv("data/combined_data/imports22.csv")
imports23 <- read.csv("data/combined_data/imports23.csv")

imports23$Year <- "2023"
imports22$Year <- "2022"
imports21$Year <- "2021"
imports20$Year <- "2020"
imports19$Year <- "2019"
imports18$Year <- "2018"

imports <- rbind(imports18,imports19,imports20,imports21,imports22,imports23)

rm(imports18,imports19,imports20,imports21,imports22,imports23)

#Removing empty rows and standardising output period dates
imports_date<- imports%>%
  filter(!is.na(number_of_certs), current_holder != "")%>%
  mutate(output_period_clean = parse_date_time(output_period,orders = c('dmy','ymd','ym')),
         output_period_clean = floor_date(output_period_clean,unit="month"))

#fixing technologies
imports_tech <- imports_date %>%
  mutate(Tech = case_when(str_detect(tech_type, "T01|Solar|olar|hotovoltaic|PV|SOLAR") ~ "Solar",
                          str_detect(tech_type, "T02|Wind|ind|WIND")~"Wind",
                          str_detect(tech_type, "Bio|ood|forest|Agri|Crop|crop")~"Biomass",
                          str_detect(tech_type, "T05|hermal|Steam")~"Thermal",
                          str_detect(tech_type, "ydro|T03")~"Hydro",
                          TRUE ~ NA_character_))


rm(imports_date)

imports_num <- imports_tech%>%
  mutate(number_of_certs = as.numeric(str_replace(number_of_certs, ",", "")))

rm(imports_tech)

# Create a mapping dictionary for standardization of country names
country_mapping_df <- data.frame(
  uncleaned_name = c(
    " Belgium", " Denmark", " Estonia", " Luxembourg", " Netherlands",
    "Austria", "Belgium", "Bulgaria", "Croatia", "CZ",
    "Czech Republic", "Denmar", "Denmark", "Estonia", "Finland",
    "Finland", "France", "Germany", "Greece", "GREECE",
    "Italy", "ITALY", "Luxembourg", "Netherlands", "Poland", "Poland ",
    "Portugal", "Slovenia", "Spain", "Sweden"
  ),
  cleaned_name = c(
    "Belgium", "Denmark", "Estonia", "Luxembourg", "Netherlands",
    "Austria", "Belgium", "Bulgaria", "Croatia", "Czechia",
    "Czechia", "Denmark", "Denmark", "Estonia", "Finland",
    "Finland", "France", "Germany", "Greece", "Greece",
    "Italy", "Italy", "Luxembourg", "Netherlands", "Poland", "Poland",
    "Portugal", "Slovenia", "Spain", "Sweden"
  )
)

mapping_dict <- setNames(country_mapping_df$cleaned_name, country_mapping_df$uncleaned_name)


imports_country<- imports_num%>%
  mutate(Country_of_origin = mapping_dict[Country_of_origin])

rm(imports_num)


imports_company<- imports_country%>%
  mutate(current_holder = case_when(str_detect(current_holder, "British Gas") ~ "British gas",
                             str_detect(current_holder, "Ovo|OVO") ~ "OVO",
                             str_detect(current_holder, "EDF")~ "EDF",
                             str_detect(current_holder, "ctopus")~"Octopus",
                             str_detect(current_holder, "E.ON")~"E.ON",
                             str_detect(current_holder, "SSE")~"SSE",
                             TRUE ~ current_holder))

rm(imports_country)

co2_national <- read.csv("data/combined_data/ember_2023_cleaned.csv")%>%
  mutate(Date = as.Date(Date,format="%d/%m/%Y"),
         Year = year(Date))%>%
  filter(Area %in% country_mapping_df$cleaned_name)%>%
  filter(Year > 2014)%>%
  dplyr::select(Area,Date,Value)%>%
  rename(CO2_in = Value)

imports_co2 <- imports_company%>%
  left_join(co2_national,by=c("Country_of_origin"="Area","output_period_clean"="Date"))

rm(imports_company)
### troubleshooting ###
imports_co2_test <- imports_co2%>%
  filter(is.na(CO2_in))



imports_final <- imports_co2%>%
  dplyr::select(Year,Country_of_origin,Tech,current_holder,output_period_clean, number_of_certs,CO2_in)%>%
  group_by(Year,output_period_clean,current_holder,Tech,Country_of_origin,CO2_in)%>%
  summarise(number_of_certs = sum(number_of_certs))%>%
  mutate(Type = "Import")

rm(imports_co2,imports_company,imports,co2_national,country_mapping_df)

write.csv(imports_final,"data/cleaned_data/imports.csv")

#### REGOS ####
regos <- read.csv("data/combined_data/rego_2018_23.csv")

# fixing output period date format 
regos_date <- regos %>%
  mutate(output_period_clean = if_else(grepl(" - ",output_period), str_extract(output_period, "\\d{2}/\\d{2}/\\d{4}"),output_period))%>%
  mutate(output_period_clean = parse_date_time(output_period_clean,orders = c('dmy','my')))%>%
  dplyr::select(acred_number,country,gen_station,tech_type,number_of_certs,current_holder,year,output_period_clean)

## reading in generator location data for REGO certified sites
loc_techs <- read.csv("data/combined_data/rego_sites.csv")%>%
  dplyr::select(acred_number,Capacity,Name,Gen_address,Comission_date)%>%
  mutate(Gen_address = as.character(Gen_address))%>%
  mutate(address_count = as.numeric(str_count(Gen_address,"\n")))%>%
  mutate(Comission_date = dmy(Comission_date))
#note: Generators with capacity <50kW don't record an address and therefore can't be matched with a regional emissions intensity


#combining generator addresses with REGO data
postcode_str<-function(address_str,address_count){
  add_list <- str_split(address_str,"\n")
  postcode <- add_list[[1]][address_count]
  postcode <- toupper(postcode)
  pc_length <- str_length(postcode)
  if(address_count !=0){
    if (grepl(" ",postcode)==FALSE){
      pc_lhs<-str_sub(postcode, 1,pc_length-3)
      pc_lhs<- str_pad(pc_lhs,str_length(pc_lhs)+1,side="right",pad = " ")
      pc_rhs<- str_sub(postcode,pc_length-3,pc_length)
      postcode <- str_c(pc_lhs,pc_rhs)
    }
  }
  if(address_count == 0 ){
    postcode<-NA
  }
  return(postcode)
}

# extract and standardise postcodes from addresses 
loc_techs$postcode <- mapply(postcode_str,loc_techs$Gen_address,loc_techs$address_count)

## adding lat and long based on postcode
pc_lat_long <- read.csv("data/raw_data/ukpostcodes.csv")

loc_techs_ll <- loc_techs%>%
  left_join(pc_lat_long,by="postcode")


## matching to FIT and CfD sites ##

#FiT sites
fit_sites_1 <- read.csv("data/raw_data/Feed-in Tariff Installation Report May 2022 Part 1.csv",skip=4)%>%
  filter(!is.na(Installed.capacity))%>%
  mutate(Technology = case_when(Technology == "Micro CHP" ~ "Biomass",
                                Technology == "Anaerobic digestion" ~ "Biomass",
                                TRUE ~ Technology))%>%
  dplyr::select(PostCode,Technology,Installed.capacity,Constituency,Commissioning.date)

fit_sites_2 <- read.csv("data/raw_data/Feed-in Tariff Installation Report May 2022 Part 2.csv",skip=4)%>%
  filter(!is.na(Installed.capacity))%>%
  mutate(Technology = case_when(Technology == "Micro CHP" ~ "Biomass",
                                Technology == "Anaerobic digestion" ~ "Biomass",
                                TRUE ~ Technology))%>%
  dplyr::select(PostCode,Technology,Installed.capacity,Constituency,Commissioning.date)

fit_sites_3 <- read.csv("data/raw_data/Feed-in Tariff Installation Report May 2022 Part 3.csv",skip=4)%>%
  filter(!is.na(Installed.capacity))%>%
  mutate(Technology = case_when(Technology == "Micro CHP" ~ "Biomass",
                                Technology == "Anaerobic digestion" ~ "Biomass",
                                TRUE ~ Technology))%>%
  dplyr::select(PostCode,Technology,Installed.capacity,Constituency,Commissioning.date)

fit_sites <- rbind(fit_sites_1,fit_sites_2,fit_sites_3)%>%
  filter(Installed.capacity >=50)%>%
  mutate(Commissioning.date = dmy(Commissioning.date))

rm(fit_sites_1,fit_sites_2,fit_sites_3)

#isolating the beginning of the postcode
postcode_start <- function(pc){
  pc1 <- str_split(pc," ")
  pc2 <- pc1[[1]][1]
  return(pc2)
}

loc_techs_ll$start_pc <- mapply(postcode_start,loc_techs_ll$postcode)


loc_tech_fit <- loc_techs_ll%>%
  mutate(Tech = case_when(
    grepl("FW",acred_number) ~ "Wind",
    grepl("NW",acred_number) ~ "Wind",
    grepl("PV",acred_number) ~ "Photovoltaic",
    grepl("HY",acred_number) ~ "Hydro",
    grepl("BW",acred_number) ~ "Biomass",
    grepl("SG",acred_number) ~ "Biomass",
    TRUE ~ NA_character_
  ))


loc_tech_fit_match<- loc_tech_fit%>%
  left_join(fit_sites,by = c("Tech" = "Technology","Capacity"="Installed.capacity","Comission_date"= "Commissioning.date"))%>%
  filter(!is.na(Constituency))%>%
  mutate(subsidy = "FiT")%>%
  dplyr::select(acred_number,subsidy)



## CfD matching
cfd_sites <- read.csv("data/raw_data/auction-outcomes.csv")

# Example using Jaccard distance
dist_matrix <- stringdist::stringdistmatrix(loc_techs$Name, cfd_sites$Project_Name, method = "jaccard")
threshold <- 0.1
match_matrix <- as.data.frame(which(dist_matrix <= threshold, arr.ind = TRUE))

locs_cfd <- cbind(loc_techs[match_matrix$row,]%>%
  dplyr::select(Name),cfd_sites[match_matrix$col,]%>%dplyr::select(Project_Name))

non_cfd_sites <- c("Castlecraig Wind Farm","Tirgoland Farm")

locs_cfd<-locs_cfd%>%
  filter(!Name %in% non_cfd_sites )

locs_cfd_match <- loc_tech_fit%>%
  filter(Name %in% locs_cfd$Name)%>%
  mutate(subsidy = "CfD")%>%
  dplyr::select(acred_number,subsidy)

loc_subsidy <- rbind(loc_tech_fit_match,locs_cfd_match)

subsidy_rego <- regos_date%>%
  filter(acred_number %in% loc_subsidy$acred_number)%>%
  group_by(year)%>%
  summarise(certs = sum(number_of_certs))

loc_techs_sub <- loc_techs_ll%>%
  mutate(subsidy = "NA")%>%
  mutate(subsidy = case_when(
    acred_number %in% loc_tech_fit_match$acred_number ~ "FiT",
    acred_number %in% locs_cfd_match$acred_number ~ "CfD",
    TRUE ~ NA_character_
  ))

rm(regos,pc_lat_long,match_matrix,locs_cfd_match,locs_cfd,loc_techs_sub)

#### Matching stations to regions using shapefiles ####
#importing shape file of DNO regions in UK (ex. N Ireland)
shape <- st_read("data/dno_license_areas_20200506/DNO_License_Areas_20200506.shp")%>%
  mutate(LongName = as.factor(LongName))
#fixing names to match regional emissions dataset
names <- c("North West England",
           "North East England",
           "Yorkshire",
           "South Scotland",
           "North Wales and Merseyside",
           "North Scotland",
           "South England",
           "East England",
           "London",
           "South East England",
           "East Midlands",
           "West Midlands",
           "South Wales",
           "South West England")

levels(shape$LongName) <- as.factor(names)

#creating subset of generator data that has latitude and longitude matches as regional matching requires it and wont work with NAs
loc_tech_w_pc <- loc_techs_sub%>%
  filter(!is.na(latitude))

#creating sf point object from latitude and longitude data
data_sf <- st_as_sf(loc_tech_w_pc,coords=c("longitude","latitude"),crs = 4326,remove = FALSE)

#transform both datasets to the same crs projection
data_sf <- st_transform(data_sf,crs=4326)
shape <- st_transform(shape,crs=4326)

#match based on shapes
results <- st_join(data_sf,left = TRUE,shape["LongName"])%>%
  mutate(LongName = as.character(LongName),
         LongName = ifelse(is.na(LongName), "Northern Ireland", LongName))
#drop geometry to reduce size
results_wo_geo <- st_drop_geometry(results)


#combining postcodes to REGO data
rego_loc <- regos_date%>%
  left_join(results_wo_geo,by="acred_number")

### combining REGOs with regional emissions data based on region and output period
regional_co2<- read.csv("data/raw_data/regional_carbon_intensity_2023.csv")

#cleaning and making monthly averages from hourly data
regional_co2_month <- regional_co2%>%
  pivot_longer(!datetime,names_to = "name",values_to = "co2")%>%
  mutate(date = as.Date(ymd_hms(datetime)),month = format(floor_date(as.Date(date),unit = "month"),"%d/%m/%Y"),year = year(date))%>%
  filter(!is.na(co2))%>%
  group_by(name,month)%>%
  summarise(meanco2 = round(mean(co2),digits = 1),high_co2 = if_else(co2>120,0,1),prob= round(sum(high_co2)/n(),digits = 1))%>%
  select(name,month,meanco2,prob)%>%
  distinct()%>%
  mutate(name = gsub("\\."," ",name))%>%
  mutate(name = as.factor(name))%>%
  mutate(month = as.Date(month,format = "%d/%m/%Y"))
#note: probability based on biomass threshold of 120gCO2/kWh which is hard coded here


#combining REGOs with emissions data by region
rego_co2 <- rego_loc%>%
  left_join(regional_co2_month,by=c("LongName"="name","output_period_clean"="month"))%>%
  dplyr::select(acred_number,country,gen_station,tech_type,current_holder,year,output_period_clean,Capacity,postcode,subsidy,LongName,meanco2,prob,number_of_certs)%>%
  rename(generation_period = output_period_clean,
         region = LongName,
         mean_co2_per_kwh = meanco2)


#calculating marginal emissions based on lifetime co2 emission
rego_marg_co2 <- rego_co2%>%
  mutate(marginal_co2 = if_else(tech_type %in% c("Biodegradable","Biogas","Biomass","Landfill Gas","Sewage Gas"),mean_co2_per_kwh - 120,mean_co2_per_kwh))%>%
  dplyr::select(acred_number,country,gen_station,tech_type,current_holder,year,generation_period,Capacity,postcode,subsidy,region,mean_co2_per_kwh,marginal_co2,prob,number_of_certs)

rm(regional_co2,regos,regos_date,results_wo_geo,shape,data_sf)



write.csv(rego_marg_co2,"data/cleaned_data/regos.csv")


