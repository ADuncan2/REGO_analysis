library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(zoo)

### combining rego database scrapes

data18 <- read.csv("data/raw_data/rego_2018.csv",skip=3)
data18$year <- "2018"
data19 <- read.csv("data/raw_data/rego_2019.csv",skip=3)
data19$year <- "2019"
data20 <- read.csv("data/raw_data/rego_2020.csv",skip=3)
data20$year <- "2020"
data21 <- read.csv("data/raw_data/rego_2021.csv",skip=3)
data21$year <- "2021"
data22 <- read.csv("data/raw_data/rego_2022.csv",skip=3)
data22$year <- "2022"
data23 <- read.csv("data/raw_data/rego_2023.csv",skip=3)
data23$year <- "2023"

data<- rbind(data18,data19,data20,data21,data22,data23)

colnames(data) <- c("acred_number", "gen_station", "station_tic","scheme","country","tech_type","gen_type","output_period","number_of_certs","start_cert_num","end_cert_num","mwh_per_cert","issue_date","cert_status","status_date","current_holder","company_reg_num","year")

write.csv(data,file = "data/combined_data/rego_2018_23.csv")




### imports cleaning and combination
#2023
imports_23_1 <- read.csv("data/raw_data/Recognised Guarantees of Origin 2023 (Part 1).csv",skip=3)
imports_23_2 <- read.csv("data/raw_data/Recognised Guarantees of Origin 2023 (Part 2).csv",skip=3)
imports_23_3 <- read.csv("data/raw_data/Recognised Guarantees of Origin 2023 (Part 3).csv",skip=3)
imports23 <- rbind(imports_23_1,imports_23_2,imports_23_3)

imports23 <- imports23 %>% select(GoOs.to.be.recognised.on.behalf.of.,Investment.Support.the.Station.has.Received, Country.of.generation,Technology,Production.period.from,Total.number.of.GoOs.Requested)

colnames(imports23) <- c("current_holder","support_type","Country_of_origin","tech_type","output_period","number_of_certs")

write.csv(imports23,"data/combined_data/imports23.csv")

#2022
imports_22_1<- read.csv("data/raw_data/Recognised Guarentees of Origin 2022 (Part 1).csv",skip=3)
imports_22_2<- read.csv("data/raw_data/Recognised Guarantees of Origin 2022 (Part 2).csv",skip=3)
imports_22_3<- read.csv("data/raw_data/Recognised Guarantees of Origin 2022 (Part 3).csv",skip=3)
imports22 <- rbind(imports_22_1,imports_22_2,imports_22_3)

imports22 <- imports22 %>% select(GoOs.to.be.recognised.on.behalf.of.,Investment.Support.the.Station.has.Received, Country.of.generation,Technology,Production.period.from,Total.number.of.GoOs.Requested)

colnames(imports22) <- c("current_holder","support_type","Country_of_origin","tech_type","output_period","number_of_certs")

write.csv(imports22,"data/combined_data/imports22.csv")

#2021
imports21_1 <- read.csv("data/raw_data/Recognised Guarantees of Origin 2021 Part_1.csv",skip=3)
imports21_2 <- read.csv("data/raw_data/Recognised Guarantees of Origin 2021 Part_2.csv",skip=3)
imports21_3 <- read.csv("data/raw_data/Recognised Guarantees of Origin 2021 Part_3.csv",skip=3)
imports21_4 <- read.csv("data/raw_data/Recognised Guarantees of Origin 2021 Part_4.csv",skip=3)
imports21 <- rbind(imports21_1,imports21_2,imports21_3,imports21_4)

imports21 <- imports21 %>% select(GoOs.to.be.recognised.on.behalf.of.,Investment.Support.the.Station.has.Received, Country.of.generation,Technology,Production.period.from,Total.number.of.GoOs.Requested)
colnames(imports21) <- c("current_holder","support_type","Country_of_origin","tech_type","output_period","number_of_certs")

write.csv(imports21,"data/combined_data/imports21.csv")

#2020
imports20_4 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2020_part_4.csv", skip = 3)
imports20_3 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2020_part_3.csv", skip = 3)
imports20_2 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2020_part_2.csv", skip = 3)
imports20_1 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2020_part_1.csv", skip = 3)
imports20 <- rbind(imports20_1,imports20_2,imports20_3,imports20_4)

imports20 <- imports20 %>% select(GoOs.to.be.recognised.on.behalf.of.,Investment.Support.the.Station.has.Received, Country.of.generation,Technology,Production.period.from,Total.number.of.GoOs.Requested)
colnames(imports20) <- c("current_holder","support_type","Country_of_origin","tech_type","output_period","number_of_certs")

write.csv(imports20,"data/combined_data/imports20.csv")


#2019
imports19_1 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2019_part_1.csv",skip=3)
imports19_2 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2019_part_2.csv",skip=3)
imports19_3 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2019_part_3.csv",skip=3)
imports19_4 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2019_part_4.csv",skip=3)
imports19_5 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2019_part_5.csv",skip=3)
imports19 <- rbind(imports19_1,imports19_2,imports19_3,imports19_4,imports19_5)

imports19<- imports19 %>% select(GoOs.to.be.recognised.on.behalf.of.,Investment.Support.the.Station.has.Received, Country.of.generation,Technology,Production.period.from,Total.number.of.GoOs.Requested)
colnames(imports19) <- c("current_holder","support_type","Country_of_origin","tech_type","output_period","number_of_certs")

write.csv(imports19,"data/combined_data/imports19.csv")

#2018
imports18_1 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2018_part_1.csv", skip = 3)
imports18_2 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2018_part_2.csv", skip = 3)
imports18_3 <- read.csv("data/raw_data/recognised_guarantees_of_origin_2018_part_3.csv", skip = 3)
imports18 <- rbind(imports18_1,imports18_2,imports18_3)

imports18<-imports18 %>% select(GoOs.to.be.recognised.on.behalf.of.,Investment.Support.the.Station.has.Received, Country.of.generation,Technology,Production.period.from,Total.number.of.GoOs.Requested)
colnames(imports18) <- c("current_holder","support_type","Country_of_origin","tech_type","output_period","number_of_certs")

write.csv(imports18,"data/combined_data/imports18.csv")



#### REGO site location data
#rego site information by technology
loc_sewage_gas <- read.csv("data/raw_data/stations_sewage.csv")
loc_solar <- read.csv("data/raw_data/stations_solar.csv")
loc_hydro <- read.csv("data/raw_data/stations_hydro.csv")
loc_bio <- read.csv("data/raw_data/stations_biomass.csv")
loc_wind <- read.csv("data/raw_data/stations_wind.csv")

#allows selection of only a few technologies, not needed for full analysis
#techno <- c("Photovoltaic ","Sewage Gas","Hydro","Biomass","Wind")

#reducing to required information and combining
loc_list <- list(loc_solar,loc_sewage_gas,loc_hydro, loc_bio,loc_wind)

loc_tech_formatting <- function(locs){
  colnames(locs)<- c("acred_number",
                     "statusname",
                     "Name",
                     "schemename",
                     "Capacity",
                     "country",
                     "tech_type",
                     "output_type",
                     "Accred_date",
                     "Comission_date",
                     "Jurisdiction",
                     "Co_address",
                     "fax_num",
                     "Gen_address")
  locs %>%select(acred_number,
                 Name,
                 Capacity,
                 country,
                 Accred_date,
                 Comission_date,
                 Jurisdiction,
                 Co_address,
                 Gen_address)
}

loc_list <- lapply(loc_list,loc_tech_formatting)
loc_techs <- rbindlist(loc_list)
write.csv(loc_techs,"data/combined_data/rego_sites.csv")


### CLEANING EMISSONs DATA ###

ember_23 <- read.csv("data/raw_data/ember_world_em_int_data.csv")

ember_clean <- ember_23%>%
  filter(Continent == "Europe",Variable=="CO2 intensity")%>%
  dplyr::select(Area,Date,Variable,Unit,Value)

write.csv(ember_clean, "data/combined_data/ember_2023_cleaned.csv")
