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
library(forcats)
library(ggrepel)

rego_data<- read.csv("data/cleaned_data/regos.csv")

import_data <- read.csv("data/cleaned_data/imports.csv")


#### Coverage ####

coverage <- rego_data%>%
  group_by(year)%>%
  summarise(certs = sum(number_of_certs))%>%
  mutate(type = "REGO")%>%
  bind_rows(import_data%>%
              group_by(Year)%>%
              summarise(certs = sum(number_of_certs))%>%
              mutate(type = "GO")%>%
              rename(year = Year))%>%
  filter(year <2023)


re_gen <- read.csv("data/raw_data/df_fuel_ckan.csv")

re_gen_clean <- re_gen %>%
  dplyr::select(DATETIME,RENEWABLE)%>%
  mutate(year = year(as.POSIXct(DATETIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))%>%
  group_by(year)%>%
  summarise(total_gen = sum(RENEWABLE))%>%
  filter(year<2023)

ggplot(re_gen_clean,aes(year,total_gen))+
  geom_bar(data = coverage,aes(year,certs,fill=type),stat="identity")+
  geom_line(aes(color = "Renewable generation"))+
  scale_color_manual(values=c("#000000"))+
  labs(x="",y="MWh",fill = "Certificate type",color="")


#### Marginal emissions ####

#imports marginal
imports_marg <- import_data%>%
  mutate(marginal_co2 = if_else(Tech == "Biomass",CO2_in-120,CO2_in))%>%
  rename(year = Year)

marg_total <- rego_data%>%
  dplyr::select(year,marginal_co2,number_of_certs)%>%
  mutate(type="REGO")%>%
  bind_rows(imports_marg%>%dplyr::select(year,marginal_co2,number_of_certs)%>%
              mutate(type = "GO"))

marg_yearly <- marg_total %>%
  mutate(co2_bin = cut(marginal_co2, breaks = 20, labels = paste("Bin", 1:20), include.lowest = TRUE))%>%
  group_by(year,co2_bin)%>%
  summarise(avg_co2 = mean(marginal_co2),certs = sum(number_of_certs))%>%
  filter(year <2023)

ggplot(marg_yearly)+
  geom_line(aes(avg_co2,certs,color = as.factor(year)))+
  labs(x="Emissions avoided [gCO2/kWh]",y="Number of certificates [MWh]",color = "Year")


#negative marginal emissions
marg_neg <- marg_total%>%filter(marginal_co2<0)%>%
  group_by(year,type)%>%
  summarise(certs = sum(number_of_certs))%>%
  mutate(houses = certs/3.6)%>%
  filter(year<2023)



ggplot(marg_neg)+
  geom_bar(aes(year,certs,fill=type),stat="identity")+
  labs(x="",y="Harmful certificates redeemed [MWh]",fill = "Certificate type")+
  # Add the secondary x-axis
  scale_y_continuous(
    sec.axis = sec_axis(~./3.6, name = "No. of avg UK homes power usage [homes]")
  )


#### DRAX ####

drax <- rego_data%>%
  filter(gen_station == "Drax Power Station (REGO)")%>%
  group_by(year,current_holder)%>%
  summarise(certs = sum(number_of_certs))%>%
  mutate(current_holder = as.factor(current_holder))%>%
  filter(certs>100000)

ggplot(drax,aes(year,certs))+
  geom_bar(stat="identity")+
  # Add the secondary x-axis
  scale_y_continuous(
    sec.axis = sec_axis(~./3.6, name = "No. of avg UK homes power usage [homes]")
  )+
  labs(x="",y="Certificates from Drax [MWh]")+
  scale_fill_manual(values = c("blue"))

ggplot(drax,aes(certs,reorder(current_holder,certs)))+
  geom_bar(stat="identity")+
  labs(y="",x="Number of certificates from Drax [MWh] between 2018-22")
