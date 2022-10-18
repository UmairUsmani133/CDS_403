---
title: "CDS-403-001 Midterm Project"
author: "Umair Usmani"
date: "10/17/2022"
output: html_document
---




#These files can be downloaded by exporting them through the links below
  
covid_rates<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\COVID-19_Vaccinations_in_the_United_States_County.csv") 

#https://covid.cdc.gov/covid-data-tracker/#county-view?list_select_state=all_states&list_select_county=all_counties&data-type=Risk

vaccinations<-read.csv("C:\\Users\\Umair\\CDS 403 Project\\United_States_COVID-19_Community_Levels_by_County.csv")

#https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-Community-Levels-by-County/3nnm-4jni

summary(covid_rates)



summary(vaccinations)

#Future integration of weather will be extracted from the database link below:

#https://www.weather.gov/dtx/fnttemp1920-1940