# ---
# title: "Report on COVID-19 Project"
# author: "Pedro J. Llanos"
# date: "1/6/2020"
# #output: html_document 
# output: pdf_document
# #output: word_document
# #output: github_document
# ---
  

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(ggpmisc)) install.packages("ggpmisc", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gdata)) install.packages("gdata", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(rgl)) install.packages("rgl", repos = "http://cran.us.r-project.org")
if(!require(ggpmisc)) install.packages("ggpmisc", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(gdata)
library(ggpubr)
library(reshape2)
library(rgl)
library(lubridate)
library(ggpmisc)
library(RCurl)

#Get data directly from internet:
#csv data file needs to be in the same directory of "MyOwnProject.R"
#Can be obtained from: https://datascience.nih.gov/covid-19-open-access-resources

url <- "https://covid19-lake.s3.us-east-2.amazonaws.com/rearc-covid-19-world-cases-deaths-testing/csv/covid-19-world-cases-deaths-testing.csv"
tempfile <- getURL(url)
covid_data <- read.csv(textConnection(tempfile))
head(covid_data)

#or download the file direclty into your working directory:

download.file("https://covid19-lake.s3.us-east-2.amazonaws.com/rearc-covid-19-world-cases-deaths-testing/csv/covid-19-world-cases-deaths-testing.csv",
                            destfile="covid-19-world-cases-deaths-testing.csv",
                            method="libcurl")

#then read the file
covid_data <- read_csv("covid-19-world-cases-deaths-testing.csv", guess_max = 100000)
class(covid_data)
covid_data <- as.data.frame(covid_data)
names(covid_data)
head(covid_data)
class(covid_data)

#Check for missing values
is.na(covid_data)
#Replace missing value with 0
covid_data[is.na(covid_data)] <- 0
covid_data
is.na(covid_data)


#Cumulative cases in the world
CV_totalcases <- as.data.frame(cbind(covid_data$date, covid_data$total_cases))
names(CV_totalcases)[names(CV_totalcases) == "V1"] <- "date"
names(CV_totalcases)[names(CV_totalcases) == "V2"] <- "total_cases"
covid_data$date <- as.Date(covid_data$date, '%m/%d/%Y')

CV_totalcases <- covid_data %>% group_by(date) %>%
  summarize(cum_cases = max(total_cases)) 
CV_totalcases<-CV_totalcases[!is.na(CV_totalcases$cum_cases),]
ggplot(CV_totalcases, aes(date, cum_cases)) +
  geom_point(color = 'blue', size = 0.5) +
  ggtitle("Cumulative COVID-19 Cases in 2020")+
  ylab("Cumulative confirmed cases")+
  xlab("Date (month in 2020)") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('COVID_cumcases_world.bmp')

#Cumulative deaths in the world
CV_totaldeaths <- as.data.frame(cbind(covid_data$date, covid_data$total_deaths))
names(CV_totaldeaths)[names(CV_totaldeaths) == "V1"] <- "date"
names(CV_totaldeaths)[names(CV_totaldeaths) == "V2"] <- "total_deaths"
covid_data$date <- as.Date(covid_data$date, '%m/%d/%Y')

CV_totaldeaths <- covid_data %>% group_by(date) %>%
  summarize(cum_deaths = max(total_deaths))
CV_totaldeaths<-CV_totaldeaths[!is.na(CV_totaldeaths$cum_deaths),]
ggplot(CV_totaldeaths, aes(date, cum_deaths)) +
  geom_point(color = 'red', size = 0.5) +
  ggtitle("Cumulative COVID-19 Deaths in 2020")+
  ylab("Cumulative confirmed deaths")+
  xlab("Date") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('COVID_cumdeaths_world.bmp')

#Set initital time and final time of data
t0 <- covid_data$date[1] # "2020-01-23"
tf <- tail(covid_data$date)[6] #varies according to when you download the data

#Cumulative cases by country in the world
CV_totalcases <- covid_data %>% group_by(date, location) %>%
  summarize(cum_cases = max(total_cases)) %>% 
  filter(cum_cases > 1.6E6, date >= t0, location != "World") %>% 
  top_n(10,cum_cases)
CV_totalcases<-CV_totalcases[!is.na(CV_totalcases$cum_cases),]
ggplot(CV_totalcases, aes(date, cum_cases, group= location, color=location))+
  geom_point(size = 0.5)+
  ggtitle("Cumulative Confirmed Cases by Country")+
  xlab("Date") + ylab("Cumulative Confirmed Cases by country") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('COVID_cumcases_countries.bmp')

#Cumulative deaths by country in the world
CV_totaldeaths <- covid_data %>% group_by(date, location) %>%
  summarize(cum_deaths = max(total_deaths)) %>% 
  filter(cum_deaths > 40000, date >= t0, location != "World") %>% 
  top_n(10,cum_deaths)
CV_totaldeaths<-CV_totaldeaths[!is.na(CV_totaldeaths$cum_deaths),]
ggplot(CV_totaldeaths, aes(date, cum_deaths, group= location, color=location))+
  geom_point(size = 0.5)+
  ggtitle("Cumulative Confirmed Deaths by Country")+
  xlab("Date") + ylab("Cumulative Confirmed Deaths by country") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('COVID_cumdeaths_countries.bmp')

top_countries_by_total_cases <- covid_data %>%
  filter(location != "World") %>% 
  group_by(location) %>%
  summarize(cum_cases = max(total_cases)) %>% 
  arrange(desc(cum_cases)) %>%
  top_n(20, cum_cases)
top_countries_by_total_cases
TCTC <- top_countries_by_total_cases$location

top_countries_by_total_deaths <- covid_data %>%
  filter(location != "World") %>% 
  group_by(location) %>% 
  summarize(cum_deaths = max(total_deaths)) %>%
  arrange(desc(cum_deaths)) %>%
  top_n(20, cum_deaths)
top_countries_by_total_deaths
TCTD <- top_countries_by_total_deaths$location
#most countries that have the most number of cases have also the most number of deaths
as.data.frame(rbind(TCTC, TCTD))

# See the result, top 20 countries in descending/ascending ORDER
top_countries_by_total_cases[order(-top_countries_by_total_cases[,2]),]
top_countries_by_total_cases[order(+top_countries_by_total_cases[,2]),]
top_countries_by_total_deaths[order(-top_countries_by_total_deaths[,2]),]
top_countries_by_total_deaths[order(+top_countries_by_total_deaths[,2]),]

#Top 20 countries with most total deaths and total cases
TCTDplot <- covid_data %>% 
  group_by(location) %>% 
  filter(location != "World") %>%
  summarize(total_deaths = max(total_deaths)) %>%
  arrange(desc(total_deaths)) %>%
  top_n(20, total_deaths) %>%
  mutate(location = reorder(location, total_deaths)) %>%
  ggplot(aes(location, total_deaths)) +
  geom_bar(stat="identity", color= "red") +
  coord_flip() +
  ggtitle("Top Countries with Most Total Deaths")+
  xlab("Total Deaths") + ylab("Country") +
  theme(plot.title = element_text(hjust = 0.5)) 
TCTDplot

TCTCplot <- covid_data %>% 
  group_by(location) %>% 
  filter(location != "World") %>%
  summarize(total_cases = max(total_cases)) %>%
  arrange(desc(total_cases)) %>%
  top_n(20, total_cases) %>%
  mutate(location = reorder(location, total_cases)) %>%
  ggplot(aes(location, total_cases)) +
  geom_bar(stat="identity", color= "blue") +
  coord_flip() +
  ggtitle("Top Countries with Most Total Cases")+
  xlab("Total Cases") + ylab("Country") +
  theme(plot.title = element_text(hjust = 0.5)) 
TCTCplot
#Comparing the countries with most cases per million and most deaths per million
ggarrange(TCTDplot, TCTCplot)
ggsave('COVID_HistTop_countries_Total.bmp')

#Top countries with most total deaths per million and total cases per million?
TCTDplot <- covid_data %>% 
  group_by(location) %>% 
  summarize(total_deaths_per_million = max(total_deaths_per_million)) %>%
  arrange(desc(total_deaths_per_million)) %>%
  top_n(20, total_deaths_per_million) %>%
  mutate(location = reorder(location, total_deaths_per_million)) %>%
  ggplot(aes(location, total_deaths_per_million)) +
  geom_bar(stat="identity", color= "red") +
  coord_flip() +
  ggtitle("Top Countries with Most Total Deaths per Million")+
  xlab("Total Deaths per Million") + ylab("Country") +
  theme(plot.title = element_text(hjust = 0.5)) 
TCTDplot

TCTCplot <- covid_data %>% 
  group_by(location) %>% 
  summarize(total_cases_per_million = max(total_cases_per_million)) %>%
  arrange(desc(total_cases_per_million)) %>%
  top_n(20, total_cases_per_million) %>%
  mutate(location = reorder(location, total_cases_per_million)) %>%
  ggplot(aes(location, total_cases_per_million)) +
  geom_bar(stat="identity", color= "blue") +
  coord_flip() +
  ggtitle("Top Countries with Most Total Cases per Million")+
  xlab("Total Cases per Million") + ylab("Country") +
  theme(plot.title = element_text(hjust = 0.5)) 
TCTCplot
#Comparing the countries with most cases per million and most deaths per million
ggarrange(TCTDplot, TCTCplot)
ggsave('COVID_HistTop_countries_perMillion.bmp')

#Total Tests (per thousand) people for top Countries with most number of deaths
CV_totaltests <- covid_data %>% group_by(date, location) %>%
  summarize(cum_tests = max(total_tests_per_thousand)) %>% 
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11]) %>%    
  group_by(location)
CV_totaltests<-CV_totaltests[!is.na(CV_totaltests$cum_tests),]
CV_totaltests$cum_tests[CV_totaltests$cum_tests==0] <- NA #avoid plotting the zeros
ggplot(CV_totaltests, aes(date, cum_tests, group= location, color=location))+
  geom_point() + 
  ggtitle("Cumulative Confirmed Tests by Country")+
  xlab("Total Cases per Million") + ylab("Tests by Country") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('CumTests_byCountry.bmp')

#Total confirmed cases vs. confirmed deaths (per million) per country
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] | location == TCTD[12] | location == TCTD[13] | location == TCTD[14] | location == TCTD[15] | location == TCTD[16] | location == TCTD[17] | location == TCTD[18] | location == TCTD[19] | location == TCTD[20]) %>%    group_by(location) %>% 
  mutate(avg_deaths = mean(total_deaths_per_million)) %>% 
  mutate(avg_cases = mean(total_cases_per_million)) %>% 
  ggplot(aes(avg_deaths, avg_cases, group = location, color=location)) +
  geom_point() + 
  geom_text(aes(avg_deaths, avg_cases, label= location), nudge_y = -150) +
  ggtitle("Average Total Confirmed Cases vs. Confirmed Deaths")+
  xlab("Deaths per million")+
  ylab("Cases per million")+ geom_abline(intercept = 0, slope = 20, lty=2) + 
  geom_abline(intercept = 0, slope = 40, lty=2) + 
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('ConfCases_ConfDeaths_byCountry.bmp')

#Progression of New confirmed cases vs. new confirmed deaths (per million) per country
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] | location == TCTD[12] | location == TCTD[13] | location == TCTD[14] | location == TCTD[15] | location == TCTD[16] | location == TCTD[17] | location == TCTD[18] | location == TCTD[19] | location == TCTD[20]) %>%    group_by(location) %>% 
  mutate(avg_newdeaths = mean(new_deaths_per_million)) %>% 
  mutate(avg_newcases = mean(new_cases_per_million)) %>% 
  ggplot(aes(avg_newdeaths, avg_newcases, group = location, color=location)) +
  geom_point() +   
  geom_text(aes(avg_newdeaths, avg_newcases, label= location), nudge_y = -2) +
  ggtitle("Average New Confirmed Cases vs. New Confirmed Deaths")+
  xlab("New Deaths per Million")+
  ylab("New Cases per Million") + geom_abline(intercept = 0, slope = 25, lty=2) + 
  geom_abline(intercept = 0, slope = 36, lty=2) + geom_abline(intercept = 0, slope = 55, lty=2) +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('NewCases_NewDeaths_byCountry.bmp')

#Total tests per thousand vs. confirmed cases (per million) per country
#Remove zeros from these vectors
covid_data$total_cases_per_million[covid_data$total_cases_per_million==0] <- NA
covid_data$total_tests_per_thousand[covid_data$total_tests_per_thousand==0] <- NA
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] ) %>%    
  ggplot(aes(total_cases_per_million, total_tests_per_thousand, group = location, color=location)) +
  geom_point() + geom_smooth(method = 'lm', formula=y~ x + I(x^2) + I(x^3)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(35, 800)) +
  ggtitle("Total Tests per Thousand vs. Total Cases per Million ")+
  xlab("Cases per million")+
  ylab("Test per Thousand")  #+ facet_grid(location ~.)
ggsave('TestsCases_byCountry.bmp')

#BOXPLOT the top 20 countries leading COVID-19
boxplot_countries <- covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] | location == TCTD[12] | location == TCTD[13] | location == TCTD[14] | location == TCTD[15] | location == TCTD[16] | location == TCTD[17] | location == TCTD[18] | location == TCTD[19] | location == TCTD[20]) %>%    
  mutate(location = reorder(location, total_deaths_per_million, FUN = median)) %>%    # reorder
  ggplot(aes(location, total_deaths_per_million, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_jitter(width = 0.1, alpha =0.2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total Deaths per Million ")+
  xlab("Country")+
  ylab("Total Deaths per Million")  
boxplot_countries
ggsave('DeathsMillion_byCountry.bmp')


#3D graph showing TOTAL deaths/cases vs. tests vs. date
x_3D <- covid_data$total_deaths/covid_data$total_cases*100
y_3D <- covid_data$total_tests
z_3D <- covid_data$date
confirmed_deaths_by_country_toDate<- as.Date(covid_data$date, '%m/%d/%Y')
z_3D <- confirmed_deaths_by_country_toDate
require (rgl)
#plot3d(x_3D, y_3D, z_3D, xlim=c(0,30), col = rainbow(1000))
plot3d(x_3D, y_3D, z_3D, xlab = "deaths/cases", ylab = "tests", zlab = "date", xlim=c(0,30), ylim=c(0,max(y_3D)), col = rainbow(1000))


#Tile plot for Positive Rate of COVID
dateVec <- seq(from = as.Date(t0), to = as.Date(tf), by = "days")
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] | location == TCTD[12] | location == TCTD[13] | location == TCTD[14] | location == TCTD[15] | location == TCTD[16] | location == TCTD[17] | location == TCTD[18] | location == TCTD[19] | location == TCTD[20]) %>%    group_by(location) %>% 
  ggplot(aes(date, location, fill= positive_rate)) +
  geom_tile(color = "grey50") +
  scale_x_date(breaks = "2 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0))+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        text=element_text(size = 8)) +
  ggtitle("COVID-19 Positive Rate Top 20 Countries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Date")+
  ylab("Positive Rate") 
ggsave('PositiveRate_byCountry.bmp')

#Tile plot for New Cases per Million of COVID
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] | location == TCTD[12] | location == TCTD[13] | location == TCTD[14] | location == TCTD[15] | location == TCTD[16] | location == TCTD[17] | location == TCTD[18] | location == TCTD[19] | location == TCTD[20]) %>%    group_by(location) %>% 
  ggplot(aes(date, location, fill= new_cases_per_million)) +
  geom_tile(color = "grey50") +
  scale_x_date(breaks = "2 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0))+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        text=element_text(size = 8)) +
  ggtitle("COVID-19 New Cases per Million Top 20 Countries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Date")+
  ylab("New Cases per Million") 
ggsave('NewCasesperMillion_byCountry.bmp')

#Tile plot for Hospitalized per Million of COVID
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] | location == TCTD[12] | location == TCTD[13] | location == TCTD[14] | location == TCTD[15] | location == TCTD[16] | location == TCTD[17] | location == TCTD[18] | location == TCTD[19] | location == TCTD[20]) %>%    group_by(location) %>% 
  ggplot(aes(date, location, fill= hosp_patients_per_million)) +
  geom_tile(color = "grey50") +
  scale_x_date(breaks = "2 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0))+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        text=element_text(size = 8)) +
  ggtitle("COVID-19 Hospitalized Patients per Million Top 20 Countries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Date")+
  ylab("Hospitalized Patients per Million") 
ggsave('Hosp_byCountry.bmp')


#Bubble plot for Top 10 countries Weekly ICU Admissions per Million
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location ==TCTD[11]) %>%  
  group_by(location) %>% 
  ggplot(aes(x=date, y=total_deaths_per_million, size=weekly_icu_admissions_per_million, fill=location)) +
  geom_point(alpha=0.3, shape=21, color="black") +
  scale_size(range = c(.1, 20), name= "Weekly ICU Admissions per Million")+
  scale_x_date(breaks = "4 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Weekly ICU Admissions per Million for Top 20 Countries") +
  xlab("Date")+
  ylab("Weekly ICU Admissions per Million") 
ggsave('WeeklyICU_byCountry.bmp')

#Bubble plot for top 10 countries with positive rate
covid_data %>% group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location ==TCTD[11]) %>%  
  group_by(location) %>% 
  ggplot(aes(x=date, y=total_deaths_per_million, size=positive_rate, fill=location)) +
  geom_point(alpha=0.2, shape=21, color="black") +
  scale_size(range = c(.1, 20), name="Positive Rate")+
  scale_x_date(breaks = "4 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total Deaths Evolution with Positive Rate for Top 20 Countries") +
  xlab("Date")+
  ylab("Total Deaths Per Million") 
ggsave('PositiveRate_bubblePlot_byCountry.bmp')


#Obtain data from: https://covid19.isciii.es/
#Spain
Spain_symptoms <- read_csv("Coronavirus_Symptoms_Spain_Normalized_AgeGroups.csv")
Spain_symptoms$Time <- as.Date(Spain_symptoms$Time, '%m/%d/%Y')
Spain_symptoms <- Spain_symptoms[!is.na(Spain_symptoms$Time),]
t0 <- Spain_symptoms$Time[1] # "2020-03-02"
tf <- "2020-12-31"
dateVec <- seq(from = as.Date(t0), to = as.Date(tf), by = "days")

glimpse(Spain_symptoms)
head(Spain_symptoms)
tail(Spain_symptoms)
dim(Spain_symptoms)
names(Spain_symptoms)
  

#Filter by groups
Spain_agegroups_80 <- Spain_symptoms %>% filter(Characteristic=="<2" | Characteristic=="2-4" | Characteristic=="5-14" | Characteristic=="15-29" | Characteristic=="30-39" | Characteristic=="40-49" | Characteristic=="50-59" | Characteristic=="60-69" | Characteristic=="70-79" | Characteristic==">=80") %>% group_by(Time) 
head(Spain_agegroups_80)
tail(Spain_agegroups_80)
dim(Spain_agegroups_80)
glimpse(Spain_agegroups_80)
Spain_agegroups_80

male <- Spain_agegroups_80 %>% 
  ggplot(aes(Time, as.numeric(Hombres), color=Characteristic)) + 
  geom_point(stat = "identity") + geom_line() +
  ggtitle("Male Cases with COVID-19 in Spain") + scale_y_continuous(limits = c(0,1.8E5)) +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Date")+
  ylab("Male COVID-19 Cases") 
male
female <- Spain_agegroups_80 %>% 
  ggplot(aes(Time, as.numeric(Mujeres), color=Characteristic)) + 
  geom_point(stat = "identity") + geom_line() +
  ggtitle("Female Cases with COVID-19 in Spain") + scale_y_continuous(limits = c(0,1.8E5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Date")+
  ylab("Female COVID-19 Cases") 
female

ggarrange(male, female)

#=================================
#Normalized previous graphs by Age groups by considering the population pyramid of Spain.
#https://www.populationpyramid.net/spain/2019/
male_norm <- Spain_agegroups_80 %>% 
  ggplot(aes(Time, as.numeric(Spain_agegroups_80$Hombres_norm), color=Characteristic)) + 
  geom_point(stat = "identity") + geom_line() +
  ggtitle("% of Male in Spain with COVID-19") + scale_y_continuous(limits = c(0,2.1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Date")+
  ylab("% of Male COVID-19 Cases") 
male_norm
female_norm <- Spain_agegroups_80 %>% 
  ggplot(aes(Time, as.numeric(Spain_agegroups_80$Mujeres_norm), color=Characteristic)) + 
  geom_point(stat = "identity") + geom_line() +
  ggtitle("% of Female in Spain with COVID-19") + scale_y_continuous(limits = c(0,2.1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Date")+
  ylab("% of Female COVID-19 Cases") 
female_norm
ggarrange(male, female, male_norm, female_norm)
ggsave('Spain_male_female_age_evol.bmp')


group_mw1 <- Spain_symptoms %>% filter(Characteristic==">=80") %>% group_by(Time) 
dfm1 <- melt(group_mw1[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm1$value <- as.numeric(dfm1$value)
FM1<- ggplot(dfm1,aes(x = Time,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+
  labs(x = "Date", y="% COVID-19 Confirmed Cases")+
  ggtitle(">=80 years old") + 
  scale_y_continuous(limits = c(0,max(dfm1$value)+0.05*max(dfm1$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw2 <- Spain_symptoms %>% filter(Characteristic=="70-79") %>% group_by(Time) 
dfm2 <- melt(group_mw2[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm2$value <- as.numeric(dfm2$value)
FM2<- ggplot(dfm2,aes(x = Time,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+ 
  labs(x = "Date", y="% COVID-19 Confirmed Cases") +
  ggtitle("70-79 years old") +
  scale_y_continuous(limits = c(0,max(dfm2$value)+0.05*max(dfm2$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw3 <- Spain_symptoms %>% filter(Characteristic=="60-69") %>% group_by(Time)
dfm3 <- melt(group_mw3[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm3
dfm3$value <- as.numeric(dfm3$value)
FM3<- ggplot(dfm3, aes(x=Time, y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Date", y="% COVID-19 Confirmed Cases") + 
  ggtitle("60-69 years old") +
  scale_y_continuous(limits = c(0,max(dfm3$value)+0.05*max(dfm3$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw4 <- Spain_symptoms %>% filter(Characteristic=="50-59") %>% group_by(Time)
dfm4 <- melt(group_mw4[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm4
dfm4$value <- as.numeric(dfm4$value)
FM4<- ggplot(dfm4, aes(x=Time, y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Date", y="% COVID-19 Confirmed Cases") +
  ggtitle("50-59 years old") +
  scale_y_continuous(limits = c(0,max(dfm4$value)+0.05*max(dfm4$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw5 <- Spain_symptoms %>% filter(Characteristic=="40-49") %>% group_by(Time)
dfm5 <- melt(group_mw5[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm5
dfm5$value <- as.numeric(dfm5$value)
FM5<- ggplot(dfm5, aes(x=Time, y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Date", y="% COVID-19 Confirmed Cases") + 
  ggtitle("40-49 years old") +
  scale_y_continuous(limits = c(0,max(dfm5$value)+0.05*max(dfm5$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw6 <- Spain_symptoms %>% filter(Characteristic=="30-39") %>% group_by(Time)
dfm6 <- melt(group_mw6[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm6
dfm6$value <- as.numeric(dfm6$value)
FM6<- ggplot(dfm6, aes(x=Time, y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Date", y="% COVID-19 Confirmed Cases") +
  ggtitle("30-39 years old") +
  scale_y_continuous(limits = c(0,max(dfm6$value)+0.05*max(dfm6$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw7 <- Spain_symptoms %>% filter(Characteristic=="15-29") %>% group_by(Time)
dfm7 <- melt(group_mw7[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm7
dfm7$value <- as.numeric(dfm7$value)
FM7<- ggplot(dfm7, aes(x=Time, y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Date", y="% COVID-19 Confirmed Cases") + 
  ggtitle("15-29 years old") +
  scale_y_continuous(limits = c(0,max(dfm7$value)+0.05*max(dfm7$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw8 <- Spain_symptoms %>% filter(Characteristic=="5-14") %>% group_by(Time)
dfm8 <- melt(group_mw8[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm8
dfm8$value <- as.numeric(dfm8$value)
FM8<- ggplot(dfm8, aes(x=Time, y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Date", y="% COVID-19 Confirmed Cases") +
  ggtitle("5-14 years old") +
  scale_y_continuous(limits = c(0,max(dfm8$value)+0.05*max(dfm8$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

group_mw9 <- Spain_symptoms %>% filter(Characteristic=="2-4") %>% group_by(Time)
dfm9 <- melt(group_mw9[,c('Time','Mujeres_norm','Hombres_norm')],id.vars = 1)
dfm9
dfm9$value <- as.numeric(dfm9$value)
FM9<- ggplot(dfm9, aes(x=Time, y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Date", y="% COVID-19 Confirmed Cases") +
  ggtitle("2-4 years old") +
  scale_y_continuous(limits = c(0,max(dfm9$value)+0.05*max(dfm9$value))) + 
  #scale_x_date(breaks = "1 month", limits = c(t0, max = tf), expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

#Arrange all plots for all age male and female groups
FM_Allgroups <- ggarrange(FM1, FM2, FM3, FM4, FM5, FM6, FM7, FM8, FM9)
FM_Allgroups
ggsave('Spain_ALLgroupsHist.bmp')

#Analyze Spanish Autonomous Comunities
Spain_comunidades <- read_csv("CoronavirusSPAIN.csv")
Spain_comunidades$FECHA <- as.Date(Spain_comunidades$FECHA, '%d/%m/%Y')  #<----note the order (day/month/year)
glimpse(Spain_comunidades)
head(Spain_comunidades)
tail(Spain_comunidades)
dim(Spain_comunidades)
names(Spain_comunidades)
length(Spain_comunidades$Fallecidos)
length(Spain_comunidades$FECHA)

#Remove NA for theses columns
Spain_comunidades<- Spain_comunidades[!is.na(Spain_comunidades$Fallecidos),]
Spain_comunidades <- Spain_comunidades[!is.na(Spain_comunidades$Hospitalizados),]
#Spain_comunidades <- Spain_comunidades[!is.na(Spain_comunidades$Recuperados),]
Spain_comunidades <- Spain_comunidades[!is.na(Spain_comunidades$UCI),]
Spain_comunidades<- Spain_comunidades[!is.na(Spain_comunidades$New_Fallecidos),]

#Correlation between CCAA in Spain
correlation_vec_CCAA <- t(cor(Spain_comunidades[c( "CASOS", "Hospitalizados", "UCI", "Fallecidos")], 
                         Spain_comunidades[c( "CASOS", "Hospitalizados", "UCI", "Fallecidos")]))
correlation_CCAA <- data.frame(correlation_vec_CCAA[order(-correlation_vec_CCAA[,1]),])
ggcorr(correlation_CCAA, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")
ggsave('CCAA_Spain_Correlation.bmp')
corrplot::corrplot(correlation_vec_CCAA) 
#ggsave('CCAA_Spain_Correlation2.bmp')

#Initial fit
Spain_comunidades_fallecidos <- Spain_comunidades %>% 
  ggplot(aes(FECHA, as.numeric(Fallecidos), color=CCAA)) + 
  geom_point(stat = "identity") + 
  geom_smooth(method = 'loess') +
  ggtitle("Cumulative Deaths per Region in Spain") +
  scale_x_date(breaks = "4 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0))+ labs(x = "Date", y="Deaths") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))
Spain_comunidades_fallecidos


#Check: Example for Madrid
# Estimating averages and standard deviations
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(Spain_comunidades$Fallecidos, times = 1, p = 0.1, list = FALSE)
test_set <- Spain_comunidades[test_index, ]
train_set <- Spain_comunidades[-test_index, ]
#or
train_set <- Spain_comunidades %>% slice(-test_index)
test_set <- Spain_comunidades %>% slice(test_index)
params <- Spain_comunidades %>%
  group_by(CCAA) %>%
  summarize(avg = mean(Fallecidos), sd = sd(Fallecidos))
params
# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(Fallecidos=="1")) %>% pull(pi)
pi
# Getting an actual rule
x <- test_set$Fallecidos
f0 <- dnorm(x, params$avg[3], params$sd[3])
f1 <- dnorm(x, params$avg[2], params$sd[2])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))
#Plot looks like a logistic regression estimate
plot(x,p_hat_bayes)
# Can we find a polynome that fit this function ?
model <- lm(p_hat_bayes ~ x + I(x^2) + I(x^3) + I(x^4) )
summary(model)
# For each value of x, we can get the value of "y" estimated by the model, then add it to the plot
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 )  
# I add the features of the model to the plot
coeff <- round(model$coefficients , 2)
text(4000, 0.3 , paste("Model : ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))


Spain_comunidades_hospitalizados <- Spain_comunidades %>% 
  ggplot(aes(FECHA, as.numeric(Hospitalizados), color=CCAA)) + 
  geom_point(stat = "identity") + 
  geom_smooth(method = 'loess') +
  ggtitle("Cumulative Hospitalized per Region in Spain") +
  scale_x_date(breaks = "4 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0))+  labs(x = "Date", y="Hospitalized") +
  labs(x = "Date", y="Hospitalized") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

Spain_comunidades_UCI <- Spain_comunidades %>% 
  ggplot(aes(FECHA, as.numeric(UCI), color=CCAA)) + 
  geom_point(stat = "identity") + 
  geom_smooth(method = 'loess') +
  ggtitle("Cumulative UCI per Region in Spain") +
  scale_x_date(breaks = "4 weeks", limits = c(min(dateVec), max = max(dateVec)),
               expand=c(0,0))+ labs(x = "Date", y="UCI") +
  labs(x = "Date", y="Hospitalized") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggarrange(Spain_comunidades_fallecidos,Spain_comunidades_hospitalizados,Spain_comunidades_UCI)
ggsave('CCAA_Spain_HospUCIdeaths.bmp')


#BOXPLOT hospitalized, UCI and Deaths due to COVID-19 for all the CCAA in Spain
boxplot_CCAA <- Spain_comunidades %>% group_by(Fallecidos) %>%
  group_by(CCAA) %>% mutate(total_casos= Fallecidos) %>%
  mutate(CCAA = reorder(CCAA, total_casos, FUN = median)) %>%    # reorder
  ggplot(aes(CCAA, total_casos, fill = CCAA)) +    # color by continent
  geom_boxplot() + coord_cartesian(ylim = c(0, 4500)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Cumulative Deaths in Spain Autonomous Communities") +
  labs(x = "CCAA", y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_jitter(width = 0.1, alpha =0.2)
boxplot_CCAA 
ggsave('Spain_boxPlotCCAA_zoom.bmp')


CCAA_Fallecidos <- Spain_comunidades %>% 
  group_by(CCAA) %>% 
  summarize(cum_Fallecidos = max(Fallecidos)) %>% 
  arrange(desc(cum_Fallecidos)) %>%
  top_n(20, cum_Fallecidos)
CCAA_Fallecidos
sum(CCAA_Fallecidos$cum_Fallecidos)
class(CCAA_Fallecidos)
CCAA_Fallecidos_hist <- data.frame(CCAA_Fallecidos)
hist(CCAA_Fallecidos_hist[[2]])


CCAA_New_Fallecidos <- Spain_comunidades %>% 
  group_by(CCAA) %>% 
  summarize(cum_New_Fallecidos = max(New_Fallecidos)) %>% 
  arrange(desc(cum_New_Fallecidos)) %>%
  top_n(20, cum_New_Fallecidos)
CCAA_New_Fallecidos
sum(CCAA_New_Fallecidos$cum_New_Fallecidos)
sum(Spain_comunidades$New_Fallecidos)

#Preliminary analysis for Madrid and Catalonia
#Example: Madrid and Catalonia deaths evolution
MD <- Spain_comunidades %>% filter(CCAA=="MD")
x<- as.numeric(MD$FECHA)
y<- MD$Fallecidos
plot(x,y,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3, 
     xlab="Date (numeric)", ylab="Madrid Fallecidos" )
title("Evolution of Deaths in Madrid due to COVID-19")
# Can we find a polynome that fit this function ?
model <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) )
summary(model)
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col="purple", lwd=2 )
coeff <- round(model$coefficients , 2)
text(18500, 6000 , paste("Model: ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "+" , coeff[5] , "*x^4" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))
#ggsave('Madrid_DeathsEvol.bmp')

CT <- Spain_comunidades %>% filter(CCAA=="CT")
x<- as.numeric(CT$FECHA)
y<- CT$Fallecidos
plot(x,y,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3, 
     xlab="Date (numeric)", ylab="Madrid Fallecidos" )
title("Evolution of Deaths in Catalonia due to COVID-19")
# Can we find a polynome that fit this function ?
model <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) )
summary(model)
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col="dark green", lwd=2 )
coeff <- round(model$coefficients , 2)
text(18500, 4000 , paste("Model: ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "+" , coeff[5] , "*x^4" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))
#ggsave('Catalonia_DeathsEvol.bmp')


#Check evolution of COVID New Deaths with time in Spain
#Test other specific polynomals
#Remove first two weeks of data
test.formula <- y ~ poly(x,12)
Spain_comunidades %>% filter(FECHA>"2020-03-15") %>%
  mutate(FECHA = round_date(FECHA, unit = "days")) %>%
  group_by(FECHA) %>%
  summarize(New_Fallecidos = max(New_Fallecidos)) %>%
  ggplot(aes(FECHA, New_Fallecidos)) +
  geom_point() +
  geom_smooth(method = 'lm', formula= test.formula, color='dark blue' ) +
  stat_poly_eq(formula= test.formula,
               aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")), 
               parse = TRUE) +
  theme_bw() +
  labs(x="Date (year) ", y="New Fallecidos", title = "Maximum New Fallecidos in Spain (Mar-Dec) 2020") +
  scale_x_date(date_breaks = "2 weeks") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Spain_CurveTimeEvol_New_Fallecidos.bmp')

#Check evolution of COVID Deaths with time in Spain
#Test other specific polynomals
#Remove first two weeks of data
test.formula <- y ~ poly(x, 10 )
Spain_comunidades %>% filter(FECHA>"2020-03-15") %>%
  mutate(FECHA = round_date(FECHA, unit = "weeks")) %>%
  group_by(FECHA) %>%
  summarize(Fallecidos = mean(Fallecidos)) %>%
  ggplot(aes(FECHA, Fallecidos)) +
  geom_point() +
  geom_smooth(method = 'lm', formula= test.formula, color='dark blue' ) +
  stat_poly_eq(formula= test.formula,
               aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")), 
               parse = TRUE) +
  theme_bw() +
  labs(x="Date (year) ", y="Fallecidos", title = "Average Fallecidos in Spain (Mar-Dec) 2020") +
  scale_x_date(date_breaks = "2 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Spain_CurveTimeEvol_Fallecidos.bmp')


#Symptoms, total count, men and women.
Spain_symptoms_diseases <- Spain_symptoms %>% filter(Characteristic=="Diabetes" | Characteristic=="Diarrea" | Characteristic=="Disnea" | Characteristic=="Dolor de garganta" | Characteristic=="Enfermedad cardiovascular" | Characteristic=="Enfermedad respiratoria" | Characteristic=="Fallo renal agudo" | Characteristic=="Fiebre" | Characteristic=="Hipertension arterial" | Characteristic=="Neumonia" | Characteristic=="Tos" | Characteristic=="Vomitos" | Characteristic=="Escalofrios") %>% group_by(Time) 

plt_diseases_total <- Spain_symptoms_diseases %>% group_by(Total) %>%
  ggplot(aes(Time, as.numeric(Total), color=Characteristic)) + 
  geom_point(stat = "identity") + geom_line() +
  ggtitle("Overall Count in Spain") +
  labs(x = "Date", y="COVID Symptoms") +
  theme(plot.title = element_text(hjust = 0.5))
plt_diseases_total

plt_diseases_Hombres <- Spain_symptoms_diseases %>% 
  ggplot(aes(Time, as.numeric(Hombres), color=Characteristic)) + 
  geom_point(stat = "identity") + geom_line() +
  ggtitle("Total Male Count in Spain") +
  labs(x = "Date", y="COVID Symptoms") +
  theme(plot.title = element_text(hjust = 0.5))
plt_diseases_Hombres

plt_diseases_Mujeres <- Spain_symptoms_diseases %>% 
  ggplot(aes(Time, as.numeric(Mujeres), color=Characteristic)) + 
  geom_point(stat = "identity") + geom_line() +
  ggtitle("Total Female Count in Spain") +
  labs(x = "Date", y="COVID Symptoms") +
  theme(plot.title = element_text(hjust = 0.5))
plt_diseases_Mujeres

ggarrange(plt_diseases_total, plt_diseases_Hombres, plt_diseases_Mujeres)
ggsave('Spain_Symptoms.bmp')


#===============================================================================
#Machine Learning Application for Spain
# Validation set will be 10% of covid data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = Spain_comunidades$Fallecidos, times = 1, p = 0.1, list = FALSE)
#Define train set and test set:
covid_train_set <- Spain_comunidades[-test_index,] #train set
covid_test_set <- Spain_comunidades[test_index,] #test set

names(Spain_comunidades)
# Make sure needed variables are in validation set are also in covid data set for Spain case
validation <- covid_test_set %>%
  semi_join(covid_train_set, by = "Fallecidos" ) 

# Add rows removed from validation set back into covid data set
removed <- anti_join(covid_test_set, validation)
covid_train_set <- rbind(covid_train_set, removed)
head(covid_train_set)

#Explore the data a bit more for train set and validation (test) set:
summary(covid_train_set)
#for validation
summary(validation)

#Run Models:
#Apply simple model to the train set:
mu_hat <- mean(covid_train_set$Fallecidos)
mu_hat
#Obtain a naive RMSE for test set:
naive_rmse <- RMSE(covid_test_set$Fallecidos, mu_hat)
naive_rmse
#Note this naive RMSE is different than that one for the validation
naive_rmse <- RMSE(validation$Fallecidos, mu_hat)
naive_rmse
rmse_results <- tibble(method = "Just the average", 
                       RMSE = naive_rmse)
rmse_results


#===================Model 1===============================
#Analyze the "CCAA" effect using the training set
#Average
mu <- mean(Spain_comunidades$Fallecidos) 
#Fallecidos (deaths) averages
CCAA_avgs <- Spain_comunidades %>% 
  group_by(CCAA) %>% 
  summarize(b_c = mean(Fallecidos - mu))
CCAA_avgs
#Plot the estimated death averages
CCAA_avgs %>% qplot(b_c, geom ="histogram", bins = 30, data = ., color = I("blue")) +
  labs(x="Least Square Estimate, b_c", y="CCAA Averages", title="CCAA Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("CCAA_effect.bmp")

#Predict the number of deaths in this simple model:
predicted_deaths <- mu + validation %>% 
  left_join(CCAA_avgs, by='CCAA') %>%
  .$b_c
predicted_deaths
#Obtain the RMSE for model 1:
model_1_rmse <- RMSE(predicted_deaths, validation$Fallecidos)
#Display the RMSE results for model 1:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="CCAA Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results
#Provide RMSE results for model 1 in the form of a table
rmse_results %>% knitr::kable()
rmse_results$RMSE[1]-rmse_results$RMSE[2]

#====================Model 2======================================
#Analyze the "Hospitalizados" effect using the training set:
Spain_comunidades %>% 
  group_by(Hospitalizados) %>% 
  summarize(b_h = mean(Fallecidos)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_h)) + 
  geom_histogram(bins = 30, color = "red") +
  labs(x="Least Square Estimate, b_h", y="Hospitalizaded Averages", title="Hospitalized Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Hopitalized_effect.bmp")

#User averages on the training set
hosp_avgs <- Spain_comunidades %>%
  left_join(CCAA_avgs, by='CCAA') %>%
  group_by(Hospitalizados) %>%
  summarize(b_h = mean(Fallecidos - mu - b_c))
hosp_avgs
#Obtain predicted deaths using the test set
predicted_deaths <- validation %>% 
  left_join(CCAA_avgs, by='CCAA') %>%
  left_join(hosp_avgs, by='Hospitalizados') %>%
  mutate(pred = mu + b_c + b_h) %>%
  .$pred
#Obtain the RMSE for model 2:
model_2_rmse <- RMSE(predicted_deaths, validation$Fallecidos)
#Display the RMSE results for model 2:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="CCAA + Hospitalizados Effects Model",  
                                     RMSE = model_2_rmse ))
#Provide RMSE results for model 2 in the form of a table
rmse_results %>% knitr::kable()
rmse_results$RMSE[2]-rmse_results$RMSE[3]

#====================Model 3=======================
#Analyze the "UCI" effect using the training set:
Spain_comunidades %>% 
  group_by(UCI) %>% 
  summarize(b_u = mean(Fallecidos)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "green") +
  labs(x="Least Square Estimate, b_u", y="UCI Averages", title="UCI Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("UCI_effect.bmp")

#User averages on the training set
UCI_avgs <- Spain_comunidades %>% 
  left_join(CCAA_avgs, by='CCAA') %>%
  left_join(hosp_avgs, by='Hospitalizados') %>%
  group_by(UCI) %>%
  summarize(b_u = mean(Fallecidos - mu - b_c - b_h ))
UCI_avgs
#Obtain predicted deaths using the test set
predicted_deaths <- validation %>% 
  left_join(CCAA_avgs, by='CCAA') %>%
  left_join(hosp_avgs, by='Hospitalizados') %>%
  left_join(UCI_avgs, by='UCI') %>%
  mutate(pred = mu + b_c + b_h + b_u) %>%
  .$pred
#Obtain the RMSE for model 3:
model_3_rmse <- RMSE(predicted_deaths, validation$Fallecidos)
#Display the RMSE results for model 3:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="CCAA + Hospitalizados + UCI Effects Model",  
                                     RMSE = model_3_rmse ))
#Provide RMSE results for model 3 in the form of a table
rmse_results %>% knitr::kable()
rmse_results$RMSE[3]-rmse_results$RMSE[4]

#====================Model 4==============================
#Analyze the "FECHA" effect using the training set:
Spain_comunidades %>% 
  group_by(FECHA) %>% 
  summarize(b_f = mean(Fallecidos)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_f)) + 
  geom_histogram(bins = 30, color = "orange") +
  labs(x="Least Square Estimate, b_f", y="Averages", title="FECHA Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("FECHA_effect.bmp")
#Obtain averages using training set 
fecha_avgs <- Spain_comunidades %>% 
  left_join(CCAA_avgs, by='CCAA') %>%
  left_join(hosp_avgs, by='Hospitalizados') %>%
  left_join(UCI_avgs, by='UCI') %>%
  mutate(FECHA = round_date(as_datetime(FECHA), unit = "week")) %>%
  group_by(FECHA) %>%
  summarize(b_f = mean(Fallecidos - mu - b_c - b_h - b_u))
fecha_avgs
#Obtain predicted deaths using the test set
predicted_deaths <- validation %>% 
  left_join(CCAA_avgs, by='CCAA') %>%
  left_join(hosp_avgs, by='Hospitalizados') %>%
  left_join(UCI_avgs, by='UCI') %>%
  mutate(FECHA = round_date(as_datetime(FECHA), unit = "week")) %>%
  left_join(fecha_avgs, by='FECHA') %>%
  mutate(pred = mu + b_c + b_h + b_u + b_f) %>%
  .$pred
#Obtain the RMSE for model 4:
model_4_rmse <- RMSE(predicted_deaths, validation$Fallecidos)
#Display the RMSE results for model 4:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="CCAA + Hospitalizados + UCI + FECHA Effects Model",  
                                     RMSE = model_4_rmse ))
#Provide RMSE results for model 4 in the form of a table
rmse_results %>% knitr::kable()
rmse_results$RMSE[4]-rmse_results$RMSE[5]


#==========================Regularization============================
#Regularization: Spain 
#Regularization permits us to penalize large estimates that
#come from small sample sizes. 
CCAA_names <- Spain_comunidades %>% 
  filter(!is.na(Fallecidos)) %>% #check for any NA name
  select(CCAA) %>%
  distinct()
CCAA_names

lambdas <- seq(0, 2, 0.05)
rmses <- sapply(lambdas, function(l){
  mu <- mean(Spain_comunidades$Fallecidos)
  b_c <- Spain_comunidades %>%#CCAA
    group_by(CCAA) %>%
    summarize(b_c = sum(Fallecidos - mu)/(n()+l))
  b_h<- Spain_comunidades %>%#Hospitalized
    left_join(b_c, by="CCAA") %>%
    group_by(Hospitalizados) %>%
    summarize(b_h = sum(Fallecidos - b_c - mu)/(n()+l))
  b_u <- Spain_comunidades %>%#UCI
    left_join(b_c, by="CCAA") %>%
    left_join(b_h, by="Hospitalizados") %>%
    group_by(UCI) %>%
    summarize(b_u = sum(Fallecidos - b_c - b_h - mu)/(n()+l))
  b_f <- Spain_comunidades %>%#FECHA
    left_join(b_c, by="CCAA") %>%
    left_join(b_h, by="Hospitalizados") %>%
    left_join(b_u, by="UCI") %>%
    group_by(FECHA) %>%
    summarize(b_f = sum(Fallecidos - b_c - b_h - b_u - mu)/(n()+l))
  predicted_deaths <- 
    validation %>% 
    left_join(b_c, by = "CCAA") %>%
    left_join(b_h, by = "Hospitalizados") %>%
    left_join(b_u, by = "UCI") %>%
    left_join(b_f, by = "FECHA") %>%
    mutate(pred = mu + b_c + b_h + b_u + b_f) %>%
    .$pred
  return(RMSE(predicted_deaths, validation$Fallecidos))
})
#Plot the RMSES vs. the Lambdas
qplot(lambdas, rmses) + 
  labs(x="Lambdas", y="RMSES", title="Regularization: CCAA + Hospitalizados + UCI + FECHA") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Lambda_covid.bmp")
#Select the lambda that minimizes the RMSE
lambda <- lambdas[which.min(rmses)]
lambda
#RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized CCAA + Hospitalizados + UCI + FECHA Effects Model",  
                                     RMSE = min(rmses)))
#RMSE results in form of table
rmse_results %>% knitr::kable()
rmse_results$RMSE[5]-rmse_results$RMSE[6]

#Save the RMSE results as a file
write.table(rmse_results, file = "RMSE_Covid_Spain.csv",
            sep = "\t", row.names = F)


#=====================================================================================
#Setting up Europe data for machine learning application
#Get COVID data for Europe
covid_data_Europe <- covid_data %>% filter(continent == "Europe") 

nrow(covid_data_Europe)
EuropeanCountries <- covid_data_Europe$location
EuropeanCountries <- as.data.frame(EuropeanCountries)
#See frequency table of all European countries
table(EuropeanCountries)
EC_dataframe <- as.data.frame(table(EuropeanCountries))
EC_dataframe

#European countries with most number of deaths
EUcountries_by_total_deaths <- covid_data_Europe %>%
  group_by(location) %>% 
  summarize(cum_deaths = max(total_deaths)) %>%
  arrange(desc(cum_deaths)) %>%
  top_n(46, cum_deaths)
EUcountries_by_total_deaths
#Total number of deaths
sum(EUcountries_by_total_deaths$cum_deaths)


n <- nrow(EC_dataframe)
countries <-
  sort(c(rep("Albania", EC_dataframe$Freq[1]),
         rep("Andorra", EC_dataframe$Freq[2]),
         rep("Austria", EC_dataframe$Freq[3]),
         rep("Belarus", EC_dataframe$Freq[4]),
         rep("Belgium", EC_dataframe$Freq[5]),
         rep("Bosnia and Herzegovina", EC_dataframe$Freq[6]),
         rep("Bulgaria", EC_dataframe$Freq[7]),
         rep("Croatia", EC_dataframe$Freq[8]),
         rep("Cyprus", EC_dataframe$Freq[9]),
         rep("Czech Republic", EC_dataframe$Freq[10]),
         rep("Denmark", EC_dataframe$Freq[11]),
         rep("Estonia", EC_dataframe$Freq[12]),
         rep("Finland", EC_dataframe$Freq[13]),
         rep("France", EC_dataframe$Freq[14]),
         rep("Germany", EC_dataframe$Freq[15]),
         rep("Greece", EC_dataframe$Freq[16]),
         rep("Hungary", EC_dataframe$Freq[17]),
         rep("Iceland", EC_dataframe$Freq[18]),
         rep("Ireland", EC_dataframe$Freq[19]),
         rep("Italy", EC_dataframe$Freq[20]),
         rep("Kosovo", EC_dataframe$Freq[21]),
         rep("Latvia", EC_dataframe$Freq[22]),
         rep("Liechtenstein", EC_dataframe$Freq[23]),
         rep("Lithuania", EC_dataframe$Freq[24]),
         rep("Luxembourg", EC_dataframe$Freq[25]),
         rep("Macedonia", EC_dataframe$Freq[26]),
         rep("Malta", EC_dataframe$Freq[27]),
         rep("Moldova", EC_dataframe$Freq[28]),
         rep("Monaco", EC_dataframe$Freq[29]),
         rep("Montenegro", EC_dataframe$Freq[30]),
         rep("Netherlands", EC_dataframe$Freq[31]),
         rep("Norway", EC_dataframe$Freq[32]),
         rep("Poland", EC_dataframe$Freq[33]),
         rep("Portugal  ", EC_dataframe$Freq[34]),
         rep("Romania  ", EC_dataframe$Freq[35]),
         rep("Russia  ", EC_dataframe$Freq[36]),
         rep("San Marino  ", EC_dataframe$Freq[37]),
         rep("Serbia  ", EC_dataframe$Freq[38]),
         rep("Slovakia  ", EC_dataframe$Freq[39]),
         rep("Slovenia  ", EC_dataframe$Freq[40]),
         rep("Spain  ", EC_dataframe$Freq[41]),
         rep("Sweden", EC_dataframe$Freq[42]),
         rep("Switzerland", EC_dataframe$Freq[43]),
         rep("Ukraine", EC_dataframe$Freq[44]),
         rep("United Kingdom", EC_dataframe$Freq[45]),
         rep("Vatican", EC_dataframe$Freq[46])
  ))
#countries
class(countries)
#Get lat lon for each country
#https://developers.google.com/public-data/docs/canonical/countries_csv
#Make a new csv file with these latitudes and longitudes, store file
latlongEU_countries <- read_csv("LatLonEUcountries.csv", guess_max = 100000)
nrow(latlongEU_countries)
#indexNA <- which(is.na(latlongEU_countries$latitude))
#latlongEU_countries$latitude[indexNA]<-0
#latlongEU_countries$longitude[indexNA]<-0

index <- which(latlongEU_countries$name == "Albania")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n1 <- EC_dataframe$Freq[1]
lat <- rep(c_lat, n1)
lon <- rep(c_lon, n1)
c1 <- as.data.frame(cbind("Albania", lat, lon))

index <- which(latlongEU_countries$name == "Andorra")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n2 <- EC_dataframe$Freq[2]
lat <- rep(c_lat, n2)
lon <- rep(c_lon, n2)
c2 <- as.data.frame(cbind("Andorra", lat, lon))

index <- which(latlongEU_countries$name == "Austria")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n3 <- EC_dataframe$Freq[3]
lat <- rep(c_lat, n3)
lon <- rep(c_lon, n3)
c3 <- as.data.frame(cbind("Austria", lat, lon))

index <- which(latlongEU_countries$name == "Belarus")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n4 <- EC_dataframe$Freq[4]
lat <- rep(c_lat, n4)
lon <- rep(c_lon, n4)
c4 <- as.data.frame(cbind("Belarus", lat, lon))

index <- which(latlongEU_countries$name == "Belgium")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n5 <- EC_dataframe$Freq[5]
lat <- rep(c_lat, n5)
lon <- rep(c_lon, n5)
c5 <- as.data.frame(cbind("Belgium", lat, lon))

index <- which(latlongEU_countries$name == "Bosnia and Herzegovina")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n6 <- EC_dataframe$Freq[6]
lat <- rep(c_lat, n6)
lon <- rep(c_lon, n6)
c6 <- as.data.frame(cbind("Bosnia and Herzegovina", lat, lon))

index <- which(latlongEU_countries$name == "Bulgaria")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n7 <- EC_dataframe$Freq[7]
lat <- rep(c_lat, n7)
lon <- rep(c_lon, n7)
c7 <- as.data.frame(cbind("Bulgaria", lat, lon))

index <- which(latlongEU_countries$name == "Croatia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n8 <- EC_dataframe$Freq[8]
lat <- rep(c_lat, n8)
lon <- rep(c_lon, n8)
c8 <- as.data.frame(cbind("Croatia", lat, lon))

index <- which(latlongEU_countries$name == "Cyprus")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n9 <- EC_dataframe$Freq[9]
lat <- rep(c_lat, n9)
lon <- rep(c_lon, n9)
c9 <- as.data.frame(cbind("Cyprus", lat, lon))


index <- which(latlongEU_countries$name == "Czech Republic")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n10 <- EC_dataframe$Freq[10]
lat <- rep(c_lat, n10)
lon <- rep(c_lon, n10)
c10 <- as.data.frame(cbind("Czech Republic", lat, lon))


index <- which(latlongEU_countries$name == "Denmark")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n11 <- EC_dataframe$Freq[11]
lat <- rep(c_lat, n11)
lon <- rep(c_lon, n11)
c11 <- as.data.frame(cbind("Denmark", lat, lon))

index <- which(latlongEU_countries$name == "Estonia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n12 <- EC_dataframe$Freq[12]
lat <- rep(c_lat, n12)
lon <- rep(c_lon, n12)
c12 <- as.data.frame(cbind("Estonia", lat, lon))

index <- which(latlongEU_countries$name == "Finland")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n13<- EC_dataframe$Freq[13]
lat <- rep(c_lat, n13)
lon <- rep(c_lon, n13)
c13 <- as.data.frame(cbind("Finland", lat, lon))

index <- which(latlongEU_countries$name == "France")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n14 <- EC_dataframe$Freq[14]
lat <- rep(c_lat, n14)
lon <- rep(c_lon, n14)
c14 <- as.data.frame(cbind("France", lat, lon))

index <- which(latlongEU_countries$name == "Germany")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n15 <- EC_dataframe$Freq[15]
lat <- rep(c_lat, n15)
lon <- rep(c_lon, n15)
c15 <- as.data.frame(cbind("Germany", lat, lon))

index <- which(latlongEU_countries$name == "Greece")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n16 <- EC_dataframe$Freq[16]
lat <- rep(c_lat, n16)
lon <- rep(c_lon, n16)
c16 <- as.data.frame(cbind("Greece", lat, lon))

index <- which(latlongEU_countries$name == "Hungary")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n17 <- EC_dataframe$Freq[17]
lat <- rep(c_lat, n17)
lon <- rep(c_lon, n17)
c17 <- as.data.frame(cbind("Hungary", lat, lon))

index <- which(latlongEU_countries$name == "Iceland")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n18 <- EC_dataframe$Freq[18]
lat <- rep(c_lat, n18)
lon <- rep(c_lon, n18)
c18 <- as.data.frame(cbind("Iceland", lat, lon))

index <- which(latlongEU_countries$name == "Ireland")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n19 <- EC_dataframe$Freq[19]
lat <- rep(c_lat, n19)
lon <- rep(c_lon, n19)
c19 <- as.data.frame(cbind("Ireland", lat, lon))

index <- which(latlongEU_countries$name == "Italy")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n20 <- EC_dataframe$Freq[20]
lat <- rep(c_lat, n20)
lon <- rep(c_lon, n20)
c20 <- as.data.frame(cbind("Italy", lat, lon))

index <- which(latlongEU_countries$name == "Kosovo")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n21 <- EC_dataframe$Freq[21]
lat <- rep(c_lat, n21)
lon <- rep(c_lon, n21)
c21 <- as.data.frame(cbind("Kosovo", lat, lon))

index <- which(latlongEU_countries$name == "Latvia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n22 <- EC_dataframe$Freq[22]
lat <- rep(c_lat, n22)
lon <- rep(c_lon, n22)
c22 <- as.data.frame(cbind("Latvia", lat, lon))

index <- which(latlongEU_countries$name == "Liechtenstein")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n23 <- EC_dataframe$Freq[23]
lat <- rep(c_lat, n23)
lon <- rep(c_lon, n23)
c23 <- as.data.frame(cbind("Liechtenstein", lat, lon))

index <- which(latlongEU_countries$name == "Lithuania")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n24 <- EC_dataframe$Freq[24]
lat <- rep(c_lat, n24)
lon <- rep(c_lon, n24)
c24 <- as.data.frame(cbind("Lithuania", lat, lon))

index <- which(latlongEU_countries$name == "Luxembourg")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n25 <- EC_dataframe$Freq[25]
lat <- rep(c_lat, n25)
lon <- rep(c_lon, n25)
c25 <- as.data.frame(cbind("Luxembourg", lat, lon))

index <- which(latlongEU_countries$name == "Macedonia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n26 <- EC_dataframe$Freq[26]
lat <- rep(c_lat, n26)
lon <- rep(c_lon, n26)
c26 <- as.data.frame(cbind("Macedonia", lat, lon))

index <- which(latlongEU_countries$name == "Malta")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n27 <- EC_dataframe$Freq[27]
lat <- rep(c_lat, n27)
lon <- rep(c_lon, n27)
c27 <- as.data.frame(cbind("Malta", lat, lon))

index <- which(latlongEU_countries$name == "Moldova")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n28 <- EC_dataframe$Freq[28]
lat <- rep(c_lat, n28)
lon <- rep(c_lon, n28)
c28 <- as.data.frame(cbind("Moldova", lat, lon))

index <- which(latlongEU_countries$name == "Monaco")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n29 <- EC_dataframe$Freq[29]
lat <- rep(c_lat, n29)
lon <- rep(c_lon, n29)
c29 <- as.data.frame(cbind("Monaco", lat, lon))

index <- which(latlongEU_countries$name == "Montenegro")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n30 <- EC_dataframe$Freq[30]
lat <- rep(c_lat, n30)
lon <- rep(c_lon, n30)
c30 <- as.data.frame(cbind("Montenegro", lat, lon))

index <- which(latlongEU_countries$name == "Netherlands")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n31 <- EC_dataframe$Freq[31]
lat <- rep(c_lat, n31)
lon <- rep(c_lon, n31)
c31 <- as.data.frame(cbind("Netherlands", lat, lon))

index <- which(latlongEU_countries$name == "Norway")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n32 <- EC_dataframe$Freq[32]
lat <- rep(c_lat, n32)
lon <- rep(c_lon, n32)
c32 <- as.data.frame(cbind("Norway", lat, lon))

index <- which(latlongEU_countries$name == "Poland")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n33 <- EC_dataframe$Freq[33]
lat <- rep(c_lat, n33)
lon <- rep(c_lon, n33)
c33 <- as.data.frame(cbind("Poland", lat, lon))

index <- which(latlongEU_countries$name == "Portugal")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n34 <- EC_dataframe$Freq[34]
lat <- rep(c_lat, n34)
lon <- rep(c_lon, n34)
c34 <- as.data.frame(cbind("Portugal", lat, lon))

index <- which(latlongEU_countries$name == "Romania")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n35 <- EC_dataframe$Freq[35]
lat <- rep(c_lat, n35)
lon <- rep(c_lon, n35)
c35 <- as.data.frame(cbind("Romania", lat, lon))

index <- which(latlongEU_countries$name == "Russia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n36 <- EC_dataframe$Freq[36]
lat <- rep(c_lat, n36)
lon <- rep(c_lon, n36)
c36 <- as.data.frame(cbind("Russia", lat, lon))

index <- which(latlongEU_countries$name == "San Marino")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n37 <- EC_dataframe$Freq[37]
lat <- rep(c_lat, n37)
lon <- rep(c_lon, n37)
c37 <- as.data.frame(cbind("San Marino", lat, lon))

index <- which(latlongEU_countries$name == "Serbia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n38 <- EC_dataframe$Freq[38]
lat <- rep(c_lat, n38)
lon <- rep(c_lon, n38)
c38 <- as.data.frame(cbind("Serbia", lat, lon))

index <- which(latlongEU_countries$name == "Slovakia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n39 <- EC_dataframe$Freq[39]
lat <- rep(c_lat, n39)
lon <- rep(c_lon, n39)
c39 <- as.data.frame(cbind("Slovakia", lat, lon))

index <- which(latlongEU_countries$name == "Slovenia")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n40 <- EC_dataframe$Freq[40]
lat <- rep(c_lat, n40)
lon <- rep(c_lon, n40)
c40 <- as.data.frame(cbind("Slovenia", lat, lon))

index <- which(latlongEU_countries$name == "Spain")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n41 <- EC_dataframe$Freq[41]
lat <- rep(c_lat, n41)
lon <- rep(c_lon, n41)
c41 <- as.data.frame(cbind("Spain", lat, lon))

index <- which(latlongEU_countries$name == "Sweden")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n42 <- EC_dataframe$Freq[42]
lat <- rep(c_lat, n42)
lon <- rep(c_lon, n42)
c42 <- as.data.frame(cbind("Sweden", lat, lon))

index <- which(latlongEU_countries$name == "Switzerland")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n43 <- EC_dataframe$Freq[43]
lat <- rep(c_lat, n43)
lon <- rep(c_lon, n43)
c43 <- as.data.frame(cbind("Switzerland", lat, lon))

index <- which(latlongEU_countries$name == "Ukraine")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n44<- EC_dataframe$Freq[44]
lat <- rep(c_lat, n44)
lon <- rep(c_lon, n44)
c44 <- as.data.frame(cbind("Ukraine", lat, lon))

index <- which(latlongEU_countries$name == "United Kingdom")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n45 <- EC_dataframe$Freq[45]
lat <- rep(c_lat, n45)
lon <- rep(c_lon, n45)
c45 <- as.data.frame(cbind("United Kingdom", lat, lon))

index <- which(latlongEU_countries$name == "Vatican City")
c_lat <- latlongEU_countries$latitude[index]
c_lon <- latlongEU_countries$longitude[index]
n46 <- EC_dataframe$Freq[46]
lat <- rep(c_lat, n46)
lon <- rep(c_lon, n46)
c46 <- as.data.frame(cbind("Vatican City", lat, lon))


concat_data <- cbindX(t(c1),   t(c2),  t(c3),  t(c4),  t(c5),  t(c6),  t(c7),  t(c8),  t(c9), t(c10),
                      t(c11), t(c12), t(c13), t(c14), t(c15), t(c16), t(c17), t(c18), t(c19), t(c20),
                      t(c21), t(c22), t(c23), t(c24), t(c25), t(c26), t(c27), t(c28), t(c29), t(c30), 
                      t(c31), t(c32), t(c33), t(c34), t(c35), t(c36), t(c37), t(c38), t(c39), t(c40), 
                      t(c41), t(c42), t(c43), t(c44), t(c45), t(c46))

LL_df <- as.data.frame((t(as.data.frame(concat_data))))
head(LL_df)
tail(LL_df)
#LL_df$lat[!is.na(LL_df$lat)]
#LL_df$lon[!is.na(LL_df$lon)]
LL_df[,1]
latitude = as.data.frame(LL_df[,2])
longitude = as.data.frame(LL_df[,3])

#Add latitudes and Longitudes as columns to the original csv file "covid_data_Europe"
covid_data_Europe <- cbindX(covid_data_Europe, latitude, longitude)
glimpse(covid_data_Europe)
#Rename added column vectors to the data frame as "Latitude" and "Longitude"
names(covid_data_Europe)[names(covid_data_Europe) == "LL_df[, 2]"] <- "Latitude"
names(covid_data_Europe)[names(covid_data_Europe) == "LL_df[, 3]"] <- "Longitude"
glimpse(covid_data_Europe)

#Correlation of some variables for covid using two approaches (Europe)
covid_data_Europe$Latitude <- as.numeric(covid_data_Europe$Latitude)
covid_data_Europe$Longitude <- as.numeric(covid_data_Europe$Longitude)

covid_data_Europe<- covid_data_Europe[!is.na(covid_data_Europe$total_cases_per_million),]
covid_data_Europe<- covid_data_Europe[!is.na(covid_data_Europe$total_tests_per_thousand),]
correlation_vec_Europe <- t(cor(covid_data_Europe[c( "new_cases", "new_cases_per_million", "new_deaths", 
                                                     "new_deaths_per_million", "positive_rate", "weekly_icu_admissions",
                                                     "icu_patients_per_million","weekly_hosp_admissions", "hosp_patients_per_million",
                                                     "new_tests_per_thousand", "total_tests_per_thousand","total_cases_per_million",
                                                     "total_deaths_per_million")], 
                                covid_data_Europe[c( "new_cases", "new_cases_per_million", "new_deaths", 
                                                     "new_deaths_per_million", "positive_rate", "weekly_icu_admissions",
                                                     "icu_patients_per_million","weekly_hosp_admissions", "hosp_patients_per_million",
                                                     "new_tests_per_thousand", "total_tests_per_thousand","total_cases_per_million",
                                                     "total_deaths_per_million")]))
correlation_Europe <- data.frame(correlation_vec_Europe[order(-correlation_vec_Europe[,1]),])
ggcorrplot(correlation_Europe) + ggtitle("Correlation Matrix Europe") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("Correlation_Europe_all.bmp")
ggcorr(correlation_Europe, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white") 
ggsave("Correlation_Europe_all2.bmp")
corrplot::corrplot(correlation_vec_Europe) 
#ggsave("Correlation_Europe_all3.bmp")

#Correlation of some variables for covid using two approaches (all)
#Set initital time and final time of data
t0 <- covid_data$date[1] # "2020-02-24"
tf <- tail(covid_data$date)[6] #varies according to when you download the data

covid_data_top20 <- covid_data %>%
  group_by(date, location) %>%
  filter(date >= t0, location == TCTD[1] | location == TCTD[2] | location == TCTD[3] | location == TCTD[4] | location == TCTD[5] | location == TCTD[6] | location == TCTD[7] | location == TCTD[8] | location == TCTD[9] | location == TCTD[10] | location == TCTD[11] | location == TCTD[12] | location == TCTD[13] | location == TCTD[14] | location == TCTD[15] | location == TCTD[16] | location == TCTD[17] | location == TCTD[18] | location == TCTD[19] | location == TCTD[20]) 

covid_data_top20<- covid_data_top20[!is.na(covid_data_top20$total_cases_per_million),]
covid_data_top20<- covid_data_top20[!is.na(covid_data_top20$total_tests_per_thousand),]
correlation_vec <- t(cor(covid_data_top20[c( "new_cases", "new_cases_per_million", "new_deaths", 
                                             "new_deaths_per_million", "positive_rate", "weekly_icu_admissions",
                                             "icu_patients_per_million","weekly_hosp_admissions", "hosp_patients_per_million",
                                             "new_tests_per_thousand", "total_tests_per_thousand","total_cases_per_million",
                                             "total_deaths_per_million")],
                         covid_data_top20[c( "new_cases", "new_cases_per_million", "new_deaths", 
                                             "new_deaths_per_million", "positive_rate", "weekly_icu_admissions",
                                             "icu_patients_per_million","weekly_hosp_admissions", "hosp_patients_per_million",
                                             "new_tests_per_thousand", "total_tests_per_thousand","total_cases_per_million",
                                             "total_deaths_per_million")]))

correlation <- data.frame(correlation_vec[order(-correlation_vec[,1]),])
ggcorrplot(correlation) + ggtitle("Correlation Matrix Top 20 Countries in Europe") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("Correlation_Europe_20.bmp")
ggcorr(correlation, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")
ggsave("Correlation_Europe_20_2.bmp")
corrplot::corrplot(correlation_vec) 
#ggsave("Correlation_Europe_20_3.bmp")

#========================================================
#Machine Learning
# Validation set will be 10% of covid data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = covid_data_Europe$total_deaths_per_million, times = 1, p = 0.1, list = FALSE)
#Define train set and test set:
covid_train_set <- covid_data_Europe[-test_index,] #train set
covid_test_set <- covid_data_Europe[test_index,] #test set

names(covid_data_Europe)
# Make sure needed variables are in validation set are also in covid data set for Europe case
validation <- covid_test_set %>%
  semi_join(covid_train_set, by = "new_cases_per_million" ) 

# Add rows removed from validation set back into covid data set
removed <- anti_join(covid_test_set, validation)
covid_train_set <- rbind(covid_train_set, removed)
head(covid_train_set)

#Explore the data a bit more for the train set and validation (test) set:
summary(covid_train_set)
#for validation
summary(validation)

#Run Models:
#Apply simple model to the train set:
mu_hat <- mean(covid_train_set$total_deaths_per_million)
mu_hat
#Obtain a naive RMSE for test set:
naive_rmse <- RMSE(covid_test_set$total_deaths_per_million, mu_hat)
naive_rmse
#Note this naive RMSE is different that that one for the validation:
naive_rmse <- RMSE(validation$total_deaths_per_million, mu_hat)
naive_rmse
rmse_results <- tibble(method = "Just the average", 
                       RMSE = naive_rmse)
rmse_results

#===================Model 1===============================
covid_train_set %>% 
  group_by(new_cases_per_million) %>% 
  summarize(b_n = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_n)) + 
  geom_histogram(bins = 30, color = "Blue") +
  labs(x="Least Square Estimate, b_n", y="Averages", title="New Cases per Million Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("NewCasesPM_bn.bmp")

mu <- mean(covid_train_set$total_deaths_per_million) 
#New cases averages
newcasesPM_avgs <- covid_train_set %>% 
  group_by(new_cases_per_million) %>% 
  summarize(b_n = mean(total_deaths_per_million - mu))
#Plot the estimated new cases per million averages
newcasesPM_avgs %>% qplot(b_n, geom ="histogram", bins = 30, data = ., color = I("black"))
#Predict the deaths in this simple model:
predicted_newcasesPM <- mu + validation %>% 
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  .$b_n
sum(is.na(predicted_newcasesPM))/length(predicted_newcasesPM)
#Replace any NA by 0
predicted_newcasesPM[is.na(predicted_newcasesPM)] <- 0

#Obtain the RMSE for model 1:
model_1_rmse <- RMSE(predicted_newcasesPM, validation$total_deaths_per_million)
#Display the RMSE results for model 1:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="New Cases Per Million Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results
#Provide RMSE results for model 1 in the form of a table
rmse_results %>% knitr::kable()

#===================Model 2===============================
#Next, we are going to analyze the "location" effect
#using the training set:
covid_train_set %>% 
  group_by(location) %>% 
  summarize(b_l = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_l)) + 
  geom_histogram(bins = 30, color = "Cyan")+
  labs(x="Least Square Estimate, b_l", y="Averages", title="Location Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Location_bl.bmp")

#User averages on the training set
location_avgs <- covid_train_set %>%
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  group_by(location) %>%
  summarize(b_l = mean(total_deaths_per_million - mu - b_n))
#Obtain predicted deaths using the test set
predicted_new_deaths <- validation %>% 
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  left_join(location_avgs, by='location') %>%
  mutate(pred = mu + b_n + b_l) %>%
  .$pred
#Replace any NA by 0
predicted_new_deaths[is.na(predicted_new_deaths)] <- 0

#Obtain the RMSE for model 2:
model_2_rmse <- RMSE(predicted_new_deaths, validation$total_deaths_per_million)
#Display the RMSE results for model 2:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="New Cases Per Million + Location Effects Model",  
                                     RMSE = model_2_rmse ))
#Provide RMSE results for model 2 in the form of a table
rmse_results %>% knitr::kable()

#===================Model 3===============================
#Next, we are going to analyze the "date" effect
#using the training set:
covid_train_set %>% 
  group_by(date) %>% 
  summarize(b_d = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_d)) + 
  geom_histogram(bins = 30, color = "orange")+
  labs(x="Least Square Estimate, b_d", y="Averages", title="Date Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Date_bd.bmp")

#User averages on the training set
date_avgs <- covid_train_set %>%
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  left_join(location_avgs, by='location') %>%
  group_by(date) %>%
  summarize(b_d = mean(total_deaths_per_million - mu - b_n - b_l))
#Obtain predicted deaths using the test set
predicted_new_deaths <- validation %>% 
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  left_join(location_avgs, by='location') %>%
  left_join(date_avgs, by='date') %>%
  mutate(pred = mu + b_n + b_l + b_d) %>%
  .$pred
#Replace any NA by 0
predicted_new_deaths[is.na(predicted_new_deaths)] <- 0

#Obtain the RMSE for model 3:
model_3_rmse <- RMSE(predicted_new_deaths, validation$total_deaths_per_million)
#Display the RMSE results for model 3:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="New Cases Per Million + Location + Date Effects Model",  
                                     RMSE = model_3_rmse ))
#Provide RMSE results for model 3 in the form of a table
rmse_results %>% knitr::kable()


#===================Model 4===============================
#Analyze the "location" effect using the training set
#Average
covid_train_set %>% 
  group_by(location) %>% 
  summarize(b_l = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_l)) + 
  geom_histogram(bins = 30, color = "black")

covid_train_set %>% group_by(location) %>%
  summarize(n = n(), avg = mean(new_deaths), se = sd(new_deaths)/sqrt(n())) %>%
  filter(n >= 5) %>% 
  mutate(location = reorder(location, avg)) %>%
  ggplot(aes(x = reorder(location, -avg), y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

mu <- mean(covid_train_set$total_deaths_per_million) 
#New death averages
location_avgs <- covid_train_set %>% 
  group_by(location) %>% 
  summarize(b_l = mean(total_deaths_per_million - mu))
#Plot the estimated death averages by location
location_avgs %>% qplot(b_l, geom ="histogram", bins = 30, data = ., color = I("black"))
#Predict the deaths in this simple model:
predicted_locations <- mu + validation %>% 
  left_join(location_avgs, by='location') %>%
  .$b_l
#Obtain the RMSE for model 4:
model_4_rmse <- RMSE(predicted_locations, validation$total_deaths_per_million)
#Display the RMSE results for model 4:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Location Effect Model",
                                     RMSE = model_4_rmse ))
rmse_results
#Provide RMSE results for model 4 in the form of a table
rmse_results %>% knitr::kable()


#====================Model 5=============================
#Next, we are going to analyze the "date" effect
#using the training set:
covid_train_set %>% 
  group_by(date) %>% 
  summarize(b_d = mean(total_deaths_per_million)) %>% 
  filter(n()>= 1) %>%
  ggplot(aes(b_d)) + 
  geom_histogram(bins = 30, color = "black")

covid_train_set %>% group_by(date) %>%
  summarize(n = n(), avg = mean(new_deaths), se = sd(new_deaths)/sqrt(n())) %>%
  filter(n >= 30) %>% 
  mutate(date = reorder(date, avg)) %>%
  ggplot(aes(x = reorder(date, -avg), y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#User averages on the training set
date_avgs <- covid_train_set %>%
  left_join(location_avgs, by='location') %>%
  group_by(date) %>%
  summarize(b_d = mean(total_deaths_per_million - mu - b_l))
#Obtain predicted deaths the test set
predicted_new_deaths <- validation %>% 
  left_join(location_avgs, by='location') %>%
  left_join(date_avgs, by='date') %>%
  mutate(pred = mu + b_l + b_d) %>%
  .$pred
#Replace any NA by 0
predicted_new_deaths[is.na(predicted_new_deaths)] <- 0
#Obtain the RMSE for model 5:
model_5_rmse <- RMSE(predicted_new_deaths, validation$total_deaths_per_million)
#Display the RMSE results for model 5:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Location + Date Effects Model",  
                                     RMSE = model_5_rmse ))
#Provide RMSE results for model 5 in the form of a table
rmse_results %>% knitr::kable()


#====================Model 6==============================
#Next, we are going to analyze the "new_cases_per_million" effect
#using the training set:
covid_train_set %>% group_by(new_cases_per_million) %>%
  summarize(n = n(), avg = mean(new_deaths), se = sd(new_deaths)/sqrt(n())) %>%
  filter(n >= 8) %>% 
  mutate(new_cases_per_million = reorder(new_cases_per_million, avg)) %>%
  ggplot(aes(x = reorder(new_cases_per_million, -avg), y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

covid_train_set %>% 
  group_by(new_cases_per_million) %>% 
  summarize(b_n = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_n)) + 
  geom_histogram(bins = 30, color = "black")
#User averages on the training set
newcasesPM_avgs <- covid_train_set %>%
  left_join(location_avgs, by='location') %>%
  left_join(date_avgs, by='date') %>%
  group_by(new_cases_per_million) %>%
  summarize(b_n = mean(total_deaths_per_million - mu - b_l - b_d))
#Obtain predicted deaths using the test set
predicted_new_casesPM <- validation %>% 
  left_join(location_avgs, by='location') %>%
  left_join(date_avgs, by='date') %>%
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  mutate(pred = mu + b_l + b_d + b_n) %>%
  .$pred
#Replace any NA by 0
predicted_new_casesPM[is.na(predicted_new_casesPM)] <- 0
#Obtain the RMSE for model 6:
model_6_rmse <- RMSE(predicted_new_casesPM, validation$total_deaths_per_million)
#Display the RMSE results for model 6:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Location + Date + New cases Per Million Effects Model",  
                                     RMSE = model_6_rmse ))
#Provide RMSE results for model 6 in the form of a table
rmse_results %>% knitr::kable()

#===================Model 7===============================
#Analyze the "date" effect using the training set
#Average
covid_train_set %>% 
  group_by(date) %>% 
  summarize(b_d = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_d)) + 
  geom_histogram(bins = 30, color = "black")

mu <- mean(covid_train_set$total_deaths_per_million) 
#New death averages
date_avgs <- covid_train_set %>% 
  group_by(date) %>% 
  summarize(b_d = mean(total_deaths_per_million - mu))
#Plot the estimated death averages by date
date_avgs %>% qplot(b_d, geom ="histogram", bins = 30, data = ., color = I("black"))
#Predict the deaths in this simple model:
predicted_dates <- mu + validation %>% 
  left_join(date_avgs, by='date') %>%
  .$b_d
#Replace any NA by 0
predicted_dates[is.na(predicted_dates)] <- 0
#Obtain the RMSE for model 1:
model_7_rmse <- RMSE(predicted_dates, validation$total_deaths_per_million)
#Display the RMSE results for model 7:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Date Effect Model",
                                     RMSE = model_7_rmse ))
rmse_results
#Provide RMSE results for model 7 in the form of a table
rmse_results %>% knitr::kable()


#===================Model 8===============================
#Next, we are going to analyze the "date and latitude" effect
#using the training set:
covid_train_set %>% 
  group_by(Latitude) %>% 
  summarize(b_lt = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_lt)) + 
  geom_histogram(bins = 30, color = "black")+
  labs(x="Least Square Estimate, b_n", y="Averages", title="Latitude Effects") +
  theme(plot.title = element_text(hjust = 0.5))

#User averages on the training set
latitude_avgs <- covid_train_set %>%
  left_join(date_avgs, by='date') %>%
  group_by(Latitude) %>%
  summarize(b_lt = mean(total_deaths_per_million - mu - b_d))
#Obtain predicted deaths using the test set
predicted_latitudes <- validation %>% 
  left_join(date_avgs, by='date') %>%
  left_join(latitude_avgs, by='Latitude') %>%
  mutate(pred = mu + b_d + b_lt) %>%
  .$pred
#Replace any NA by 0
predicted_latitudes[is.na(predicted_latitudes)] <- 0
#Obtain the RMSE for model 8:
model_8_rmse <- RMSE(predicted_latitudes, validation$total_deaths_per_million)
#Display the RMSE results for model 8:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Date + Latitude Effects Model",  
                                     RMSE = model_8_rmse ))
#Provide RMSE results for model 8 in the form of a table
rmse_results %>% knitr::kable()

#===================Model 9===============================
#Next, we are going to analyze the "date and longitude" effect
#using the training set:
covid_train_set %>% 
  group_by(Longitude) %>% 
  summarize(b_ln = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_ln)) + 
  geom_histogram(bins = 30, color = "black")+
  labs(x="Least Square Estimate, b_n", y="Averages", title="Longitude Effects") +
  theme(plot.title = element_text(hjust = 0.5))

#User averages on the training set
longitude_avgs <- covid_train_set %>%
  left_join(date_avgs, by='date') %>%
  group_by(Longitude) %>%
  summarize(b_ln = mean(total_deaths_per_million - mu - b_d))
#Obtain predicted deaths using the test set
predicted_longitudes <- validation %>% 
  left_join(date_avgs, by='date') %>%
  left_join(longitude_avgs, by='Longitude') %>%
  mutate(pred = mu + b_d + b_ln) %>%
  .$pred
#Replace any NA by 0
predicted_longitudes[is.na(predicted_longitudes)] <- 0
#Obtain the RMSE for model 9:
model_9_rmse <- RMSE(predicted_longitudes, validation$total_deaths_per_million)
#Display the RMSE results for model 9:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Date + Longitude Effects Model",  
                                     RMSE = model_9_rmse ))
#Provide RMSE results for model 9 in the form of a table
rmse_results %>% knitr::kable()


#===================Model 10===============================
#Next, we are going to analyze the "date and new cases per million" effect
#using the training set:
covid_train_set %>% 
  group_by(new_cases_per_million) %>% 
  summarize(b_n = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_n)) + 
  geom_histogram(bins = 30, color = "black")
#User averages on the training set
newcasesPM_avgs <- covid_train_set %>%
  left_join(date_avgs, by='date') %>%
  group_by(new_cases_per_million) %>%
  summarize(b_n = mean(total_deaths_per_million - mu - b_d))
#Obtain predicted deaths using the test set
predicted_newcasesPM <- validation %>% 
  left_join(date_avgs, by='date') %>%
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  mutate(pred = mu + b_d + b_n) %>%
  .$pred
#Replace any NA by 0
predicted_newcasesPM[is.na(predicted_newcasesPM)] <- 0
#Obtain the RMSE for model 10:
model_10_rmse <- RMSE(predicted_newcasesPM, validation$total_deaths_per_million)
#Display the RMSE results for model 10:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Date + New Cases Per Million Effects Model",  
                                     RMSE = model_10_rmse ))
#Provide RMSE results for model 10 in the form of a table
rmse_results %>% knitr::kable()

#==================================Model 11==================
#Next, we are going to analyze the "date and new cases per million" effect
#using the training set:
covid_train_set %>% 
  group_by(date) %>% 
  summarize(b_d = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_d)) + 
  geom_histogram(bins = 30, color = "black")
#User averages on the training set
date_avgs <- covid_train_set %>%
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  group_by(date) %>%
  summarize(b_d = mean(total_deaths_per_million - mu - b_n))
#Obtain predicted deaths using the test set
predicted_dates <- validation %>% 
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  left_join(date_avgs, by='date') %>%
  mutate(pred = mu + b_n + b_d) %>%
  .$pred
#Replace any NA by 0
predicted_dates[is.na(predicted_dates)] <- 0
#Obtain the RMSE for model 11:
model_11_rmse <- RMSE(predicted_dates, validation$total_deaths_per_million)
#Display the RMSE results for model 11:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="New Cases Per Million + Date Effects Model",  
                                     RMSE = model_11_rmse ))
#Provide RMSE results for model 11 in the form of a table
rmse_results %>% knitr::kable()

#===================Model 12===============================
#Analyze the "positive rate" effect using the training set
covid_train_set %>% 
  group_by(positive_rate) %>% 
  summarize(b_r = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_r)) + 
  geom_histogram(bins = 30, color = "Red")+
  labs(x="Least Square Estimate, b_d", y="Averages", title="Positive Rate Effects") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("PosRate_br.bmp")

mu <- mean(covid_train_set$total_deaths_per_million) 
#New cases averages
posrate_avgs <- covid_train_set %>% 
  group_by(positive_rate) %>% 
  summarize(b_r = mean(total_deaths_per_million - mu))
#Plot the estimated deaths averages by positive rate
posrate_avgs %>% qplot(b_r, geom ="histogram", bins = 30, data = ., color = I("black"))
#Predict the positive rates in this simple model:
predicted_posrate <- mu + validation %>% 
  left_join(posrate_avgs, by='positive_rate') %>%
  .$b_r
sum(is.na(predicted_posrate))/length(predicted_posrate)
#Replace any NA by 0
predicted_posrate[is.na(predicted_posrate)] <- 0
#Obtain the RMSE for model 12:
model_12_rmse <- RMSE(predicted_posrate, validation$total_deaths_per_million)
#Display the RMSE results for model 12:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Positive Rate Effect Model",
                                     RMSE = model_12_rmse ))
rmse_results
#Provide RMSE results for model 12 in the form of a table
rmse_results %>% knitr::kable()

#===================Model 13===============================
#Analyze the "positive rate + new cases per million" effect using the training set:
covid_train_set %>% 
  group_by(positive_rate) %>% 
  summarize(b_r = mean(total_deaths_per_million)) %>% 
  filter(n()>=1) %>%
  ggplot(aes(b_r)) + 
  geom_histogram(bins = 30, color = "black")
#User averages on the training set
newcasesPM_avgs <- covid_train_set %>%
  left_join(posrate_avgs, by='positive_rate') %>%
  group_by(new_cases_per_million) %>%
  summarize(b_n = mean(total_deaths_per_million - mu - b_r))
#Obtain predicted new cases per million using the test set
predicted_new_deaths <- validation %>% 
  left_join(posrate_avgs, by='positive_rate') %>%
  left_join(newcasesPM_avgs, by='new_cases_per_million') %>%
  mutate(pred = mu + b_r + b_n) %>%
  .$pred
#Replace any NA by 0
predicted_new_deaths[is.na(predicted_new_deaths)] <- 0
#Obtain the RMSE for model 13:
model_13_rmse <- RMSE(predicted_new_deaths, validation$total_deaths_per_million)
#Display the RMSE results for model 13:
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Positive Rate + New Cases per Million Effects Model",  
                                     RMSE = model_13_rmse ))
#Provide RMSE results for model 13 in the form of a table
rmse_results %>% knitr::kable()

#=====================Regularization==================
#To improve our results, we will use regularization.
#Group by DATE
lambdas <- seq(0, 10, 0.25)
mu <- mean(covid_train_set$total_deaths_per_million)
just_the_sum <- covid_train_set %>% 
  group_by(date) %>% 
  summarize(s = sum(total_deaths_per_million - mu), n_i = n())
rmses <- sapply(lambdas, function(d){
  predicted_date <- validation %>% 
    left_join(just_the_sum, by="date") %>% 
    mutate(b_d = s/(n_i+d)) %>%
    mutate(pred = mu + b_d) %>%
    .$pred
  predicted_date[is.na(predicted_date)] <- 0
  return(RMSE(predicted_date, validation$total_deaths_per_million))
})
qplot(lambdas, rmses)  + 
  labs(x="Lambdas", y="RMSES", title="Regularization: Date") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Lambda_covid_date.bmp")
lambdas[which.min(rmses)]
min(rmses)
#We can also use regularization to estimate the new cases per million effect:
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(d){
  mu <- mean(covid_train_set$total_deaths_per_million)
  b_d <- covid_train_set %>%
    group_by(date) %>%
    summarize(b_d = sum(total_deaths_per_million - mu)/(n()+d))
  b_n <- covid_train_set %>% 
    left_join(b_d, by="date") %>%
    group_by(new_cases_per_million) %>%
    summarize(b_n = sum(total_deaths_per_million - b_d - mu)/(n()+d))
  predicted_newcasesPM <- 
    validation %>% 
    left_join(b_d, by = "date") %>%
    left_join(b_n, by = "new_cases_per_million") %>%
    mutate(pred = mu + b_d + b_n) %>%
    .$pred
  predicted_newcasesPM[is.na(predicted_newcasesPM)] <- 0
  return(RMSE(predicted_newcasesPM, validation$total_deaths_per_million))
})
#Plot the RMSES vs. the Lambdas
qplot(lambdas, rmses)  + 
  labs(x="Lambdas", y="RMSES", title="Regularization: Date + New Cases per Million") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Lambda_covid_date_NewCases.bmp")
#Select the lambda that minimizes the RMSE
lambda <- lambdas[which.min(rmses)]
lambda
#RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Date + New Cases Per Million Effect Model",  
                                     RMSE = min(rmses)))
#RMSE results in form of table
rmse_results %>% knitr::kable()

#=====================Regularization==================
#To improve our results, we will use regularization.
#Group by positive rate
lambdas <- seq(0, 30, 0.25)
mu <- mean(covid_train_set$total_deaths_per_million)
just_the_sum <- covid_train_set %>% 
  group_by(positive_rate) %>% 
  summarize(s = sum(total_deaths_per_million - mu), n_i = n())
rmses <- sapply(lambdas, function(d){
  predicted_posrate <- validation %>% 
    left_join(just_the_sum, by="positive_rate") %>% 
    mutate(b_r = s/(n_i+d)) %>%
    mutate(pred = mu + b_r) %>%
    .$pred
  predicted_posrate[is.na(predicted_posrate)] <- 0
  return(RMSE(predicted_posrate, validation$total_deaths_per_million))
})
qplot(lambdas, rmses)  + 
  labs(x="Lambdas", y="RMSES", title="Regularization: Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Lambda_covid_PositiveRate.bmp")
lambdas[which.min(rmses)]
#RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Positive Rate Effect Model",  
                                     RMSE = min(rmses)))
#RMSE results in form of table
rmse_results %>% knitr::kable()

#We can also use regularization to estimate the new cases per million effect:
lambdas <- seq(0, 20, 0.25)
rmses <- sapply(lambdas, function(d){
  mu <- mean(covid_train_set$total_deaths_per_million)
  b_r <- covid_train_set %>%
    group_by(positive_rate) %>%
    summarize(b_r = sum(total_deaths_per_million - mu)/(n()+d))
  b_n <- covid_train_set %>% 
    left_join(b_r, by="positive_rate") %>%
    group_by(new_cases_per_million) %>%
    summarize(b_n = sum(total_deaths_per_million - b_r - mu)/(n()+d))
  predicted_newcasesPM <- 
    validation %>% 
    left_join(b_r, by = "positive_rate") %>%
    left_join(b_n, by = "new_cases_per_million") %>%
    mutate(pred = mu + b_r + b_n) %>%
    .$pred
  predicted_newcasesPM[is.na(predicted_newcasesPM)] <- 0
  return(RMSE(predicted_newcasesPM, validation$total_deaths_per_million))
})
#Plot the RMSES vs. the Lambdas
qplot(lambdas, rmses)  + 
  labs(x="Lambdas", y="RMSES", title="Regularization: Positive Rate + New Cases per Million") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Lambda_covid_PositiveRate_NewCasesPM.bmp")
#Select the lambda that minimizes the RMSE
lambda <- lambdas[which.min(rmses)]
lambda
#RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Positive Rate + New Cases Per Million Effect Model",  
                                     RMSE = min(rmses)))
#RMSE results in form of table
rmse_results %>% knitr::kable()


#=====================Regularization==================
#New cases per million, date and positive rate:
lambdas <- seq(0, 20, 0.25)
rmses <- sapply(lambdas, function(d){
  mu <- mean(covid_train_set$total_deaths_per_million)
  b_n <- covid_train_set %>%
    group_by(new_cases_per_million) %>%
    summarize(b_n = sum(total_deaths_per_million - mu)/(n()+d))
  b_d <- covid_train_set %>% 
    left_join(b_n, by="new_cases_per_million") %>%
    group_by(date) %>%
    summarize(b_d = sum(total_deaths_per_million - b_n - mu)/(n()+d))
  b_r <- covid_train_set %>% 
    left_join(b_n, by="new_cases_per_million") %>%
    left_join(b_d, by="date") %>%
    group_by(positive_rate) %>%
    summarize(b_r = sum(total_deaths_per_million - b_n - b_d - mu)/(n()+d))
  predicted_deaths <- 
    validation %>% 
    left_join(b_n, by = "new_cases_per_million") %>%
    left_join(b_d, by = "date") %>%
    left_join(b_r, by = "positive_rate") %>%
    mutate(pred = mu + b_n + b_d + b_r) %>%
    .$pred
  predicted_deaths[is.na(predicted_deaths)] <- 0
  return(RMSE(predicted_deaths, validation$total_deaths_per_million))
})
#Plot the RMSES vs. the Lambdas
qplot(lambdas, rmses) + 
  labs(x="Lambdas", y="RMSES", title="Regularization: New Cases per Million + Date + Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Lambda_covid_NewCasesPM_date_PosRate.bmp")
#Select the lambda that minimizes the RMSE
lambda <- lambdas[which.min(rmses)]
lambda
#RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized New Cases Per Million + Date + Positive Rate Effect Model",  
                                     RMSE = min(rmses)))
#RMSE results in form of table
rmse_results %>% knitr::kable()

#=====================Regularization==================
#Final regularization (date, new cases per million and positive rate):
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(d){
  mu <- mean(covid_train_set$total_deaths_per_million)
  b_d <- covid_train_set %>%
    group_by(date) %>%
    summarize(b_d = sum(total_deaths_per_million - mu)/(n()+d))
  b_n <- covid_train_set %>% 
    left_join(b_d, by="date") %>%
    group_by(new_cases_per_million) %>%
    summarize(b_n = sum(total_deaths_per_million - b_d - mu)/(n()+d))
  b_r <- covid_train_set %>% 
    left_join(b_d, by="date") %>%
    left_join(b_n, by="new_cases_per_million") %>%
    group_by(positive_rate) %>%
    summarize(b_r = sum(total_deaths_per_million - b_d - b_n - mu)/(n()+d))
  predicted_deaths <- 
    validation %>% 
    left_join(b_d, by = "date") %>%
    left_join(b_n, by = "new_cases_per_million") %>%
    left_join(b_r, by = "positive_rate") %>%
    mutate(pred = mu + b_d + b_n + b_r) %>%
    .$pred
  predicted_deaths[is.na(predicted_deaths)] <- 0
  return(RMSE(predicted_deaths, validation$total_deaths_per_million))
})
#Plot the RMSES vs. the Lambdas
qplot(lambdas, rmses)  + 
  labs(x="Lambdas", y="RMSES", title="Regularization: Date + New Cases per Million + Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Lambda_covid_Date_NewCasesPM_PosRate.bmp")
#Select the lambda that minimizes the RMSE
lambda <- lambdas[which.min(rmses)]
lambda
#RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Date + New Cases Per Million + Positive Rate Effect Model",  
                                     RMSE = min(rmses)))
#RMSE results in form of table
rmse_results %>% knitr::kable()

#Save the RMSE results as a file
write.table(rmse_results, file = "RMSE_Covid_Europe.csv",
            sep = "\t", row.names = F)