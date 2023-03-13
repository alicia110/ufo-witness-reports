########## Patterns & Correlations in UFO witness reports worldwide #######
###########################################################################
########## Geo-Visualization, Temporal Correlation ########################

#test git

# loading packages
library(ggplot2)#plots in general
library(ggmap)#maps
library(tidyverse)#pipes
library(ggrepel)#for data cleaning
library(dplyr)#stats
library(psych)#stats
library(lubridate)#converting date
library(plotly)#interactive graphics
library(stringr)#for data cleaning
library(gridExtra)#for arranging plots in grid
library(grid)#for arranging plots in grid
citation("ggplot2")
options(stringsAsFactors = FALSE)

# setting working directory
setwd("/Users/aliciadiem/Desktop/Digital Humanities")

# loading dataframe 
dat <- read.csv("ufo_sighting_data.csv", sep = ",", header = T, fill = T)

### DATA CLEANING & PREPARATION

# renaming columns (shorter names)
names(dat) <- c("datetime", "city", "state", "country", "shape", "duration_s", "duration", "report", "date_doc", "latitude", "longitude")

# checking data structure
str(dat)

# data cleaning
dat$duration_s <- dat$duration_s %>% gsub(",","",.)%>% gsub("'","",.)%>% gsub("`","",.)
dat$latitude <- dat$latitude %>% gsub("q","",.)%>% gsub("'","",.)%>% gsub("`","",.)

# preparing variables for analysis
dat <- dat %>% 
  mutate(latitude = as.numeric(str_replace_all(latitude, ",","."))) %>% # converting to numeric for analysis
  mutate(longitude = as.numeric(str_replace_all(longitude, ",","."))) %>% # changing "," to "."
  mutate(duration_s = as.numeric(duration_s))%>%
  mutate(country = as.factor(country))%>% # converting to factor for analysis
  mutate(state = as.factor(state))%>%
  mutate(shape = as.factor(shape))%>%
  mutate(datetime = mdy_hm(datetime))%>% # converting date formats
  mutate(date_doc = mdy(date_doc))%>%
  na.omit() # removing NAs
  
# checking factor levels of country and renaming them
levels(dat$country)
levels(dat$country) <- c("Andere", "Australien", "Kanada", "Deutschland", "Großbritannien", "USA")
# changing order of factor
dat$country <- factor(dat$country, levels = c("USA", "Andere", "Kanada", "Großbritannien", "Australien", "Deutschland"))

# summary and descriptive stats of variables
summary(dat)
library(skimr)
skim(dat)
sum(dat$country == "USA")/ 80332 * 100 # ca 81% of total sightings are in USA

### WORLD ANALYSIS ###

# setting boundaries for map
bbox_world <- c(bottom = -60, top = 80, left = -180, right = 180)

# mapping density of sightings worldwide
map_world <- get_map(bbox_world, zoom=2,
                       source = "stamen",
                       color = "color",
                       maptype="toner-lite")
map_world <- ggmap(map_world) + 
  geom_point(data = dat, mapping = aes(x = longitude, y = latitude, color = country),
             shape = ".", alpha = 0.6)+ 
  scale_color_manual(name = "Land", values = c("blue","#ce93d8","#81d4fa","#F9740F","#F6C628", "#3CB371"), 
                     guide = guide_legend(override.aes = list(size = 2, shape= 16, alpha =1)))+
  ylab("Latitude") + xlab("Longitude")+ ggtitle("Verteilung der UFO-Sichtungen weltweit (1906-2014)") + 
  theme(plot.title = element_text(size = 12), legend.position = "right")
map_world

# plotting number of sightings/country
count_world <- ggplot(dat, aes(x=reorder(country, country, FUN=length), fill=country)) +
  geom_bar() +
  theme(legend.position="none") +
  scale_fill_manual(name = "country", values = c("blue","#ce93d8","#81d4fa","#F9740F","#F6C628", "#3CB371"))+
  labs(x = "Land", y = "Anzahl", 
       title="UFO-Sichtungen pro Land")
count_world
  
# creating subset to show timeline of sightings worldwide
dat_time <- dat %>% group_by(country = country, year=year(datetime))%>% 
  summarize(count=n())
dat_time_all <- dat %>% group_by(year=year(datetime))%>% summarize(count=n())
dat_time_all['country']='Alle'
dat_time_all %>% select("country", everything())
dat_time <- bind_rows(dat_time, dat_time_all)%>% mutate(country = as.factor(country))
dat_time$country <- factor(dat_time$country, levels = c("Alle", "USA", "Andere", "Kanada", "Großbritannien", "Australien", "Deutschland"))
names(dat_time)= c("Land", "Jahr", "Anzahl")

# plotting timeline
plot_timeline <- ggplot(dat_time, aes(x=Jahr, y=Anzahl, colour = Land))+
  geom_line()+
  labs(x = "Jahre", y = "Anzahl", title ="Zeitverlauf der UFO-Sichtungen weltweit (1906-2014)",
       subtitle = "aufgeschlüsselt nach Ländern")+
  scale_color_manual(name = "Land", values = c("darkgrey","blue","#ce93d8","#81d4fa","#F9740F","#F6C628", "#3CB371"))

# converting to interactive plot  
plot_timeline <- ggplotly(plot_timeline)
plot_timeline

# plotting date documented 
dat_documented <- dat %>% group_by(year=year(date_doc))%>% summarize(count=n())
dat_documented <- merge(dat_documented, dat_time_all, by="year", all = T)%>% 
  select("Jahr"="year", "dokumentiert"="count.x", "gesichtet"="count.y")%>%
  gather(.,"dok_sich","Anzahl", 2:3)

plot_documentation <- ggplot(dat_documented, aes(x=Jahr, y=Anzahl, color = dok_sich))+
  geom_line() +
  labs(x = "Jahre", y = "Anzahl", title ="Dokumentation der UFO-Sichtungen weltweit")+
  scale_color_manual(name = "", values = c("darkblue", "darkgrey"))

plot_documentation <- ggplotly(plot_documentation)
plot_documentation

# plotting timeline (1990-2015) to analyse peaks of sightings
plot_timeline_90er <- ggplot(dat_time, aes(x=Jahr, y=Anzahl, colour = Land))+
  geom_line()+
  annotate("text",size= 3, x=1995, y=1300, label="1995", colour="darkgrey", parse =T)+
  annotate("text",size= 3, x=1999, y=3100, label="1999", colour="darkgrey")+
  annotate("text",size= 3, x=2004, y=4500, label="2004", colour="darkgrey")+
  annotate("text",size= 3, x=2008, y=5100, label="2008", colour="darkgrey")+
  annotate("text",size= 3, x=2012, y=7600, label="2012", colour="darkgrey")+
  labs(x = "Jahre", y = "Anzahl", title ="Peaks im Zeitverlauf der UFO-Sichtungen weltweit (1990 - 2014)")+
  scale_color_manual(name = "Land", values = c("darkgrey","blue","#ce93d8","#81d4fa","#F9740F","#F6C628", "#3CB371"))+
  xlim(1990, 2015)+ ylim(0,8000)
plot_timeline_90er

plot_timeline_90er <- ggplotly(plot_timeline_90er)
plot_timeline_90er

### USA ANALYSIS ###

# creating subset for USA
dat_us <- dat %>% filter(., country == "USA", .preserve = FALSE)
#states only
dat_states <- dat_us %>% group_by(Staat_abk = state)%>% reframe(Anzahl=n())
summary(dat_us)
# adding population data
df_population <- read.csv("us_pop_by_state.csv", sep = ",", header = T, fill = T)
df_population$state_code <- tolower(df_population$state_code)
names(df_population)= c("Ranking", "Staat", "Staat_abk", "Population", "Prozent")

# data per state
dat_states <- merge(dat_states, df_population, by = "Staat_abk", all = TRUE)%>%
  filter(., Staat != "NA", .preserve = FALSE)%>%
  filter(., Staat != "Total U.S.", .preserve = FALSE)%>%
dat_states <- dat_states %>% arrange(desc(Anzahl))%>%
  mutate(Staat = as.factor(Staat))# converting to factor for analysis
  
# plotting number of sightings/top 20 states
library(RColorBrewer)
palette_us <- c("#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
palette_us <- colorRampPalette(palette_us)(20)
palette_us <- sort(palette_us, decreasing = F)
dat_states$Staat <- reorder(dat_states$Staat, dat_states$Anzahl, FUN = mean)

count_us <- ggplot(head(dat_states,20), aes(x=Staat, y=Anzahl), fill=Staat) +
  geom_col(fill = palette_us) +
  coord_flip()+
  theme(legend.position="none")+
  scale_color_manual(name = "Staat", values = palette_us)+
  labs(x = "Staat", y = "Anzahl der Sichtungen", 
       title="Sichtungen pro Staat in den USA (Top 20)")
count_us

population_us <- ggplot(head(dat_states,20), aes(x=Staat, y=Prozent*100), fill=Staat) +
  geom_col(fill =palette_us) +
  coord_flip()+
  theme(legend.position="none")+
  scale_color_manual(name = "Staat", values = palette_us)+
  labs(x = "Staat", y = "Prozentualer Anteil an Gesamtpopulation", 
       title="Population pro Staat in den USA")
population_us

# mapping density of total sightings for USA

bbox_us <- c(bottom = 24, top = 50, left = -127, right = -65)
mapping_us <- get_stamenmap(bbox_us, zoom=5,color = "color", maptype = "toner-lite")
map_us <- ggmap(mapping_us) + 
  geom_point(data = dat_us, mapping = aes(x = longitude, y = latitude),
             shape = ".", alpha = 0.6, color = "blue")+ theme(legend.position= "none")+
  ylab("Latitude") + xlab("Longitude")+ ggtitle("Verteilung der UFO-Sichtungen in den USA (1910-2014)")
map_us

# mapping density of sightings for USA before and after peaks of sightings to see development

map_us_before1990 <- ggmap(mapping_us) + 
  geom_point(data = dat_us %>% filter(., year(datetime) <= "1990", .preserve = FALSE), mapping = aes(x = longitude, y = latitude),
             shape = ".", alpha = 0.5, color = "blue")+ theme(legend.position= "none")+ ylab("Latitude") + xlab("Longitude")+ggtitle("Jahr 1990")
map_us_before1990

map_us_before1995 <- ggmap(mapping_us) + 
  geom_point(data = dat_us %>% filter(., year(datetime) <= "1995", .preserve = FALSE), mapping = aes(x = longitude, y = latitude),
             shape = ".", alpha = 0.5, color = "blue")+ theme(legend.position= "none")+  ylab("Latitude") + xlab("Longitude")+ggtitle("Jahr 1995")
map_us_before1995

map_us_before2000 <- ggmap(mapping_us) + 
  geom_point(data = dat_us %>% filter(., year(datetime) <= "2000", .preserve = FALSE), mapping = aes(x = longitude, y = latitude),
             shape = ".", alpha = 0.5, color = "blue")+ theme(legend.position= "none")+ ylab("Latitude") + xlab("Longitude")+ ggtitle("Jahr 2000")
map_us_before2000

map_us_before2005 <- ggmap(mapping_us) + 
  geom_point(data = dat_us %>% filter(., year(datetime) <= "2005", .preserve = FALSE), mapping = aes(x = longitude, y = latitude),
             shape = ".", alpha = 0.5, color = "blue")+ theme(legend.position= "none")+ ylab("Latitude") + xlab("Longitude")+ggtitle("Jahr 2005")
map_us_before2005

map_us_before2010 <- ggmap(mapping_us) + 
  geom_point(data = dat_us %>% filter(., year(datetime) <= "2010", .preserve = FALSE), mapping = aes(x = longitude, y = latitude),
             shape = ".", alpha = 0.5, color = "blue")+ theme(legend.position= "none")+ ylab("Latitude") + xlab("Longitude")+ ggtitle("Jahr 2010")
map_us_before2010

map_us_before2015 <- ggmap(mapping_us) + 
  geom_point(data = dat_us %>% filter(., year(datetime) <= "2015", .preserve = FALSE), mapping = aes(x = longitude, y = latitude),
             shape = ".", alpha = 0.5, color = "blue")+ theme(legend.position= "none")+ ylab("Latitude") + xlab("Longitude")+ggtitle("Jahr 2015")
map_us_before2015  


# arranging maps of different years in grid
map_us_development_density <- grid.arrange(map_us_before1990,map_us_before1995,map_us_before2000,
                                           map_us_before2005,map_us_before2010,map_us_before2015, 
                                           nrow=3, top = textGrob("Entwicklung der Verteilung von UFO-Sichtungen in den USA",gp=gpar(fontsize=14)))
