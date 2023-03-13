########## Patterns & Correlations in UFO witness reports worldwide #######
###########################################################################
####### crawling & scraping list of movies featuring extraterrestrials ####

# loading packages
library(webdriver)
library(htmltools)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# setting working directory
setwd("/Users/aliciadiem/Desktop/Digital Humanities")
options(stringsAsFactors = F)

# webscraping sci-fi-movie dataframe
install_phantomjs()
require(webdriver)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)
url <- "https://en.wikipedia.org/wiki/List_of_films_featuring_extraterrestrials"
require("rvest")
pjs_session$go(url)
rendered_source <- pjs_session$getSource()
html_document <- read_html(rendered_source)
tbls <- html_nodes(html_document, "table")
tbls_ls <- html_document %>% html_nodes("table") %>%.[1] %>% html_table(fill = TRUE)

# saving subset as dataframe
movies <- as.data.frame(tbls_ls) %>% subset(., select = -c(3,4))%>% rename(., "Titel" = "Title", "Jahr" = "Year")

# saving new dataframe as csv-file
write_csv(movies, "/Users/aliciadiem/Desktop/Digital Humanities/movies.csv")

# loading dataframe
movies <- read.csv("movies.csv", sep = ",", header = T, fill = T)
movies_number <- movies %>% group_by(Jahr)%>% summarize(Anzahl=n()) %>% arrange(.,Jahr,.by_group = FALSE)
movies_number['Medium']='Film' 
movies_number %>% select("Medium", everything())

# summary movies
summary(movies)

# plotting movie timeline
plot_movies <- movies_number %>% 
  ggplot(aes(x=Jahr, y=Anzahl))+
  geom_line(color="#066a67") +
  geom_hline(yintercept=mean(movies_number$Anzahl), color="darkgrey", linewidth=.5) +
  labs(x = "Jahr der Veröffentlichung", y = "Anzahl", title ="Filme mit Alienkontext")
plot_movies <- ggplotly(plot_movies)
plot_movies

# loading dataframe with sci-fi-books and their release dates
data_load1 <- read.csv("sf_aliens.csv", sep = ",", header = T, fill = T)
data_load2 <- read.csv("sf_space_opera.csv", sep = ",", header = T, fill = T)

books <- merge(data_load1, data_load2, all = TRUE) %>% select(Titel = Book_Title, AutorIn = Author_Name, Rating = Rating_score, Review = Review_number, Beschreibung = Book_Description, Jahr = Year_published)
books_number <- books %>% group_by(Jahr)%>% summarize(Anzahl=n()) %>% arrange(.,Jahr,.by_group = FALSE)%>% .[-1,] # deleting first row since there where two countings for year 0
books_number['Medium']='Buch' 
books_number %>% select("Medium", everything())

# descriptive summary 
range(books$Jahr)
View(books_number)

# plotting books timeline
plot_books <- books_number %>%
  ggplot(aes(x=Jahr, y=Anzahl))+
  geom_line(color="#10c5c0") +
  geom_hline(yintercept=mean(books_number$Anzahl), color="darkgrey", linewidth=.5) +
  labs(x = "Jahr der Veröffentlichung", y = "Anzahl", title ="Bücher mit Alienkontext")+
  xlim(1990, 2015)
plot_books <- ggplotly(plot_books)
plot_books

scifi_media <- merge(books_number, movies_number, all = TRUE)

# plotting timeline of sci-fi-media 1990-2015
plot_scifi_media <- ggplot(scifi_media, aes(x=Jahr, y=Anzahl, colour = Medium))+
  geom_line()+
  scale_color_manual(name = "Medium", values = c("#10c5c0", "#066a67"), 
                     guide = guide_legend(override.aes = list(size = 2, shape= 16, alpha =1)))+
  labs(x = "Jahr der Veröffentlichung", y = "Anzahl", title ="Science-Fiction-Medien mit Alienkontext (1990-2015)")+
  xlim(1990, 2015)
plot_scifi_media <- ggplotly(plot_scifi_media)
plot_scifi_media