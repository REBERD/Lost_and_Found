library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

df = Lost_artworks <- read_csv("/Users/rebekaerdo/Documents/Digital Humanities/2023 Winter/Data Structures and Management/Final Project/Lost_artworks.csv")
View(df)

location = table(df$Location)
df_location = as.data.frame(location)
colnames(df_location) = c("Country", "Artworks")
ss_location = subset(df_location, Artworks > 2)
ggplot(ss_location, aes(x=reorder(Country, -Artworks), y = Artworks)) + 
  geom_bar(stat="identity",color="grey", fill= "grey") +
  ggtitle("Location") +
  theme(plot.title = element_text(hjust = 0.5))

culture = table(df$Culture)
culture
df_culture = as.data.frame(culture)
colnames(df_culture) = c("Origin", "Artworks")
ggplot(df_culture, aes(x=reorder(Origin, -Artworks), y = Artworks)) + 
  geom_bar(stat="identity",color="lightblue", fill= "lightblue") +
  scale_y_continuous(breaks=c(5,10,15,20,25,30,35,40,45,50,55,60,65,70)) +
  ggtitle("Artworks by Origin") +
  theme(plot.title = element_text(hjust = 0.5))

collector = table(df$Hungarian_collection)
df_collector = as.data.frame(collector)
colnames(df_collector) = c("Name", "Artworks")
ggplot(df_collector, aes(x="", y = Artworks, fill=Name)) + 
  geom_bar(stat="identity",width =1, color="white") +
  geom_text(aes(label = after_stat(y)),
             position = position_stack(vjust = 0.5)) +
  coord_polar(theta="y") +
  scale_fill_brewer(palette="Pastel1") +
  theme_void() +
  ggtitle("Artworks by Collections") +
  theme(plot.title = element_text(hjust = 0.5))


df1 = subset(df, df$Current_location == "Lost")
table = table(df1$Hungarian_collection)
df_table = as.data.frame(table)
colnames(df_table) = c("Collector", "Artworks")
ggplot(df_table, aes(x=Collector, y = Artworks)) + 
  geom_bar(stat="identity",color="darkblue", fill= "darkblue") +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12)) +
  ggtitle("Lost Artworks by Collectors") +
  theme(plot.title = element_text(hjust = 0.5))
  
barplot(table, 
        xlab = "The hungarian collectors",
        ylab = "The number of lost artworks",
        ylim = c(0,12),
        main = "Lost artworks by all collectors",
        col = "darkblue")

Owners = c("Lost", "Private", "Museums")
Artworks = c(29, 31, 99) 
df_owner = data.frame(Owners, Artworks)
ggplot(df_owner, aes(x=Owners, y=Artworks)) +
  geom_bar(stat = "identity", color="blue", fill= "blue") +
    ggtitle("Current Location of the Artworks") +
    theme(plot.title = element_text(hjust = 0.5))
  
