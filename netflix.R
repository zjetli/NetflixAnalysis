library(ggplot2) #Load all the required libraries
library(dplyr)
library(tidyverse)

netfl <- read.csv("C:/Users/HP/Desktop/netflix_titles/netflix_titles.csv")
summary(netfl)
colSums(is.na(netfl))

netflix <- na.omit(netfl)
colSums(is.na(netflix))    

view(netflix)

#Changed the date format to yyyy-mm-dd
netflix$date_added <- format(as.Date(netflix$date_added, "%d-%b-%y"), "%Y-%m-%d")

#Set theme to classic dark on light
theme_set(theme_bw())

#Some countries have multiple countries
#We extract the first country
#netfl$first_country <- gsub("([A-Za-z]+).*", "\\,", netfl$country)
#sub("\\|.*", "", str1)

netflix$first_country <- word(netflix$country,1,sep = "\\,")

plot_data1 <- netflix %>%
  group_by(first_country) %>% 
  count() %>%
  arrange(desc(n))
view(plot_data1)

top_ten <- head(plot_data1,10)

ggplot(top_ten, aes(x = reorder(first_country, -n), y = n )) +
  geom_bar(position = 'dodge',stat="identity", width = 0.5, fill="#940404") + 
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+
  #eom_text(aes(label=first_country), hjust=1.6, color="white", size=3.5)+
  labs(title="Bar Chart", 
       subtitle="Top 10 content creating Countries on netflix", 
       #caption="Source: Elo's nfl ratings data for the latest season",
       x = "",
       y = "Content created"
       ) +
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

#############################################################
#############################################################
#############################################################
#############################################################

plot_data2 <- netflix 
netflix$year_added <- format(as.Date(netflix$date_added, "%Y-%m-%d"), "%Y")

view(plot_data2)

plot_datax <- plot_data2 %>%
  group_by(year_added) %>%
  count(type) %>%
  filter(!is.na(type))
view(plot_datax)

plot_datax %>%
  ggplot() + 
  geom_area(aes(x=year_added, y=n, fill=type, group=type)) +
  scale_fill_manual(values=c("#940404", "#3a4749")) +
  labs(title="Area Plot", 
       subtitle="Movies and TV Shows added over time", 
       #caption="Source: Elo's nfl ratings data for the latest season",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6))


#############################################################
#############################################################
#############################################################
#############################################################

df_show_categories <- netflix %>% 
  select(c('show_id','type','listed_in')) %>% 
  separate_rows(listed_in, sep = ',') %>%
  rename(Show_Category = listed_in)
df_show_categories$Show_Category <- trimws(df_show_categories$Show_Category)
head(df_show_categories)

#############33333
df_unique_categories <- df_show_categories %>% 
  group_by(type,Show_Category) %>%  summarise()
df_category_correlations_movies <- data.frame(expand_grid(type = 'Movie', 
                                                          Category1 = subset(df_unique_categories, type == 'Movie')$Show_Category,
                                                          Category2 = subset(df_unique_categories, type == 'Movie')$Show_Category))

df_category_correlations_TV <- data.frame(expand_grid(type = 'TV Show', 
                                                           Category1 = subset(df_unique_categories, type == 'TV Show')$Show_Category,
                                                           Category2 = subset(df_unique_categories, type == 'TV Show')$Show_Category))

df_category_correlations <- rbind(df_category_correlations_movies,df_category_correlations_TV)
df_category_correlations$matched_count <- apply(df_category_correlations, MARGIN = 1,FUN = function(x) {
  length(intersect(subset(df_show_categories, type == x['type'] & Show_Category == x['Category1'])$show_id,
                   subset(df_show_categories, type == x['type'] & Show_Category == x['Category2'])$show_id))})


df_category_correlations <- subset(df_category_correlations, (as.character(Category1) < as.character(Category2)) & (matched_count > 0))
# Change plot size to 8 x 3
options(repr.plot.width=14, repr.plot.height=10)

ggplot(subset(df_category_correlations, type == 'Movie'), 
       aes(x = Category1, y = Category2, fill = matched_count)) + 
  geom_tile() + 
  facet_wrap( ~type, scales = 'free') + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(title="Area Plot for Genre Correlation", 
       subtitle="Independence movies tend to be Dramas", 
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6),legend.text = element_text(size = 14), legend.title = element_text(size = 16))
  


