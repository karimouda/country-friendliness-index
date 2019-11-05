## reviews-analysis.R
##
## This script was used to do the analysis and visualization of 
## the "Country Friendliness Index" research that was published on medium.com
##
## Author: Karim Ouda
##
## Date Created: 05.11.2019

# Set working directory
setwd("~/PUT_HERE_YOUR_BASE_WORKING_DIRECTORY")

# Change formating options to disable scientific notation 0.1e+10
options("scipen"=999)

# Load libraries
library("data.table")
library("tm")
library("ggplot2")
library("maps")
library("mapproj")
library("stringr")
library("stringi")
library("quanteda")
library("ggwordcloud")

#import helper functions
source("lib.R")

# Load data file from the data-mining phase, country-fi-reviews.csv is not shared in this repo
# due to Google Maps terms of service agreement
reviews_dt = fread("../data/country-fi-reviews.csv")

## Data cleaning and preparation


# quick analysis and checks
View(reviews_dt)
summary(reviews_dt)
hist(reviews_dt$place_review_score)
hist(reviews_dt$number_of_reviewers,xlim=c(0,100),breaks = 5000)
sort(table(reviews_dt$number_of_reviewers))

## clean reviews_dt + quality checks
nrow(reviews_dt)
nrow(reviews_dt[number_of_reviewers<10]) #16%
reviews_dt = reviews_dt[number_of_reviewers>=10]
nrow(reviews_dt)
hist(reviews_dt$place_review_score)
summary((reviews_dt$place_review_score))
#View(reviews_dt[place_review_score==2])
#View(reviews_dt[place_review_score==5])
boxplot(reviews_dt$place_review_score)



# generate a table that holds counts per country
counts_per_country_dt = reviews_dt[,.(count=.N),by=country][order(-count)]
hist(counts_per_country_dt$count)
nrow(counts_per_country_dt[count<10])


# only keep countries with 60 samples or more
list_of_countries_above_60_samples = counts_per_country_dt[count>=60]$country
reviews_dt = reviews_dt[country %in% list_of_countries_above_60_samples]



# check counts per continent
counts_per_continent_dt = reviews_dt[,.(count=.N),by=continent][order(-count)]
boxplot(reviews_dt$place_review_score)
summary(reviews_dt$place_review_score)

# Draw box plot for all data
draw_boxplot(reviews_dt,"Boxplot","Review score","Boxplot chart for all reviews ")


## Analysis & visualization


# Generate a table with min/max/means,quantiles of reviews per country
mmm_per_country_dt = reviews_dt[,.(min=min(place_review_score),q_25=quantile(place_review_score,0.25),
                                   mean_r=mean(place_review_score),
                                   q_75=quantile(place_review_score,0.75),
                                   max=max(place_review_score),
                                   n_reviews=mean(number_of_reviewers)),by=country]
# Order by mean desc
mmm_per_country_dt = mmm_per_country_dt[order(-mean_r)]
mmm_per_country_dt$rank = 1:nrow(mmm_per_country_dt)

# Save country aggregate data to file
fwrite(mmm_per_country_dt,"../data/country-friendliness-index.csv")

# Get table size and global mean of reviews
tab_len = nrow(mmm_per_country_dt)
global_mean_val = mean(reviews_dt$place_review_score)

# Get a sub-table of top 10 countries and plot country/means chart
ploting_table_dt = mmm_per_country_dt[1:10]
draw_countries_chart(ploting_table_dt,global_mean_val,"Country","Average Review Score","Top 10 countries by average reviews")

# Get a sub-table of top 10 countries and plot country/means chart
ploting_table_dt = mmm_per_country_dt[(tab_len-10):tab_len]
draw_countries_chart(ploting_table_dt,global_mean_val,"Country","Average Review Score","Bottom 10 countries by average reviews", annot_hjust_param = 3 )


# Get all countries ordered assending 
ploting_table_dt = mmm_per_country_dt[order(mean_r)]
# Attache rank to country name
ploting_table_dt$country = paste0(ploting_table_dt$country," #",ploting_table_dt$rank)
# Plot Country Friendliness Index 
draw_countries_chart(ploting_table_dt,global_mean_val,"Country","Average Review Score",
                     "Country Friendliness & Service Quality Index - 2019", 
                     annot_hjust_param = 2)



# aggregate continent reviews
mean_per_continent_dt = reviews_dt[,.(min_r=min(place_review_score),max_r=max(place_review_score),
                                 mean_r=mean(place_review_score),
                                 density_r=max(density(place_review_score)$y)),by=continent]

# Plot distribution density for all continents
ggplot(reviews_dt,
       aes(x=place_review_score, fill=continent)) +
        geom_density( alpha=.5, position="identity") +
        theme(legend.position= "top") +
        geom_text(data = mean_per_continent_dt, 
                  aes(x = mean_r, y = density_r, 
                      label = continent, vjust = 0)) 



# Is teh difference of top/bottom continentsstatistically significant ?
tt_result = t.test(reviews_dt[continent=="EU"]$place_review_score,
       reviews_dt[continent=="AF"]$place_review_score)

print(tt_result)
print(tt_result$p.value)

# Plot densities 
ggplot(reviews_dt[continent=="AF" | continent=="EU"],
       aes(x=place_review_score, fill=continent)) +
  geom_density( alpha=.5, position="identity") +
  theme(legend.position= "top") 

# Is the difference of top/bottom countries statistically significant ?
tt_result = t.test(reviews_dt[country=="Bosnia and Herzegovina"]$place_review_score,
                   reviews_dt[country=="Togo"]$place_review_score)

print(tt_result)
print(tt_result$p.value)

# Plot densities 
ggplot(reviews_dt[country=="Bosnia and Herzegovina" | country=="Togo"],
       aes(x=place_review_score, fill=country)) +
       geom_density( alpha=.5, position="identity") +
       xlab("Review scores") +
       ylab("Density") +
       ggtitle("Distribution of top & bottom countries")+
       theme(legend.position= "top") 


# Get world map data
world_map_data_dt = as.data.table(map_data('world'))





# Get mean reviews by country & plot world map
mean_review_score_dt = reviews_dt[,.(mean_rev_score=mean(place_review_score)),
                                  by=country]


draw_world_map(mean_review_score_dt,plot_title="Countries colored by average review score 1-5",
               legend_title="Average\nReview\nScore")


# Get mean number of reviews by country & plot world map
mean_n_of_reviews_dt = reviews_dt[,.(mean_rev_score=mean(number_of_reviewers)),
                                  by=country]

draw_world_map(mean_n_of_reviews_dt,plot_title="Countries by average number of reviews per cafe/restaurant",
               legend_title="Average\nNumber of\nReviews")


# Get mean review scores by continent & plot world map
mean_continent_review_score_dt = reviews_dt[,.(country=country,mean_rev_score=mean(place_review_score)),
                                  by=continent]

draw_world_map(mean_continent_review_score_dt,plot_title="Continents colored by average review score 1-5",
               legend_title="Average\nReview\nScore")



# Show top and worst places in terms of reviews
reviews_dt[place_review_score==5][order(-number_of_reviewers)]
reviews_dt[place_review_score==2][order(-number_of_reviewers)]


## Text Analytics

# Get rows with non empty reviews 
non_empty_review_text_dt = reviews_dt[text_of_five_reviews!=""]

# Calculate and how score group 
non_empty_review_text_dt$score_group = sapply(non_empty_review_text_dt$place_review_score,get_score_group)
table(non_empty_review_text_dt$score_group)

# normalize and clean review text
non_empty_review_text_dt$text_of_five_reviews = sapply(non_empty_review_text_dt$text_of_five_reviews,clean_review_text)

# concatenate review text that lies in similarscore_group
text_by_score_dt = non_empty_review_text_dt[,.(text=paste(text_of_five_reviews,collapse = " "))
                                            ,by=score_group]


# generate Tri-grames
ngrams_table = tokens_ngrams(tokens(text_by_score_dt$text),3,concatenator = " ")
# Remove heavy object to save memory
remove(text_by_score_dt)

# Constants
max_xgrams_to_return = 30
max_size = 2

top_xgrams_by_rev_score_dt = data.table()

# Get top tri-grams for the top score group
star_rows_dt = get_top_xgrams(ngrams_table$text1,max_xgrams_to_return)
star_rows_dt$score = "4.31 to 5"
# calculated size of text in worldcloud
star_rows_dt$size = star_rows_dt$N/max(star_rows_dt$N)
star_rows_dt$size[1:5] = max_size
# add new rows of this score group to main table
top_xgrams_by_rev_score_dt = rbind(top_xgrams_by_rev_score_dt,star_rows_dt)

# Get top tri-grams for 3.5 to 4.31 score group
star_rows_dt = get_top_xgrams(ngrams_table$text2,max_xgrams_to_return)
star_rows_dt$score = "3.5 to 4.31"
star_rows_dt$size = star_rows_dt$N/max(star_rows_dt$N)
star_rows_dt$size[1:5] = max_size
top_xgrams_by_rev_score_dt = rbind(top_xgrams_by_rev_score_dt,star_rows_dt)

# Get top tri-grams for 3 to 3.5 score group
star_rows_dt = get_top_xgrams(ngrams_table$text3,max_xgrams_to_return)
star_rows_dt$score = "3 to 3.5"
star_rows_dt$size = star_rows_dt$N/max(star_rows_dt$N)
star_rows_dt$size[1:5] = max_size
top_xgrams_by_rev_score_dt = rbind(top_xgrams_by_rev_score_dt,star_rows_dt)

# Get top tri-grams for 2 to 3 score group
star_rows_dt = get_top_xgrams(ngrams_table$text4,max_xgrams_to_return)
star_rows_dt$score = "2 to 3"
star_rows_dt$size = star_rows_dt$N/max(star_rows_dt$N)
star_rows_dt$size[1:5] = max_size
top_xgrams_by_rev_score_dt = rbind(top_xgrams_by_rev_score_dt,star_rows_dt)


# Rename columns
colnames(top_xgrams_by_rev_score_dt) = c("bi_grams","count","score_group","size")

# Visualize wordclouds

set.seed(1) #seed to fix worldcloud randomization

ggplot(top_xgrams_by_rev_score_dt,
       aes(label = bi_grams, size = size, color=factor(score_group))) +
       geom_text_wordcloud_area(eccentricity=0.5) +
       scale_size_area(max_size = 14) +
       facet_grid(rows=vars(score_group),switch="y",as.table=F) +
       scale_color_manual(values=c("#ff2a00","#ff7f00","#b3bf2f","#53bf2f")) +
       ylab("Review scores range") + ggtitle("Aggregated wordclouds by review scores range") +
       get_ggplot_theme_no_x_axis() +
       theme(strip.background =element_rect(fill="black"))+
       theme(strip.text = element_text(colour = 'white')) 

