## lib.R
##
## Helper functions for reviews-analysis.R
##
## Author: Karim Ouda
##
## Date Created: 05.11.2019

## Returns ggplot theme with empty x-axis
get_ggplot_theme_no_x_axis <- function(){
  
  return(  theme(legend.position = "none", 
                 axis.title.x = element_blank(),
                 axis.text.x=element_blank(),
                 text=element_text(size=12,family="Arial"),
                 panel.border = element_rect(colour = "black",fill = NA),  
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_line(colour = "#eeeeee"),
                 panel.background = element_rect(fill=NA),
                 axis.line = element_line(colour = "grey")))
}

## Returns a unified ggplot theme
get_ggplot_theme <- function(no_border=F){
  
  border_element = element_rect(colour = "black",fill = NA)
  
  if(no_border){
    border_element = element_blank()
  }
  
  return(  theme(legend.position = "none", 
                 text=element_text(size=11,family="Arial"),
                 panel.border = border_element,  
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_line(colour = "#eeeeee"),
                 panel.background = element_rect(fill=NA),
                 axis.line = element_line(colour = "grey")))
}

## Returns ggplot annotation - source text added to chart
get_ggplot_annotation <- function(hjust_arg=1.2,vjust_arg=-0.6,x_arg = -Inf, y_arg = Inf){
  return( annotate("text", x = x_arg, y = y_arg, label = "karim.ouda.net",
                   hjust=hjust_arg, vjust=vjust_arg, col="#aaaaaa", cex=3, alpha = 0.4))
}

## Draw ggplot boxplot
draw_boxplot <- function(reviews_dt,xtitle,ytitle,main_title){
  ggplot(reviews_dt,aes(y=place_review_score,fill="red")) + 
      geom_boxplot()  + 
      labs( x=xtitle, y=ytitle, title=main_title) +
      get_ggplot_theme_no_x_axis() +
      get_ggplot_annotation() +
      scale_fill_identity()
}

## Draws countries list chart using ggplot 
draw_countries_chart <- function(ploting_table_dt,global_mean_val,xtitle,ytitle,main_title,
                                 annot_hjust_param=1){
  
  ggplot(ploting_table_dt,
         aes(x=factor(country,levels = ploting_table_dt$country),y=mean_r,color=country)) +
        geom_point(aes(size=ploting_table_dt$n_reviews)) +
        geom_segment(aes(x=factor(country,levels = ploting_table_dt$country),y=q_25,
                         xend=factor(country,levels = ploting_table_dt$country),yend=q_75
        ),alpha=0.5,size=0.1,colour="black",linetype="solid")  +
        geom_hline(yintercept=global_mean_val,linetype="dashed", colour="#74b235")  +
        geom_text(aes(0,global_mean_val,label = "mean"),vjust = -0.7, hjust=0.5 , size=2.7, color="#74b235") +
        get_ggplot_theme(no_border = T) +
        get_ggplot_annotation(hjust_arg = annot_hjust_param) +
        labs( x=xtitle, y=ytitle, title=main_title) +
        coord_flip()
}


## Draws a worldmap using ggplot 
draw_world_map <- function(review_by_country_data_dt,plot_title="",legend_title=""){
  
  world_map_data_dt = as.data.table(map_data('world'))
  
  #find country names that are not consistent world map table
  #setdiff(unique(mean_review_score_dt$country),unique(world_map_data_dt$region))
  #world_map_data_dt$region[world_map_data_dt$region %like% "Gibraltar"]
  
  #make sure country names in our data matches "world map data"
  review_by_country_data_dt[country =="United States"]$country= "USA"
  review_by_country_data_dt[country =="United Kingdom"]$country= "UK"
  review_by_country_data_dt[country =="Vatican City"]$country= "Vatican"
  review_by_country_data_dt[country =="North Macedonia"]$country= "Macedonia"
  review_by_country_data_dt[country == "Trinidad and Tobago"]$country= "Tobago"
  review_by_country_data_dt[country == "Réunion"]$country= "Reunion"
  #Gibraltar ignored - since it is not visible in the map anyway
  
  ggplot() +
    geom_map(data = world_map_data_dt , map = world_map_data_dt,
             aes( group = group, map_id=region),
             fill = "white", colour = "#000000", size=0.5) +
    geom_map(data = review_by_country_data_dt, map=world_map_data_dt,
             aes(fill=mean_rev_score, map_id=country),
             colour="#000000", size=0.5) +
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
    labs(fill=legend_title, title=plot_title) +
    get_ggplot_annotation(x_arg = -120,y_arg =-60) +
    theme_void()+
    scale_fill_gradient(high="#56B1F7", low="#112233")
}


## Clean review text by removing special characters and stop words
clean_review_text <- function(text)
{
  
  text <- str_to_lower(text)
  
  #some stop words were given an exception because they are relevant to the data
  custom_stop_words_list = setdiff(stopwords("en"),c("not","so","very","too") )
  text <-  stri_replace_all_fixed(text,paste(" ",custom_stop_words_list," ",sep="") ," ",vectorize_all=FALSE)
  #text <-  stri_replace_all_fixed(text,paste(" ",stopwords("fr")," ",sep="") ," ",vectorize_all=FALSE)
  #text <-  stri_replace_all_fixed(text,paste(" ",stopwords("de")," ",sep="") ," ",vectorize_all=FALSE)
  
  
  removal_list <- c("&#39;","&amp;","&nbsp;","(",")","/","+",":",";","-",",","\"",".","#","/","$","*",",","?","!","'","&")
  text <- stri_replace_all_fixed(text,removal_list ," ",vectorize_all=FALSE)
  
  

  
  return(text)
}

## Get top repeated trigrams from an array - max_n is how many trigrams to return 
get_top_xgrams <- function(str_array,max_n){
  return(as.data.table(((sort(table(str_array),decreasing = T)[1:max_n]))))
}

## Calculate score group
get_score_group <- function(score){
  if ( score<2 | score > 5) 
    stop("Invalid argument !")
  
  if (score>=2 & score <3) return("2 to 3")
  else if (score>=3 & score <3.5) return("3 to 3.5")
  else if (score>=3.5 & score <4.31) return("3.5 to 4.31")
  else if (score>=4.31 & score <=5) return("4.31 to 5")
  
}