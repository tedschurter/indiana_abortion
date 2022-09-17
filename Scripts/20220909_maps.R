library(ggplot2)
library(sf)
library(viridis)
library(rmarkdown)
library(ggtext)

# joining dataframes for map ------------------------------------


# before joining, check number of rows for each df

nrow(fem_co_pop) # 92 rows
nrow(report)     # 92 rows



report_pop <- left_join(fem_co_pop, report, by = "Name") %>% 
  mutate(Per_Capita = round(Count/(fem_pop/1000),2)) %>% 
  relocate(Count, .after = fem_pop) %>%
  relocate(Per_Capita, .after = Count) 

# check for na's

sum(is.na(report_pop$Count)) # one NA value

# where?

is.na(report_pop) # in row 76 entry for Dekalb County for both Count and Per_Capita. 

# checking Dekalb for each of the df's before the join. fem_co_pop lists Dekalb
# while report df lists "De Kalb"

# rename for consistency between two dataframes

report[15,1] <-  "DeKalb"

# redo join

report_pop <- left_join(fem_co_pop, report, by = "Name") %>% 
  mutate(Per_Capita = round(Count/(fem_pop/1000),2)) %>% 
  relocate(Count, .after = fem_pop) %>%
  relocate(Per_Capita, .after = Count) 

#
# check for na's

sum(is.na(report_pop$Count)) # 0


# creating data frames and objects for high and low metrics and names for labels -----------------------------------------------

# extract high and low county info including name, per capita and center of 
# geometry for annotating

high_co_name <- report_pop %>%   # name for highest rated county
  arrange(desc(Per_Capita)) %>% 
  slice(1) %>% 
  pull(Name)

#

high_co_pc <- report_pop %>%  # per capita rate for highest rated county
  arrange(desc(Per_Capita)) %>% 
  slice(1) %>% 
  pull(Per_Capita)

#

sec_high_co_name <- report_pop %>%  # name for 2nd highest rated county
  arrange(desc(Per_Capita)) %>% 
  slice(2) %>% 
  pull(Name)

#

sec_high_co_pc <- report_pop %>% # per capita rate for 2nd highest rated county
  arrange(desc(Per_Capita)) %>% 
  slice(2) %>% 
  pull(Per_Capita)

#

# add column named `center` to report_pop to identify center of county to add point 
# and label to on map

report_pop$center <- st_centroid(report_pop$geometry)

high_co_cntr <- report_pop %>%  # calculating center of county for point placement
  arrange(desc(Per_Capita)) %>% 
  slice(1) %>% 
  pull(center) %>% 
  sf::st_coordinates(report_pop$center)

# extract x and y for use in annotation

high_co_cntr_x <- high_co_cntr[, -2] # converting to form annotaton can use - x, y
high_co_cntr_y <- high_co_cntr[, -1] # converting to form annotaton can use - x, y

#

#lowest counties

report_pop %>%  # determine lowest ranked county(s)
  arrange(Per_Capita) %>% 
  slice(1:5) %>% 
  select(c(Name,Per_Capita)) 

# two counties tied for lowest with 0 abortions

lowest_co_pc <- report_pop %>%  # name for lowest rated county 1 of 2
  arrange(Per_Capita) %>% 
  slice(1) %>% 
  pull(Name)  

# second county tied at 0
lowest_co_pc2 <- report_pop %>% # name for lowest rated county 2 of 2
  arrange(Per_Capita) %>% 
  slice(2) %>% 
  pull(Name)  

# 

lowest_co_cntr <- report_pop %>%   # calculate center of first low county
  arrange((Per_Capita)) %>% 
  slice(1) %>% 
  pull(center) %>% 
  sf::st_coordinates(report_pop$center)

# extract x and y for use in annotation

low_co_cntr_x <- lowest_co_cntr[, -2]  # converting to form annotaton can use - x, y
low_co_cntr_y <- lowest_co_cntr[, -1]  # converting to form annotaton can use - x, y

#


lowest_co_cntr2 <- report_pop %>% # calculate center of second low county
  arrange((Per_Capita)) %>% 
  slice(2) %>% 
  pull(center) %>% 
  sf::st_coordinates(report_pop$center)

# extract x and y for use in annotation

low_co_cntr2_x <- lowest_co_cntr2[, -2] # converting to form annotaton can use - x, y
low_co_cntr2_y <- lowest_co_cntr2[, -1] # converting to form annotaton can use - x, y

#

# geom_sf_label requires dataframe 

# highest per capita data frame
high_co_label <- report_pop %>% 
  arrange(desc(Per_Capita)) %>% 
  slice(1) 

# lowest per capita data frame
low_co_label <- report_pop %>% 
  arrange(Per_Capita) %>% 
  slice(1)

# lowest per capita data frame 2 (two countes at 0 rate)
low_co_label_2 <- report_pop %>% 
  arrange(Per_Capita) %>% 
  slice(2)


# calculate difference between first and second high counties
per_cap_dif <- round(high_co_pc/sec_high_co_pc,2)
per_cap_dif <- round(per_cap_dif,2)
  
#

# calculate mean for per_capita rate for all counties
tcount <- report_pop  %>% 
  summarise(total_count = sum(Count)) %>% 
  pull(total_count)
#

# Create map --------------------------------------------------------------


ggplot(report_pop)+
  geom_sf(aes(fill=Per_Capita),
          color="#E0E0E0")+
  coord_sf(datum=NA)+
  scale_fill_viridis(option="G", 
                     direction=-1,
                     name = "Per 1,000 <br>females<br>of child-<br>bearing-age*")+
  theme_void()+
  theme(plot.title = element_markdown(hjust = 0),
        plot.subtitle = element_markdown(size = 10),
        plot.caption = element_markdown(size = 5,
                                        hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        legend.text = element_text(size = 7),
        legend.title = element_markdown(size = 6),
        legend.position = "left")+
  #plot.margin =unit(c(0.1,0.1,0.1,0.1), "cm"),
  #panel.spacing=unit(c(0.1,0.1,0.1,0.1), "cm"))+
  labs(title = "2021 county abortion rates in Indiana.",
       subtitle = paste0(high_co_name, " County had the highest rate of abortions<br>
       among the female child-bearing-age population while<br>", lowest_co_pc, " and ", lowest_co_pc2, 
                         " counties tied for the lowest rate."),   
       caption = "<p> *Child bearing age: 10 to 54-years-old. <br><br><i>Source<b>:<br> in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports <br>
        <br>US Census Bureau 2019 Population Estimates <br> <p>")
#

# Adding labels via annotations to map ------------------------------------

  
ggplot(report_pop)+
  geom_sf(aes(fill=Per_Capita),
          color="#E0E0E0")+
  coord_sf(datum=NA)+
  scale_fill_viridis(option="G", 
                     direction=-1,
                     name = "Per 1,000 <br>females<br>of child-<br>bearing-age*")+
  theme_void()+
  theme(plot.title = element_markdown(hjust = 0),
        plot.subtitle = element_markdown(size = 10),
        plot.caption = element_markdown(size = 5.5,
                                        hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        legend.text = element_text(size = 7),
        legend.title = element_markdown(size = 6),
        legend.position = "left")+
  labs(title = "2021 county abortion rates in Indiana",
       subtitle = paste("Among the population of child-bearing-age females,*<br> ", high_co_name, 
                        " County had the highest rate of abortions<br> while ", lowest_co_pc, 
                        " and ", lowest_co_pc2, " counties tied for the<br> lowest rate."),   
       caption = "<p> *Child bearing age: 10 to 54-years-old. <br><br><i>Source<b>:<br> in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports <br>
        US Census Bureau 2019 Population Estimates <br> <p>")+
  annotate("point", high_co_cntr_x, high_co_cntr_y,  
           colour = "antiquewhite4", 
           size = 1.3)+ # annotation point for high county 
  annotate("point", low_co_cntr_x, low_co_cntr_y,  
           colour = "antiquewhite4", 
           size = 1.3)+ # annotation point for low county 1
  annotate("point", low_co_cntr2_x, low_co_cntr2_y,  
           colour = "antiquewhite4", 
           size = 1.3)+ # annotation point for low county 2
  annotate("text", x = -84.45, y = 39.72, 
           label = paste(high_co_name,"\nCounty"), 
           size=2,
           colour="black") + # annotation text for high County
  annotate("text", x = -84.899, y = 38.55, 
           label = paste(lowest_co_pc,"\nCounty"), 
           size=2,
           colour="black") +  # annotation text for low County
  annotate("text", x = -87.89, y = 39.75, 
           label = paste(lowest_co_pc2,"\nCounty"), 
           size=2,
           colour="black") +
  annotate("segment", x = high_co_cntr_x, xend = -84.71, y = high_co_cntr_y, yend = 39.76612,
           colour = "antiquewhite4", size=.75, ) +
  annotate("segment", x = low_co_cntr_x, xend = -84.899, y = low_co_cntr_y, yend = 38.67,
           colour = "antiquewhite4", size=.75) +
  annotate("segment", x = low_co_cntr2_x, xend = -87.64, y = low_co_cntr2_y, yend = 39.8,
           colour = "antiquewhite4", size=.75) 

#

# Labels using geom_sf_label ----------------------------------------------

# alternate approach to labeling - doesn't require segment

# also adding title and subtitle objects to more easily control text wrapping

t_title <- "<br>Indiana's new, near-total abortion ban took effect Sept. 15, 2022, 
        making 2021 the last full year abortion was legal."
#
s_title <- paste("<br>There were", format(as.numeric(paste(tcount)), nsmall=0, big.mark=","),
                 "abortions performed on Indiana residents in 2021 with an average county rate 
        of ", pc_mean, "per 1,000 females of child-bearing age.* ", high_co_name, 
                 " County's rate of", high_co_pc, "was the highest and", per_cap_dif, "times higher than 
        the next highest county. ", lowest_co_pc, " and ", lowest_co_pc2, " counties 
        tied for the lowest rate.")

ggplot(report_pop)+
  geom_sf(aes(fill=Per_Capita),
          color="white")+
  coord_sf(datum=NA)+
  geom_sf_label(data=high_co_label, # label for highest county
                aes(label = Name),
                fill="#FFFFFF",
                fun.geometry = sf::st_centroid,
                nudge_x = .1,
                nudge_y = .15)+
  geom_sf_label(data=low_co_label, # label for lowest county 1
                aes(label = Name),
                fill="#FFFFFF",
                fun.geometry = sf::st_centroid,
                nudge_x = -.1,
                nudge_y  = .15)+
  geom_sf_label(data=low_co_label_2, # label for lowest county 2
                aes(label = Name),
                fill="#FFFFFF",
                fun.geometry = sf::st_centroid,
                nudge_x = .1,
                nudge_y = .15)+
  scale_fill_distiller(type="seq", palette = "BrBG",
                       "Abortions<br> per 1,000 <br>females*",
                       breaks = c(0, pc_mean, 3,6,9),
                       labels = c("0","- Average", "3", "6", "9"))+
  # scale_fill_viridis(option="G", 
  #                    direction=-1,
  #                    name = "Per 1,000 <br>females<br>of child-<br>bearing-age*")+
  theme_void()+
  theme(plot.title = element_markdown(hjust = 0,
                                      size = 16,
                                      family = "serif"),
        plot.subtitle = element_markdown(size = 10),
        plot.caption = element_markdown(size = 7,
                                        hjust = 1,
                                        lineheight = 1.25),
        plot.caption.position = "plot",
        plot.title.position   = "plot",
        legend.text = element_text(size = 8),
        legend.title = element_markdown(size = 7),
        legend.position = "left",
        legend.key.size = unit(1, units = "cm"),
        panel.background = element_rect(color = "#Fffff033",
                                        size = 3,
                                        fill = "#Fffff033"),
        plot.background  = element_rect(color = "gray88",
                                        size = 2,
                                        fill = "#Fffff033"),
        plot.margin  = margin(t = .5, r = 2, b = .05, l = 2, unit = "cm"))+
  labs(title =paste(strwrap(t_title, width = 65), collapse = " <br>"),
       subtitle = paste(strwrap(s_title, width = 80), collapse = " <br>"),
       
       caption = "<p> *Female of child-bearing age: 10 to 54-years-old. <br><br><i>Source<b>:<br> in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports <br>
        US Census Bureau 2019 Population Estimates <br> <p>")+
  annotate("point", high_co_cntr_x, high_co_cntr_y,
           colour = "white",
           size = 1.3)+ # annotation point for high county
  annotate("point", low_co_cntr_x, low_co_cntr_y,
           colour = "white",
           size = 1.3)+ # annotation point for low county 1
  annotate("point", low_co_cntr2_x, low_co_cntr2_y,
           colour = "white",
           size = 1.3)


ggsave("Plots/20220916_maps_01.pdf", width = 11, height = 8.5, units = "in")





