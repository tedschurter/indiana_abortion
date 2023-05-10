# load libraries ####
library(ggtext)
library(readr)
library(readxl)
library(tidyverse)


# import abortion count by age data for 2014 to 2-21 ####

files <- list.files(path = "External Data/reports",
                    full.names = T)

for (i in 1: length(files)){
  
  ab_age <-   read_xlsx(files[i], 
                        sheet = 1,
                        col_names = c('age', 'count', "percent"), 
                        skip = 1) %>% 
    
    mutate(year = as.numeric(str_extract(files[i], pattern = "\\d{4}")),
           .before = age) %>% 
    select(-percent)
  
  # colnames to lowercase
  
  colnames(ab_age) <- str_to_lower(colnames(ab_age))
  
  # save and bind to one dataframe (df)
  
  if (i == 1) {
    ab_age2 <- ab_age }
    else {
    ab_age2 <- rbind(ab_age, ab_age2)
  }
}

# rename ab_age2 to ab_age and remove unneeded df
ab_age <- ab_age2
rm(ab_age2)

# check for NA's
sum(is.na(ab_age))

# make sure we have all the years we need
unique(ab_age$year)

# time to look at age ranges and see what we're dealing with
unique(ab_age$age)
# duplicate of age ranges due to inconsistent formatting of hyphens bewteen ages
# remove all extra space around hyphens 
ab_age <- ab_age %>% mutate(
  new_age = str_replace_all(string = ab_age$age,pattern = "\\ - ", "-"),
  .after = age)
#
# check again
unique(ab_age$new_age)  
# some sorting out to do needed with age ranges - categories don't align neatly. 
#need to see side by side for easier comparison.

# create sequence in order to pivot ab_age and look at age ranges side by side
ab_age$num <- rep(seq(1:9),8)
# first remove original age column
ab_age$age <- NULL
# rename columns
colnames(ab_age) <- c("year", "age", "count", "num")
ab_age  %>% 
  pivot_wider(names_from = year,values_from = age) %>% View

# hard to compare across due to NAs necessitated by count column; hide count for viewing purposes
ab_age %>% select(-count) %>% 
  pivot_wider(names_from = year, values_from = age) %>% View

# multiple versions of 45 or greater than 45. reduce to 45 and can refer to 
# 'more than' aspect of that via labeling
# youngest age in each year also varies. age groups don't become 
# consistent until the range of 18-19 years. 
ab_age %>% filter(num == 1)
# youngest age range includes
# three 10-14
# two <16
# two 9-13
# one 10-14 

# second youngest age range includes 
# 2 16-17
# 5 15-17
# first tackle 45 age issue by replacing all forms of 45 and older with 45
ab_age$age[ab_age$num==9] <- "45"

# youngest age group is at the top. need to manually order character factors
# by age.

ab_age$age <- factor(ab_age$age,
                     levels = 
                       c("9-13",  
                         "10-14", 
                         "12-14",
                         "15-17",
                         "< 16",
                         "16-17",
                         "18-19",
                         "20-24",
                         "25-29",
                         "30-34",
                         "35-39",
                         "40-44",
                         "45"),
                     ordered = T)

#remove num column used in pivot above

# write ab_age to csv
write_csv(ab_age, "Exported_Data/ab_age.csv")

# set custom theme ####
t_theme <- function(base_size = 10){
  
  theme_classic() %+replace%
    
    theme( 
      #grid elements
      panel.grid.major.y = element_line(color = "gray85", size = .25),
      #panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(), 
      axis.line = element_blank(),
      axis.ticks = element_blank(), 
      
      #axis.line = element_line(color = "gray90"),
      #axis.line.y = element_blank(),
      
      axis.text = element_text(color = "gray55",
                               size  = rel(.65)),
      axis.title.x = element_text(color = "gray55",
                                  size = rel(.65), hjust = 0),
      axis.title.y = element_text(color = "gray55",
                                  size = rel(.6)),
      
      plot.title = element_textbox_simple(size = rel(1.5), color = "gray25", 
                                          hjust = 0,lineheight = 1, family = "serif",face = "plain", margin = unit(c(0, .2, 8, 0), "pt")),
      plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                             lineheight = 1.1,margin = margin(0.2, .2, .2, 0, unit = "cm"),),
      plot.caption = element_textbox_simple(family = "sans", size = rel(.7),
                                            color = "gray40", halign = 1,
                                            lineheight = 1.2),
      plot.title.position = "plot",
      plot.caption.position = "panel", 
      plot.margin = margin(.5, .5, .5,.5, unit = "cm") 
    )
}

# calculate percent of age group for each year ####
ab_age <- ab_age %>% 
  group_by(year) %>% 
  mutate(pct= round(100*(count/sum(count)),2))


# write ab_age to csv
write_csv(ab_age, "Exported_Data/ab_age.csv")


# color
panel_c <- "#fdfdf2"

# titles, labels and captions ####
title <- paste0("Women in their 20s accounted for ",
                round(100*(ab_age %>% group_by(age) %>% filter(age == "20-24" | age == "25-29" ) %>% 
                ungroup() %>%  summarise(sum(count)))/(ab_age %>% ungroup() %>% summarise(sum(count)))),
                "% of all abortions from 2014 to 2021, while those between 20 and 34 years old made 
                up ",
                round(100*(ab_age %>% group_by(age) %>% filter(age == "20-24" | age == "25-29" | 
                age == "30-34") %>% ungroup() %>%  summarise(sum(count)))/
                (ab_age %>% ungroup() %>% summarise(sum(count)))),
                "%.")

subtitle <- paste0("The consolidation of the youngest age ranges starting in 2020 obscures the incidence of
            abortion for the very young. There was an average of ",
            round(ab_age %>% filter(age == "12-14" | age == "10-14" | age == "9-13") %>% ungroup()
            %>%  summarise(mean(count))),
            " abortions annually for nine to 14 year-olds from 2014 to 2019, ",
            round(ab_age %>% filter(age == "12-14" | age == "10-14" | age == "9-13") %>% ungroup() %>%  summarise(sum(count))/
            ab_age %>%filter(year != 2020 & year != 2021) %>% ungroup() %>% summarise(sum(count))*100,2),
            "% of the total.<br><br><br>")

#
y_labs <- c(
    "9-13",  
    "10-14", 
    "12-14",
    "15-17",
    "< 16",
    "16-17",
    "18-19",
    "<span style='color:gray25;'>**20-24**</span>",
    "<span style='color:gray25;'>**25-29**</span>",
    "30-34",
    "35-39",
    "40-44",
    "45 and up ")
#

# plot showing distribution of abortions by age ranges
ggplot(ab_age) + 
  geom_tile(aes(as.factor(year), age, fill = pct, width = 1),
            color = NA)+
  scale_fill_distiller(palette = "Greens",
                       direction = 0,
                       breaks = c(10, 20, 30),
                       labels = c("10%","20%","30%"))+
  scale_y_discrete(labels = y_labs,
                   name = "Age",)+
  scale_x_discrete(limits = factor(c(2014:2022)),
                   breaks = c(2014, 2015, 2016,2017,2018,2019,2020,
                              2021, 2022),
                   labels = c("2014", "2015", "2016","2017","2018","2019","2020",
                              "2021", "")
                   )+
  labs(title = title,
       subtitle = subtitle,
       fill = "Percent of\nabortions",
       )+
  guides(fill=guide_colourbar(title.vjust=.85))+
  # line marking 14 and under 2019
  geom_segment(aes(
           x = 6.55, xend = 6.55,
           y = .5, yend = 3.4),
           color = "gray85", linewidth = .25)+
  geom_text(aes(
    x = 6.65, y = 2),
    color = "gray55", size = rel(2.75), hjust = 0, fontface = "plain", lineheight = .9,
    label = paste0("Nine to 14 year-olds had\n", round(ab_age %>% filter(age == "12-14" | age == "10-14" | age == "9-13") %>% ungroup() %>%  summarise(sum(count))/
                ab_age %>%filter(year != 2020 & year != 2021) %>% ungroup() %>% summarise(sum(count))*100,2),
                "% of all abortions from\n2014 to 2019."))+
  # line marking 20-29 
  geom_segment(aes(
    x = 8.55, xend = 8.55,
    y = 7.5, yend = 9.4),
    color = "gray85")+
  geom_text(aes(
    x = 8.65, y = 8.5),
    color = "gray25", size = rel(3.65), hjust = 0, fontface = "plain", lineheight = .9,
    label = paste0(round(100*(ab_age %>% group_by(age) %>% filter(age == "20-24" | age == "25-29" ) %>% 
                                ungroup() %>%  summarise(sum(count)))/(ab_age %>% ungroup() %>% summarise(sum(count)))),
                   "% of\nall abortions"))+
  t_theme()+
  theme(
    axis.text.y   = element_markdown(color = "dark gray", size = 8),
    #axis.title  = element_blank(),
    axis.title.x  = element_blank(),
    axis.ticks  = element_blank(),
    axis.line   = element_line(color = "light gray", size = .25),
    plot.background  = element_rect(color = NA, fill  = panel_c),
    panel.background = element_rect(color = NA, fill = panel_c),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect(color = NA, fill = NA),
    legend.text = element_text(colour="gray40", size = 7),
    legend.title = element_text(colour="gray40", size = 8),
    legend.direction = "horizontal",
    legend.position = c(.78,1.075),
    plot.title = element_textbox_simple(size = rel(1.5), color = "gray25", 
                 hjust = 0,lineheight = 1, family = "serif", face = "plain", 
                 margin = unit(c(0, .2, 6, 0), "pt")),
    
  )
   

ggsave("Plots/age.svg", width = 11, height = 5.5, units = "in",
       plot = last_plot())
  
#  



