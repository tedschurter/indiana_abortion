# load libraries ####
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggtext)

# import data ####

# create list of yearly termination reports
files <- list.files("External Data/reports", full.names=T)

# loop import of sheet 4 (education levels) to create df with all years of education data

for(i in 1:length(files)) {
  ed <- read_xlsx(files[i],
                  sheet = 4) %>% 
    # extract year from filename
    mutate(
      year = str_extract(files[i], pattern = "\\d{4}"),
      .before = 'Education Level'
    ) 
  # adjust column names
  colnames(ed) <- c("Year", "Level", "Count", "Percent")
  # save and bind all years into one df
  if(i==1){
    ed_2 <- ed}
  else{
    ed_2 <- rbind(ed_2, ed)
  }
}
# rename df and clean up ####
ed <- ed_2
rm(ed_2)

# check number of years; should be 8
length(unique(ed$Year))  
# clean Levels ####
# check number of education levels, should be 9
length(unique(ed$Level))  
# is 21. 
# take a look at different levels used
unique(ed$Level)

# the same level is recorded in multiple ways; start with forcing all to lowercase
ed <- ed %>% 
  mutate(
    Level = str_to_lower(Level))

# solved some but still multiple levels for associate's, bachelors, masters

ed <- ed %>% 
  mutate(
    Level = recode(Level,
                   "unknown" = "Unknown",
                   "high school diploma or ged" = "High school diploma or GED",
                   "some college credit, no degree" = "Some college credit, no degree",
                   "associates" = "Associate degree",
                   "associate degree" = "Associate degree",
                   "associate's degree" = "Associate degree",
                   "bachelor's degree" = "Bachelor's degree",
                   "bachelors" = "Bachelor's degree",
                   "masters" = "Master's degree",
                   "master's degree" = "Master's degree",
                   "doctoral or professional degree" = "Doctoral or professional degree"
    )) 

# check number of education levels. should be 9            
length(unique(ed$Level))                   
# 9 as it should be. 

# set order of Level
ed$Level <- ed$Level %>%  
  factor(
    levels = c(
      "Unknown",
      "8th grade or less",
      "9th to 12th grade, no diploma",
      "High school diploma or GED",
      "Some college credit, no degree",
      "Associate degree",
      "Bachelor's degree",
      "Master's degree",
      "Doctoral or professional degree"
    ))

# convert year column to numeric

ed$Year <- as.numeric(ed$Year)

write_csv(ed, file = "Exported_Data/ed_ab.csv")


# use custom theme for plot####
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
                                  size = rel(.6),
                                  vjust = .6),
      
      plot.title = element_textbox_simple(size = rel(1.5), color = "gray25", 
                                          hjust = 0,lineheight = 1, family = "serif",face = "plain"),
      plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                             lineheight = 1,margin = margin(0.2, 0, .2, 0, unit = "cm"),),
      plot.caption = element_textbox_simple(family = "sans", size = rel(.7),
                                            color = "gray40", halign = 1,
                                            lineheight = 1.2),
      plot.title.position = "plot",
      plot.caption.position = "panel", 
      plot.margin = margin(.5, .5, .5,.5, unit = "cm")  
    )
}


# create line chart ####

# make dataframes for lines with and without color
top_ed <- ed %>% 
  filter(Level == "High school diploma or GED" |
        Level == "Some college credit, no degree")

top_ed_lab <- top_ed %>% filter(Year == 2021)

bottom_ed <- ed %>% 
  filter(Level != "High school diploma or GED" &
         Level != "Some college credit, no degree")

bottom_ed_lab <- bottom_ed %>% filter(Year == 2021)

# titles, caption, colors ####

# high school color
hsc <- "#5ab4ac"
# some college color
scc <- "#d8b365"

# set colors for top two categories
ed_cols <- c("High school diploma or GED" = hsc,
             "Some college credit, no degree" = scc)


title <- paste0(
  "<span style = 'color:black;'>Most Indiana women who get an abortion have not 
  completed higher levels of education.</span>")

subtitle <- paste0(
  "<span style = 'color:black;'>On average, <span style = 'color:",hsc,";'>",
  round(sum(ed$Count[ed$Level == "High school diploma or GED"])/sum(ed$Count)*100),
  "%</span> have a <span style = 'color:",hsc,";'>high school diploma or GED </span> and <span style = 'color:",scc,";'>",
  round(sum(ed$Count[ed$Level == "Some college credit, no degree"])/sum(ed$Count)*100),
  "%</span> have <span style = 'color:",scc,";'>some college credit, but 
  no degree.</span>")

caption <- "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
**Graphic:** Ted Schurter 2023"

#format y scale 
pct1 <- scales::percent_format(scale = 1)
# background color 
panel_c <- "#fdfdf2"

ggplot()+
  # top two categories
  geom_line(data = top_ed,
            aes(Year, Percent, color = Level, group = Level),
            show.legend = F, size = .65)+
  geom_text(data = top_ed_lab,
            aes(2021, Percent, color = Level),
            label = top_ed_lab$Level, show.legend = F,
            vjust = 0, hjust = -.15)+
  # remaining categories
  geom_line(data = bottom_ed,
          aes(Year, Percent, group = Level),
          show.legend = F, color = "gray")+
  geom_text_repel(data = bottom_ed_lab,
            aes(2021, Percent), xlim = 2021.25,
            label = bottom_ed_lab$Level, show.legend = F,
            vjust = 0.15, hjust = "left", color = "gray", size = 3,
            min.segment.length = .5, point.padding = 2.5)+
  scale_x_continuous(limits = c(2014, 2023),
                     breaks = seq(2014, 2023, by= 1),
                     labels = c("2014", "2015", "2016", "2017", "2018", "2019",
                                "2020", "2021", "", ""))+
  scale_color_manual(values = ed_cols)+
  # add grid lines
  geom_segment(aes(x = 2014, xend = 2021,
                   y = c(10,20,30,40), yend = c(10,20,30,40)),
               color = "gray80", size = .5, linetype = "dotted")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = panel_c),
    plot.background = element_rect(fill = panel_c),
  )+
  labs(title = title,
       subtitle = subtitle,
       caption = caption
       )
  


 ggsave(paste0("Plots/svg/ed_line.svg"),
        plot = last_plot(), width=3500,
        height = 1800, units = "px")
