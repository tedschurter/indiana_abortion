library(patchwork)
library(readxl)
library(tidyverse)
library(tigris)

# import non-resident abortion totals data
# figures were copied by hand from charts into spreadsheet - no tables in reports

# 

nres_19 <- 
  read_xlsx("External Data/reports/2019_termination_data.xlsx",sheet = 14) %>% 
  mutate(year = 2019) %>% select(year, state, abortions)

nres_20 <- 
  read_xlsx("External Data/reports/2020_termination_data.xlsx",sheet = 14) %>% 
  mutate(year = 2020) %>% select(year, state, abortions)

nres_21 <- 
  read_xlsx("External Data/reports/2021_termination_data.xlsx",sheet = 14) %>% 
  mutate(year = 2021) %>% select(year, state, abortions)

nres <- rbind(nres_19, nres_20, nres_21)

rm(nres_19, nres_20, nres_21)

# export data
write_csv(nres, "Exported_Data/nonresident_ab_19_21.csv")


# plots ####

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
      axis.title.x = element_blank(),
      # element_text(color = "gray55",
      # size = rel(.65), hjust = 0),
      axis.title.y = element_blank(),
      # element_text(color = "gray55",
      #             size = rel(.6)),
      
      plot.title = element_textbox_simple(size = rel(1.5), color = "gray25", 
                                          hjust = 0,lineheight = 1, 
                                          margin = margin(0.2, 0, .2, 0, unit = "cm"),
                                          family = "serif",face = "plain"),
      plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                             lineheight = 1,margin = margin(0, 0, .4, 0, unit = "cm"),),
      plot.caption = element_textbox_simple(family = "sans", size = rel(.7),
                                            color = "gray40", halign = 1,
                                            lineheight = 1.2),
      plot.title.position = "plot",
      plot.caption.position = "panel", 
      plot.margin = margin(.5, .5, .5,.5, unit = "cm"),
      plot.background = element_rect(fill  = panel_c,
                                     color = panel_c),
      panel.background = element_rect(fill = panel_c,
                                      color = panel_c)
    )
}

# colors for charts

# background color 
panel_c <- "#fdfdf2"

# kentucky color
ky_c <-  "#fc8d59"
# ohio color
oh_c <-  "#4575b4"
# Tennessee color
tn_c <-  "#91bfdb"

s_cols <- c("Kentucky" = ky_c,
            "Illinois" = "gray",
            "Michigan" = "gray",
            "Ohio"     = oh_c,
            "Tennessee" = tn_c,
            "Other"    = "gray")


# import state geometry for inset map and assign colors 

# import states geometry  ####
states <- states(cb=T)

states <- states %>% filter(NAME == "Illinois" | NAME == "Michigan" | NAME == "Kentucky" |
                              NAME == "Tennessee" | NAME == "Indiana" | NAME == "Ohio")

# add colors to match bar chart #### 

states$color[states$NAME == "Kentucky"] <- ky_c 
states$color[states$NAME == "Ohio"] <- oh_c
states$color[states$NAME == "Tennessee"] <- tn_c
states$color[states$NAME == "Illinois" ] <- "gray"
states$color[states$NAME == "Michigan"] <- "gray"




# faceted bar chart by year, state ####

# on reordering variables within facets  https://juliasilge.com/blog/reorder-within/
nres %>% group_by(state) %>% arrange(desc(abortions)) %>% ungroup() %>% 
  mutate(
    year = as.factor(year),
    state2 = tidytext::reorder_within(state, by = rev(abortions), within = year)) %>%
  # added state2 as additional column to retain ability to scale_fill_manual via state column
  ggplot(aes(state2, abortions, fill = state))+
  geom_col(show.legend = F)+
  scale_fill_manual(values = s_cols)+
  facet_wrap(~year, scales="free_y")+
  coord_flip()+
  tidytext::scale_x_reordered()+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = .25),
    plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                           lineheight = 1,margin = margin(.25, 0, .4, 0, unit = "cm"),),
  )+
  labs(title =
         paste0("Indiana included non-resident abortion totals for individual 
      states starting in 2019.<span style = 'color:",ky_c,"'> Kentucky</span> 
      residents recieved the most non-resident abortions in Indiana while <span 
      style = 'color:",tn_c,"'>Tennessee</span>, which had the second-highest totals 
      in 2019, was the lowest state in 2021 and 2022."),
       subtitle = 
         paste0("<span style = 'color:",ky_c,"'> Kentucky</span> and 
      <span style = 'color:",tn_c,"'>Tennessee</span> enacted near-total abortion 
      bans after the Dobbs decision in 2022; <span style = 'color:",oh_c,"'>Ohio's
      </span>ban is currently blocked while a case proceeds. Illinois and Michigan 
      retain abortion rights.")
  )


# variation on above with room for inset map ####

# order states as factors

nres$state <- factor(nres$state, levels = c("Kentucky", "Illinois", "Michigan", "Tennessee",
                                            "Ohio", "Other"))

# assign to variable for combo plot
pt <- nres %>% group_by(state) %>% arrange(desc(abortions)) %>% ungroup() %>% 
  # mutate(
  #   year = as.factor(year),
  #   state2 = tidytext::reorder_within(state, by = rev(abortions), within = year)) %>%
  # added state2 as additional column to retain ability to scale_fill_manual via state column
  ggplot(aes(year, abortions, fill = state))+
  geom_col(show.legend = F)+
  scale_fill_manual(values = s_cols)+
  facet_grid(~state, scales="free_y", switch = "x")+
  #coord_flip()+
  #tidytext::scale_x_reordered()+
  t_theme()+
  theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0),
    
    plot.subtitle = element_textbox_simple(size = rel(1.1), color = "gray40", hjust = 0,
                                           lineheight = 1,margin = margin(.25, 0, .4, 0, unit = "cm"),),
  )


# inset map ####

# assign to variable for combo chart
p1 <- ggplot()+
  geom_sf(data = states,
          fill = states$color)+
  t_theme()+
  theme(
    axis.text = element_blank(),
  )

# inset text 

in_txt <- "<span style = 'color:#fc8d59;'> Kentucky</span> and
      <span style = 'color:#82b6d6;'>Tennessee</span> enacted near-total abortion
      bans after the Dobbs decision in 2022. Like Indiana, <span style = 'color:#376bae;'>Ohio's
      </span>ban is currently blocked while litigation proceeds. Illinois and Michigan
      retain abortion rights."

# inset text object
text1 <- ggplot(data = tibble(x = 0, y = 1, label = in_txt)) +
  aes(x = x, y = y, label = label) +
  geom_textbox(
    box.color = NA,
    color = "gray25",
    fill = NA,
    width = unit(10, "cm"),
    hjust = 0,
    vjust = 1 
  ) +
  scale_x_continuous(limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1))+
  theme_void() 

# final combo plot ####

pt + inset_element(p1, 
                   left = 0.6, 
                   bottom = 0.3, 
                   right = .95, 
                   top = 1,
                   align_to = "plot")+
  inset_element(text1, 
                left = 0.26, 
                bottom = 0.45, 
                right = .7, 
                top = .80,
                align_to = "plot")+
  
  plot_annotation(
    theme = t_theme(),
    title =
      "Indiana began reporting non-resident abortion totals for individual
      states starting in 2019.<span style ='color:#fc8d59;'> Kentucky</span>
      residents received the most overall. <span
      style ='color:#82b6d6;'>Tennessee</span> had the second-highest total
      in 2019 and the lowest in 2021.")

# ggsave("nonres_map.svg",
#        plot = last_plot(),
#        width = 3200, height = 1500, units = "px")


