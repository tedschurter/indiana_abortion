# load libraries
library(cowplot)
library(ggtext)
library(sf)
library(tigris)
library(tidyverse)
library(tidycensus)


# import abortion data ####
# import csv with rate and count of abortions by county from 2014:2021 and geometry data

per_cap <- read_csv("Exported_Data/per_cap.csv")

# filter years to 2017:2021

per_cap <- per_cap %>% filter(year %in% 2017:2021) %>% select (-geometry)

# import median income data from tidycensus with location data ####

# 5 year ACS data for Indiana counties and annual median household income: B19013_001 

in_med <- 
  get_acs(
    geography = "county",
    year = 2021,
    state = "IN",
    variables = "B19013_001",output = "tidy",geometry = TRUE)

# calculate best range for 3 classes of median income data ####

# interquartile range for median income 
iqr <- IQR(in_med$estimate) # 10085 
#mean 
mean(in_med$estimate) # 60869.28
# median 
median(in_med$estimate) # 59371
# standard deviation 
sd(in_med$estimate) # 10235
# quantiles
quantile(in_med$estimate)
# 0%       25%       50%       75%      100% 
# 42465.00  54328.75  59371.00  64414.00 104858.00 

# remove outliers beyond 1.5* interquartile range and recalculate

in_med2 <-  in_med %>% 
  filter(estimate >= (iqr*1.5)-quantile(in_med$estimate)[2]) %>% 
  # filters 1.5 interquartile range below quartile 1
  filter(estimate <= quantile(in_med$estimate)[4]+(iqr*1.5)) 

# filters 1.5 interquartile range above quartile 3

iqr2 <- IQR(in_med2$estimate) # 9453
# standard deviation
st_d_2 <- sd(in_med2$estimate)  # returns 7509
# mean
mn_2 <- mean(in_med2$estimate) # 59394
# median
median(in_med2$estimate) # 58743
# quantiles
quantile(in_med2$estimate)
#     0%     25%     50%     75%    100% 
#42465.0 54139.5 58743.0 63592.5 79126.0 

# add new column for income rating from low income (-li) to high income (-hi) 
# based on 1.5IQR filtered standard deviation

in_med <- in_med %>% mutate(
  inc_rating = case_when(
    estimate <= mn_2-st_d_2 ~ "-li",
    between(estimate, mn_2-st_d_2, mn_2+st_d_2) ~ "-mi",
    estimate > mn_2+st_d_2 ~ "-hi"
  ), .after = moe
)

#clean up 
rm(iqr, iqr2, st_d_2, mn_2, in_med2)

# calculate best range for 3 classes of abortion rate data ####
# standard deviation
ab_st_d <- sd(per_cap$rate)  # returns 1.53
# mean 
ab_mn <- mean(per_cap$rate) # 2.48
# median 
ab_median <- median(per_cap$rate) # 2.3
# calculate trimmed mean - 10% off high and low end
ab_t_mean <- mean(per_cap$rate, trim = .1) # 2.38
# interquartile range (distance between 25 and 75 percentiles)
ab_iqr <- IQR(per_cap$rate) # 1.9

# quantiles
quantile(per_cap$rate)
# 0%  25%  50%  75% 100% 
# 0.0  1.4  2.3  3.3 12.1

# remove outliers beyond 1.5* interquartile range and recalculate

per_cap2 <-  per_cap %>% 
  filter(rate >= (ab_iqr*1.5)-quantile(per_cap$rate)[2]) %>% 
  # filters 1.5 interquartile range below quartile 1
  filter(rate <= ab_iqr*1.5+quantile(per_cap$rate)[4])  
# filters 1.5 interquartile range above quartile 3

ab_iqr2 <- IQR(per_cap2$rate) # 1.5
# standard deviation with outliers trimmed
ab_st_d_2 <- sd(per_cap2$rate)  # returns .956
# mean with outliers trimmed 
ab_mn_2 <- mean(per_cap2$rate) # 2.92

# assign rating based on standard deviation with IQR*1.5 removed 

per_cap <- per_cap %>% mutate(
  ab_rate = case_when(
    annual_co_med <= ab_mn_2-ab_st_d_2 ~ "la",
    between(annual_co_med, ab_mn_2-ab_st_d_2, ab_mn_2+ab_st_d_2) ~ "ma",
    annual_co_med >= ab_mn_2+ab_st_d_2 ~ "ha"), .after = annual_co_med) 

# clean up 
rm(per_cap2, ab_st_d, ab_st_d_2, ab_median, ab_mn, ab_mn_2, ab_iqr, ab_iqr2, ab_t_mean)

# prepare for map ####

# import county geometry and join to totals and population ####

options(tigris_use_cache=TRUE)  # cache data

# import geometry for counties and GEOID
counties <- counties("IN", cb = T) %>% select(GEOID, geometry)

# make GEOID character in per_cap

per_cap$GEOID <- as.character(per_cap$GEOID)
# join county geometry with totals and population
per_cap <- left_join(per_cap, counties)

sf_per_cap <- st_sf(per_cap)

# join dataframes and merge ab_rate and inc_rate into one variable

# remove redundant geometry column from per_cap then join
inc_ab <- per_cap %>% select(-geometry) %>% 
  left_join(in_med, per_cap, by = "GEOID") %>% 
  select(-variable)

# add column for joint ranking

inc_ab <- inc_ab %>% mutate(rank = paste0(ab_rate, inc_rating),
                            .after = Name)


# clean up 
rm(counties, per_cap, in_med)

# create bivariate color scheme ####
# create tibble for colors 

# With a nod to Josh Stevens explanation of how to create a bivariate map color scheme
# to keep things straight, visualize 3 x 3 matrix with abortion increasing from
# left to right in three stages and income increasing from bottom to top. Each 
# square gets a code to make it easier to keep things straight. 

# a3 b3 c3
# a2 b2 c2
# a1 b1 c1

# colors 
col <- c(
  "#e8e8e8",  # a1  low abortion - low income       a1: la-li           
  "#b5c0da",  # b1 medium abortion - low income     b1: ma-li     
  "#6c83b5",  # c1 high abortion - low income       c1: ha-li    
  "#b8d6be",  # a2 low abortion - medium income     a2: la-mi
  "#90b2b3",  # b2 medium abortion - medium income  b2: ma-mi
  "#567994",  # c2 high abortion - medium income    c2: ha-mi
  "#73ae80",  # a3 low abortion - high income       a3: la-hi
  "#5a9178",  # b3 medium abortion - high income    b3: ma-hi
  "#2a5a5b")  # c3 high abortion - high income      c3: ha-hi

# positions of matrix
col_pos <- c(
  "a1",
  "b1",
  "c1",
  "a2",
  "b2",
  "c2",
  "a3",
  "b3",
  "c3")

# a description for easy reference 

desc <- c("low abortion - low income",        #a1
          "medium abortion - low income",     #b1
          "high abortion - low income",       #c1
          "low abortion - medium income",     #a2
          "medium abortion - medium income",  #b2
          "high abortion - medium income",    #c2
          "low abortion - high income",       #a3
          "medium abortion - high income",    #b3
          "high abortion - high income")      #c3

# a two category code to assign to categories (abortion rate and median income)

rank <- c("la-li", "ma-li", "ha-li", "la-mi", "ma-mi", "ha-mi", "la-hi", "ma-hi",
          "ha-hi")

# combine into tibble

biv_col <- tibble(pos = col_pos, color = col, rank = rank, desc=desc)

# join biv_color to inc_ab dataframe ####
# using standard deviation 1.5*IQR abortion rate

biv <- left_join(inc_ab, biv_col, by = "rank") %>% 
  select(-c(pos, desc, inc_rating))

#Use st_sf() to add crs to per_cap for mapping 

sf_biv <- st_sf(biv)

# clean up
rm(col, col_pos, desc, rank)

# calculate center of one county for each rate-income rating corner combination and assign x, y ####

# low abortion, low income county center point
lali_county <-  sf_biv %>% filter(year == 2021) %>% 
  select(Name, GEOID, rank, geometry) %>% 
  filter(rank == "la-li") %>% 
  slice(2) %>%  # can adjust slice to select needed la-li county
  # st_coordinates(geometry) %>% 
  st_centroid(geometry)

lali_county <- do.call(rbind, st_geometry(lali_county))

lali_county_x <- lali_county[1]
lali_county_y <- lali_county[2]

# high abortion, high income county center point
hahi_county <-  sf_biv %>% filter(year == 2021) %>% 
  select(Name, GEOID, rank, geometry) %>% 
  filter(rank == "ha-hi") %>% 
  slice(2) %>%  # can adjust slice to select needed la-li county
  # st_coordinates(geometry) %>% 
  st_centroid(geometry)

hahi_county <- do.call(rbind, st_geometry(hahi_county))

hahi_county_x <- hahi_county[1]
hahi_county_y <- hahi_county[2]

# high abortion, low income county center point
hali_county <-  sf_biv %>% filter(year == 2021) %>% 
  select(Name, GEOID, rank, geometry) %>% 
  filter(rank == "ha-li") %>% 
  slice(1) %>%  # can adjust slice to select needed la-li county
  # st_coordinates(geometry) %>% 
  st_centroid(geometry)

hali_county <- do.call(rbind, st_geometry(hali_county))

hali_county_x <- hali_county[1]
hali_county_y <- hali_county[2]

# low abortion, high income county center point
lahi_county <-  sf_biv %>% filter(year == 2021) %>% 
  select(Name, GEOID, rank, geometry) %>% 
  filter(rank == "la-hi") %>% 
  slice(1) %>%  # can adjust slice to select needed la-li county
  # st_coordinates(geometry) %>% 
  st_centroid(geometry)

lahi_county <- do.call(rbind, st_geometry(lahi_county))

lahi_county_x <- lahi_county[1]
lahi_county_y <- lahi_county[2]

# before plotting, add panel color
panel_c <- "#fdfdf2"


# create legend: ####
# build dataframe with rankings from abortion rate rank and median income rank
mk_leg <- inc_ab %>% select(rank, ab_rate, inc_rating, estimate, annual_co_med)
# join to color dataframe biv_col
mk_leg <- left_join(mk_leg, biv_col, by = "rank")
# convert character ranking code to numeric
mk_leg <- mk_leg %>% 
  mutate(ab_rate =
           case_when(
             ab_rate == "ha" ~ 3,
             ab_rate == "ma" ~ 2,
             ab_rate == "la"~ 1),
         inc_rating = 
           case_when(
             inc_rating == "-hi" ~ 3,
             inc_rating == "-mi" ~ 2,
             inc_rating == "-li" ~ 1)
  )
# create legend plot ####
legend <- ggplot()+
  geom_tile(
    data = mk_leg,
    mapping = aes(
      x = ab_rate,
      y = inc_rating,
      fill = color)
  ) +
  scale_fill_identity() +
  labs(
    x = paste0("Increasing abortion  ", "\U2192"),
    y = paste0("Increasing income    ", "\U2192")
  )+
  theme_void()+
  
  theme(
    axis.title.x = element_text(size = 8,
                                hjust = .8),
    axis.title.y = element_text(size = 8,
                                vjust = 0,
                                hjust = 1,
                                angle = 90),
    axis.text = element_blank()
  )+
  coord_fixed(ratio = 1:1)


# titles and captions ####
title <- "Indiana county abortion rate and median household income"
s_title <- "Average abortion rate per 1,000 females of childbearing age and median household income from 2017 to 2021.<br>"
caption <- "<br><br>**Abortion data**: in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/ <br>
**Income data**: 5 year American Community Survey: 2021 Median household income in the past 12 months.<br><br> Plot by Ted Schurter 2023"

# plot ####
p <- 
  ggplot()+
  geom_sf(data = sf_biv %>% filter(year == 2021),
          aes(fill=color),
          color = "white",
          size = 0.1,
          alpha = .95,
          show.legend = F)+
  scale_fill_identity()+
  # expand area around map to accomodate text labels
  coord_sf(xlim = c(-89, -84),
           ylim = c(37.75, 41.65),
           expand = TRUE)+
  theme_void()+
  theme(
    plot.margin = unit(c(.25, 1, 3, 1), "cm")
  )+
  #  main labels ####
labs(
  title = title,
  subtitle = s_title,
  caption = caption
)+
  # theme adjustments ####
theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  panel.background = element_rect(color = NA,
                                  fill = panel_c),
  plot.background  = element_rect(color = NA,
                                  fill = panel_c),
  plot.title = element_textbox_simple(
    size = 22, lineheight = 1, family = "serif", padding = margin(.5, 5, 3, 1)),
  plot.subtitle = element_textbox_simple(
    size = 14, lineheight = 1, family = "sans", padding = margin(2, 0, 1, 0)),
  plot.caption = element_textbox_simple(
    size = 10, lineheight = 1.2, family = "sans", padding = margin(.5, 0, 1, 0),
    halign = 1),
  plot.caption.position = "plot"
)+
  # map annotations #### 
# low abortion low income county curve
geom_curve( 
  aes(
    x = lali_county_x/1.009,
    xend = lali_county_x,
    y = lali_county_y*1.004,
    yend = lali_county_y),
  curvature = -.2,
  linewidth = .75,
  color = "dark gray",
  arrow = arrow(
    length = unit(.2, "cm")))+  
  # low abortion low income county text
  annotate("text", lali_county_x/1.005, lali_county_y*1.008,
           colour = "black",
           size = 3.5,
           label = "Light gray counties\nmean low abortion\nand low income",
           lineheight = .9,
           hjust=0
  )+
  # high abortion high income county curve
  geom_curve( 
    aes(
      x = hahi_county_x*1.009,
      xend = hahi_county_x,
      y = hahi_county_y/1.003,
      yend = hahi_county_y),
    curvature = .2,
    linewidth = .75,
    color = "dark gray",
    arrow = arrow(
      length = unit(.2, "cm")))+
  # high abortion high income county text
  annotate("text", hahi_county_x*1.019, hahi_county_y/1.001,
           colour = "black",
           size = 3.5,
           label = "Dark bluegreen counties\nmean high abortion\nand high Income",
           lineheight = .9,
           hjust = 0
  )+
  # high abortion, low income county curve
  geom_curve( 
    aes(
      x = hali_county_x*1.009,
      xend = hali_county_x,
      y = hali_county_y/1.004,
      yend = hali_county_y),
    curvature = -.2,
    linewidth = .75,
    color = "dark gray",
    arrow = arrow(
      length = unit(.2, "cm")))+
  # high abortion, low income county text
  annotate("text", hali_county_x*1.0137, hali_county_y/1.0075,
           colour = "black",
           size = 3.5,
           label = "Blue counties\nmean high abortion\nand low income",
           lineheight = .9,
           hjust = 0
  )+
  # low abortion, high income county curve
  geom_curve( 
    aes(
      x = lahi_county_x/1.009,
      xend = lahi_county_x/1.0025,
      y = lahi_county_y*1.004,
      yend = lahi_county_y),
    curvature = -.2,
    linewidth = .75,
    color = "dark gray",
    arrow = arrow(
      length = unit(.2, "cm")))+
  # low abortion, high income county curve  
  annotate("text", lahi_county_x/1.004, lahi_county_y*1.0085,
           colour = "black",
           size = 3.5,
           label = "Green counties\nmean low abortion\nand high income",
           lineheight = .9,
           hjust = 0
  )

# combine plot and legend and save
p2 <- 
  ggdraw() +
  draw_plot(p, 0, -.05, 1, 1) +
  draw_plot(legend, x=0.77, y=0.14, 0.13, 0.13, scale = 1.2)+
  theme(plot.background = element_rect(fill = panel_c, color = NA))


ggsave(filename = "Plots/biv_med_inc_ab.pdf",
       height = 11, width = 8.5, unit = "in",
       plot = p2)























