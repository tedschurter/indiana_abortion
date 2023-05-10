library(biscale)
library(cowplot)
library(ggtext)
library(sf)
library(tigris)
library(tidyverse)


# import provider lat and lon

prov <- read_csv("Exported_Data/prov_ll.csv") %>% 
  filter(year == 2021)

# 2020-2021 population totals ####
# categorical variables (age, sex) were not available via the api for 2021. found 
# them elsewhere, though as  csv:
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-18.csv
# 
# from their file layout document regarding years:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/cc-est2021-agesex.pdf
#   "The key for YEAR is as follows:
#    1 = 4/1/2020 population estimates base
#    2 = 7/1/2020 population estimate
#    3 = 7/1/2021 population estimate"

# create vector of ages needed
ages <- c("AGE2024_FEM", "AGE2529_FEM", "AGE3034_FEM")

# import data from census.gov 2020-2021 population estimates
pop_data <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-18.csv")

# filter to ages of 20:34-year-old females in 2021
pop_1 <- pop_data  %>% 
  select(c(county=CTYNAME,year=YEAR, paste(ages), COUNTY, STATE)) %>%  # select 
  # age groups for females of childbearing age only: 10-49
  filter(year != 1) %>% # filter out unnecessary estimate from April 2020 
  mutate(
    Name = str_extract_all(county, "(?<=^).*(?=\\s\\bCounty)"), # clean up county name
    year = if_else(year == 2, 2020, 2021), # assign right year
    GEOID = str_c(STATE, COUNTY)) %>% # combine STATE, YEAR FOR GEOID 
  pivot_longer(3:5, names_to = NULL, values_to = "fem_pop") %>% 
  group_by(Name, year, GEOID) %>% 
  summarise(fem_pop = sum(fem_pop)) %>%  # add populations for age groups only
  select(year, GEOID, Name, fem_pop) %>% 
  filter(year == 2021) %>% 
  ungroup()

# import count for all women of child bearing age in county
all_ages <- "POPEST_FEM"
#c("AGE1519_FEM","AGE2024_FEM","AGE2544_FEM","AGE4549_FEM")
pop_2 <- pop_data  %>% 
  select(c(county=CTYNAME,year=YEAR, paste(all_ages), COUNTY, STATE)) %>%  # select 
  # age groups for females of childbearing age only: 10-49
  filter(year != 1) %>% # filter out unnecessary estimate from April 2020 
  mutate(
    Name = str_extract_all(county, "(?<=^).*(?=\\s\\bCounty)"), # clean up county name
    year = if_else(year == 2, 2020, 2021), # assign right year
    GEOID = str_c(STATE, COUNTY)) %>% # combine STATE, YEAR FOR GEOID 
  pivot_longer(3, names_to = NULL, values_to = "fem_pop") %>% 
  group_by(Name, year, GEOID) %>% 
  summarise(all_age = sum(fem_pop)) %>%  # add populations for age groups only
  select(year, GEOID, Name, all_age) %>% 
  filter(year == 2021) %>% 
  ungroup() %>% 
  select(-year)
#
# check for NA's
sum(is.na( pop_1)) # returns 0
sum(is.na( pop_2)) # returns 0

# add fem_rte column for percent of population 20-34-years old; remove year
pop <- left_join(pop_1, pop_2, by = c("Name", "GEOID")) %>% 
  mutate(fem_rte = round(fem_pop/all_age,2)*1000) %>% 
  select(-year)

sum(is.na(pop$fem_rte)) # 0

# import 2021 abortion totals
ab <- read_csv("Exported_Data/2021_abortion_count.csv")

sum(is.na(ab$Count))

# convert ab$name to list from character
ab$Name <- as.list(ab$Name)

# join to population totals
pop_ab <- left_join(pop, ab, by = c("Name" = "Name")) 

# check for NAs 
anyNA(pop_ab) # returns TRUE

which(is.na(pop_ab), arr.ind=TRUE)

pop_ab[17,] %>% View 

# where is the problem

anti_join(pop, ab, by = c("Name" = "Name")) %>% View  # DeKalb
anti_join(ab, pop, by = c("Name" = "Name")) %>% View  # De Kalb

# De Kalb; in row 15 of ab dataframe needs to be replaced with DeKalb then re-join 
# reimport ab - need Name column to be list again

ab <- read_csv("Exported_Data/2021_abortion_count.csv")

# rename Dekalb
ab[15,1] <- "DeKalb"

# convert ab$name to list
ab$Name <- as.list(ab$Name)

# create new dataframe and calculate rate
pop_ab <- left_join(pop, ab, by = c("Name" = "Name")) %>%
  mutate(rate = round(1000*(Count/fem_pop),1)) 

anyNA(pop_ab) # FALSE


#clean up 
rm(pop, pop_1, pop_2, pop_data, ab)

# add geometry for mapping
# import county geometry for mapping
options(tigris_use_cache=TRUE)  # cache data

# import geometry for counties and GEOID
counties <- counties("IN", cb = T) %>% select(NAME, geometry)
#colnames(counties)  <- c("Name", "geometry")

pop_ab$Name <- as.character(pop_ab$Name)

# join county geometry to prov dataframe
pop_ab <- left_join(pop_ab, counties, by= c("Name" ="NAME"))

pop_ab <- st_sf(pop_ab)


# what are the ranges for our data?

#IQR for rate and fem_rte
pop_ab_r_iqr <- IQR(pop_ab$rate) 
pop_ab_r_iqr # 5.025
pop_ab_f_iqr <- IQR(pop_ab$fem_rte) 
pop_ab_f_iqr # 20

# distributions

ggplot(pop_ab)+
  geom_bar(aes(rate))

ggplot(pop_ab)+
  geom_bar(aes(fem_rte))

# potential outliers on the right for both rate and fem_rte

# how many rows don't have outlier data for rate
pop_ab  %>% 
  filter(rate >= quantile(pop_ab$rate)[2] - (pop_ab_r_iqr*1.5)) %>% 
  filter(rate <= quantile(pop_ab$rate)[3] + (pop_ab_r_iqr*1.5)) %>% 
  nrow() # 90

# look at rate distribution of filtered data; outliers in green
ggplot()+
  geom_bar(data = pop_ab,
           aes(rate), fill = "green")+
  geom_bar(data = pop_ab  %>% 
             filter(rate >= quantile(pop_ab$rate)[2] - (pop_ab_r_iqr*1.5)) %>% 
             filter(rate <= quantile(pop_ab$rate)[3] + (pop_ab_r_iqr*1.5)),
           aes(rate))


# how many rows don't have outlier data for fem_rte
pop_ab  %>% 
  filter(fem_rte  >= quantile(pop_ab$fem_rte)[2] - (pop_ab_f_iqr*1.5), 
         fem_rte  <= quantile(pop_ab$fem_rte)[3] + (pop_ab_f_iqr*1.5)) %>% 
  nrow() # 84

# distribution of filtered female population rate data; outliers in green
ggplot()+
  geom_bar(data = pop_ab,
           aes(fem_rte), fill = "green")+
  geom_bar(data  = pop_ab  %>% 
             filter(fem_rte  >= quantile(pop_ab$fem_rte)[2] - (pop_ab_f_iqr*1.5), 
                    fem_rte  <= quantile(pop_ab$fem_rte)[3] + (pop_ab_f_iqr*1.5)),
           aes(fem_rte))


# using the bi_scale package, compare the breaks using Jenks without and without outliers

# look at jenks breaks with and without outliers

# jenks breaks with outliers:
bi_class_breaks(pop_ab, fem_rte, rate, "jenks", clean_levels = T, split = F)
# $bi_x
# [1] "13-18" "18-23" "23-31"
# 
# $bi_y
# [1] "0-5.7"   "5.7-15"  "15-28.4"

# jenks breaks without outliers:
bi_class_breaks(pop_ab %>% 
                  filter(rate >= quantile(pop_ab$rate)[2] - (pop_ab_r_iqr*1.5), 
                         rate <= quantile(pop_ab$rate)[3] + (pop_ab_r_iqr*1.5),
                         fem_rte  >= quantile(pop_ab$fem_rte)[2] - (pop_ab_r_iqr*1.5), 
                         fem_rte  <= quantile(pop_ab$fem_rte)[3] + (pop_ab_r_iqr*1.5)),
                fem_rte, rate, "jenks", clean_levels = T, split = F)

# $bi_x
# [1] "13-16" "16-18" "18-23"
# 
# $bi_y
# [1] "0-4.4"    "4.4-8.3"  "8.3-13.8"

# looks like the option without outliers is better. Compare maps.


jnks_1 <- ggplot() +
  geom_sf(data = bi_class(pop_ab, x = fem_rte, y = rate, style = "jenks", dim = 3),
          mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3)+
  labs(title = "Jenks")+
  theme_void()

jnks_2 <-   ggplot() +
  geom_sf(data = bi_class(pop_ab %>% filter(rate >= quantile(pop_ab$rate)[2] - (pop_ab_r_iqr*1.5), 
                                            rate <= quantile(pop_ab$rate)[3] + (pop_ab_r_iqr*1.5),
                                            fem_rte  >= quantile(pop_ab$fem_rte)[2] - (pop_ab_f_iqr*2), 
                                            fem_rte  <= quantile(pop_ab$fem_rte)[3] + (pop_ab_f_iqr*2)),
                          x = fem_rte, y = rate, style = "jenks", dim = 3),
          mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3)+
  labs(title = "Jenks - outliers removed")+
  theme_void()

# compare maps with and without outliers
two_jnks_34 <- ggdraw()+
  draw_plot(jnks_1, 0,0,.5,1)+
  draw_plot(jnks_2, .5,0,.5,1)

# jenks with outliers removed looks more nuanced and better reflects the distribution 
# of abortion rates.

# return outlier rows to dataframe and add appropriate ratings for bi_class 


# remove geometry column for ease; will return it later




# create dataframe with outliers removed
pop_ab_f <- bi_class(pop_ab %>% filter(rate >= quantile(pop_ab$rate)[2] - (pop_ab_r_iqr*1.5), 
                                       rate <= quantile(pop_ab$rate)[3] + (pop_ab_r_iqr*1.5),
                                       fem_rte  >= quantile(pop_ab$fem_rte)[2] - (pop_ab_f_iqr*2), 
                                       fem_rte  <= quantile(pop_ab$fem_rte)[3] + (pop_ab_f_iqr*2)),
                     x = fem_rte, y = rate, style = "jenks", dim = 3, keep_factors = T)



# use anti-join to create dataframe of the columns that were filtered out and then 
# use case_when to create bi_class rating using breaks identified above

# $bi_x
# [1] "13-16" "16-18" "18-23"
# 
# $bi_y
# [1] "0-4.4"    "4.4-8.3"  "8.3-13.8"


pop_ab_f$geometry <- NULL
pop_ab$geometry <- NULL

ol <- anti_join(pop_ab, pop_ab_f) %>% 
  mutate(bi_class = case_when(
    fem_rte < 16 ~ "1-",
    fem_rte %in% 16:18 ~ "2-",
    fem_rte > 18 ~ "3-"))%>% 
  mutate(bi_class = case_when(
    rate < 4.4 ~ paste0(bi_class,"1"),
    rate > 4.4 & rate < 8.3 ~ paste0(bi_class, "2"),
    rate > 8.3 ~ paste0(bi_class, "3")))

# join ol (outlier) dataframe to dataframe with outliers removed
pop_ab <- full_join(pop_ab_f, ol)

# add geometry back to dataframe
pop_ab <- left_join(pop_ab, counties, by= c("Name" ="NAME"))

pop_ab <- st_sf(pop_ab)

# background color
panel_c <- "#fdfdf2"

map <- ggplot() +
  geom_sf(data = pop_ab, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3)+
  labs(title = "Jenks - no outs")+
  theme_void()

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "  Rate 20-34 females ",
                    ylab = "  Abortion rate",
                    size = 8,)+
  theme(
    plot.background = element_rect(fill = panel_c, color = panel_c,),
    panel.background = element_rect(fill = panel_c)
  )

legend


finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.62, .1, 0.2, 0.2)

finalPlot

prov <- read_csv("Exported_Data/prov_ll.csv")
# join county geometry to prov dataframe
prov <- left_join(prov, counties, by= c("county" ="NAME"))

prov <- st_sf(prov)
pts <- prov

# import abortion age data for headline purposes
ab_age <- read_csv("Exported_Data/ab_age.csv")



# primary plot  ####

# create x and y label coordinates ####
# low low classification
ll <- pop_ab %>% filter(bi_class == "1-1") %>% slice(6) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

ll <- do.call(rbind, st_geometry(ll))

ll_x <- ll[1] 
ll_y <- ll[2]

# low high 
lh <- pop_ab %>% filter(bi_class == "1-3") %>% slice(1) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

lh <- do.call(rbind, st_geometry(lh))

lh_x <- lh[1]
lh_y <- lh[2]

# 

# high low

hl <- pop_ab %>% filter(bi_class == "3-1") %>% slice(1) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

hl <- do.call(rbind, st_geometry(hl))

hl_x <- hl[1]
hl_y <- hl[2]

# high high

hh <- pop_ab %>% filter(bi_class == "3-3") %>% slice(5) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

hh <- do.call(rbind, st_geometry(hh))

hh_x <- hh[1]
hh_y <- hh[2]


map2 <- 
  ggplot() +
  geom_sf(data = pop_ab, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3)+
  ylim(37.85,41.75)+
  # low abortion low income county curve ####
geom_curve(
  aes(
    x = ll_x/.992,
    xend = ll_x-.2,
    y = ll_y*1.012,
    yend = ll_y),
  curvature = .2,
  linewidth = .5,
  color = "#6F7378",
  arrow = arrow(length = unit(.2, "cm")))+ #low abortion low income county text
  annotate("text", ll_x/.984, ll_y*1.02,
           colour = "#6F7378",
           size = 3.15,
           label = "Light gray counties:\nlow abortion, few\n20 to 34 year-olds.",
           lineheight = .9,
           hjust=0)+
  # low high curve and label
  geom_curve(
    aes(
      x = lh_x*.978,
      xend = lh_x,
      y = lh_y/1.001,
      yend = lh_y),
    curvature = -.28,
    linewidth = .55,
    color = "#6F7378",
    arrow = NULL)+#arrow(length = unit(.2, "cm")))+
  # low abortion low income county text
  annotate("text", lh_x*.981, lh_y/.995,
           colour = "#6F7378",
           size = 3.15,
           label = "Green counties:\nhigh abortion, few\n20 to 34 year-olds.",
           lineheight = .9,
           hjust=0
  )+
  # high low curve and label
  geom_curve(
    aes(
      x = hl_x*.985,
      xend = hl_x+.25,
      y = hl_y*1.015,
      yend = hl_y),
    curvature = -.3,
    linewidth = .5,
    color = "#6F7378",
    arrow = arrow(
      length = unit(.2, "cm")))+
  # low abortion low income county text
  annotate("text", hl_x*.987, hl_y*1.022,
           colour = "#6F7378",
           size = 3.15,
           label = "Blue counties:\nlow abortion, many\n20 to 34 year-olds.",
           lineheight = .9,
           hjust=0
  )+
  #high high curve and label
  geom_curve(
    aes(
      x = hh_x+.8,
      xend = hh_x+.25,
      y = hh_y,
      yend = hh_y),
    curvature = 0,
    linewidth = .5,
    color = "#6F7378",
    arrow = arrow(
      length = unit(.2, "cm")))+
  # high abortion high 20-34 county text
  annotate("text", hh_x+.88, 
           hh_y,
           colour = "#6F7378",
           size = 3.15,
           label = "Dark green counties:\nhigh abortion, many\n20 to 34 year-olds.",
           lineheight = .9,
           hjust=0
  )+
  # blank annotation to extend right margin (xlim removes annotations and curves)
  annotate("text", hh_x/1.027, hh_y*1.013,
           colour = "#6F7378",
           size = 3.15,
           label = "",
           lineheight = .9,
           hjust=0
  )+
  # blank annotation to extend left margin (xlim removes annotations and curves)
  annotate("text", hh_x*1.043, hh_y*1.013,
           colour = "#6F7378",
           size = 3.15,
           label = "",
           lineheight = .9,
           hjust=0
  )+
  theme_classic()+
  theme(
    plot.background = element_rect(fill=panel_c, color = panel_c),
    panel.background = element_rect(fill=panel_c, color = panel_c),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_textbox_simple(family = "serif",
                                        size = 14, lineheight = 1, color = "gray30",
                                        margin = unit(c(0, 0, 8, 0), "pt")),
    plot.subtitle = element_textbox_simple(family = "sans", size = 11,
                                           color = "gray40", 
                                           margin = unit(c(6, 0, 0, 0), "pt")),
    plot.caption = #element_markdown(color = "gray40", size = 7),
      element_textbox_simple(family = "sans", size = 7,
                             color = "gray40",
                             halign = 1,
                             lineheight = 1.2
      ),
    plot.title.position = "panel",
    plot.margin = margin(0, .5, 0, .5, unit = "cm")
  )+
  labs(
    subtitle = paste0("Women in this age range are ", 
                      round(sum(pop_ab$fem_pop)/sum(pop_ab$all_age)*100),
                      "% of the female population and receive ",
                      round(ab_age %>% filter(num %in% 4:6 & year == 2021) %>% summarise(sum(pct))),
                      "% of all abortions."),
    title = paste0(broman::spell_out(pop_ab %>% filter(bi_class == '3-3') %>% nrow(), capitalize = TRUE),
                   " Indiana counties have a high rate of abortion and of women 
               between 20 and 34 years-old; ",
                   broman::spell_out(pop_ab %>% filter(bi_class == '1-1') %>% nrow(), capitalize = TRUE),
                   " counties have low rates of both."),
    caption = paste0("**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'
   'www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-18.csv'
   \n**Graphic:** Ted Schurter 2023"))

finalPlot2 <- ggdraw() +
  draw_plot(map2, x =0, y= 0.015, width = 1, height = 1) +
  draw_plot(legend, x= 0.71, y = .122, width = 0.22, height = 0.22)+
  theme(
    plot.background = element_rect(fill = panel_c, color = panel_c)
  )



ggsave("Plots/svg/biv_age_rate_05.svg", plot = finalPlot2,
       height = 6.35, width = 5.4, unit = "in")

# alternate plot ####

# create x and y label coordinates ####
# low low classification
ll <- pop_ab %>% filter(bi_class == "1-1") %>% slice(6) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

ll <- do.call(rbind, st_geometry(ll))

ll_x <- ll[1] 
ll_y <- ll[2]

# low high 
lh <- pop_ab %>% filter(bi_class == "1-3") %>% slice(1) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

lh <- do.call(rbind, st_geometry(lh))

lh_x <- lh[1]
lh_y <- lh[2]

# 

# high low

hl <- pop_ab %>% filter(bi_class == "3-1") %>% slice(1) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

hl <- do.call(rbind, st_geometry(hl))

hl_x <- hl[1]
hl_y <- hl[2]

# high high

hh <- pop_ab %>% filter(bi_class == "3-3") %>% slice(9) %>%  
  # use slice adjust county as needed
  st_centroid(geometry) %>% select(geometry) 

hh <- do.call(rbind, st_geometry(hh))

hh_x <- hh[1]
hh_y <- hh[2]


# plot ####
map <- 
  ggplot() +
  geom_sf(data = pop_ab, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3)+
  ylim(37.85,41.75)+
  #xlim(88.8,83.75)+
  # low abortion low income county curve ####
geom_curve(
  aes(
    x = ll_x/.992,
    xend = ll_x-.2,
    y = ll_y*1.000,
    yend = ll_y),
  curvature = .2,
  linewidth = .5,
  color = "#6F7378",
  arrow = arrow(length = unit(.2, "cm")))+ #low abortion low income county text
  annotate("text", ll_x/.984, ll_y*1.008,
           colour = "#6F7378",
           size = 3.15,
           label = "Light gray counties:\nlow abortion, few\n20 to 34 year-olds",
           lineheight = .9,
           hjust=0)+
  # low high curve and label
  geom_curve(
    aes(
      x = lh_x*.978,
      xend = lh_x,
      y = lh_y/1.009,
      yend = lh_y),
    curvature = 0,
    linewidth = .55,
    color = "#6F7378",
    arrow = arrow(length = unit(.2, "cm")))+
  # low abortion low income county text
  annotate("text", lh_x*.981, lh_y/1.015,
           colour = "#6F7378",
           size = 3.15,
           label = "Green counties:\nhigh abortion, few\n20 to 34 year-olds",
           lineheight = .9,
           hjust=0
  )+
  # high low curve and label
  geom_curve(
    aes(
      x = hl_x*.985,
      xend = hl_x+.25,
      y = hl_y*1.015,
      yend = hl_y),
    curvature = -.3,
    linewidth = .5,
    color = "#6F7378",
    arrow = arrow(
      length = unit(.2, "cm")))+
  # low abortion low income county text
  annotate("text", hl_x*.987, hl_y*1.021,
           colour = "#6F7378",
           size = 3.15,
           label = "Blue counties:\nlow abortion, many\n20 to 34 year-olds",
           lineheight = .9,
           hjust=0
  )+
  #high high curve and label
  geom_curve(
    aes(
      x = hh_x/.989,
      xend = hh_x-.2,
      y = hh_y*.992,
      yend = hh_y),
    curvature = -.3,
    linewidth = .5,
    color = "#6F7378",
    arrow = arrow(
      length = unit(.2, "cm")))+
  # high abortion high 20-34 county text
  annotate("text", hh_x/.979, 
           hh_y*.987,
           colour = "#6F7378",
           size = 3.15,
           label = "Dark green counties:\nhigh abortion, many\n20 to 34 year-olds",
           lineheight = .9,
           hjust=0
  )+
  # blank annotation to extend right margin (xlim removes annotations and curves)
  annotate("text", hh_x/1.045, hh_y*1.013,
           colour = "#6F7378",
           size = 3.15,
           label = "",
           lineheight = .9,
           hjust=0
  )+
  theme_classic()+
  theme(
    plot.background = element_rect(fill=panel_c, color = panel_c),
    panel.background = element_rect(fill=panel_c, color = panel_c),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_textbox_simple(family = "serif",
                                        size = 14, lineheight = 1, color = "gray30",
                                        margin = unit(c(0, 0, 6, 0), "pt")),
    plot.subtitle = element_textbox_simple(family = "sans", size = 11,
                                           color = "gray40"),
    plot.caption = #element_markdown(color = "gray40", size = 7),
      element_textbox_simple(family = "sans", size = 7,
                             color = "gray40",
                             halign = 1,
                             lineheight = 1.2
      ),
    plot.title.position = "panel",
    plot.margin = margin(0, .5, 0, .5, unit = "cm")
  )+
  labs(
    subtitle = paste0("Women in this age range are ", 
                      round(sum(pop_ab$fem_pop)/sum(pop_ab$all_age)*100),
                      "% of the female population and receive ",
                      round(ab_age %>% filter(num %in% 4:6 & year == 2021) %>% summarise(sum(pct))),
                      "% of all abortions."),
    title = paste0(broman::spell_out(pop_ab %>% filter(bi_class == '3-3') %>% nrow(), capitalize = TRUE),
                   " Indiana counties have a high rate of abortion and of women 
               between 20 and 34 years-old; ",
                   broman::spell_out(pop_ab %>% filter(bi_class == '1-1') %>% nrow(), capitalize = TRUE),
                   " counties have low rates of both."),
    caption = paste0("<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'
   'www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-18.csv'
   \n**Graphic:** Ted Schurter 2023"))

finalPlot <- ggdraw() +
  draw_plot(map, x =0, y= 0.015, width = 1, height = 1) +
  draw_plot(legend, x= 0.745, y = .1119, width = 0.22, height = 0.22)+
  theme(
    plot.background = element_rect(fill = panel_c, color = panel_c),
    plot.margin = margin(0,0,0,0)
  )


# ggsave("Plots/svg/biv_age_rate.svg", plot = finalPlot,
#        height = 7, width = 6, unit = "in")

