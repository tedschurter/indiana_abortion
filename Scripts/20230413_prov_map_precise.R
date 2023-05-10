library(ggforce)
library(ggmap)
library(ggrepel)
library(ggtext)
library(sf)
library(tidyverse)
library(tigris)
library(osmdata)
library(rmapshaper)

# import provider location data  ####
prov <- read_csv("Exported_Data/prov.csv")


# total count of all abortions 2014:2021 ####
tot_count <- sum(prov$count_by_yr)

# calculate percent of all abortions at each facility 2014:2021 ####
prov <- prov %>% 
  group_by(facility) %>% 
  mutate(tot_fac_pct = round(100*(sum(count_by_yr)/tot_count),2)) 


# create dataframe of distinct facilities and their lat and lon; first arrange prov 
# dataframe in descending order to get most recent year address data was available
# for that facility. If it was pre 2018, the address was provided in the report.
# After 2017, only the county was provided so addresses will reflect what is returned
# when geolocating modern facility name today.
pd <- prov %>% arrange(desc(year)) %>% distinct(facility, .keep_all = T)

# if address provided, use it for geocoding, else generate address by combining
# facility name, county and state and geocode that instead.
pd[,(ncol(prov)+1):(ncol(prov)+2)] <- ggmap::geocode(
  location = 
    if_else(is.na(pd$address) == "FALSE", 
            pd$address,
            str_c(pd$facility, paste0(pd$county, " County, IN"), 
                  sep=", ")), output = "latlon")

# returns warning that "Sydney $ Louis..." returned address from Australia.
# To fix that, pull address from last known year for Sidney & Lois Eskenazi Hospital
sl <- prov$address[prov$facility == "Sidney & Lois Eskenazi Hospital" & prov$year == 2017]

#create object with lon and lat of correct address for hospital
sl_ll <- ggmap::geocode(location = sl, output = "latlon")

# add correct lon and lat to Sidney $ Lois...
pd$lon[pd$facility == "Sidney & Lois Eskenazi Hospital"] <- sl_ll[1]
pd$lat[pd$facility == "Sidney & Lois Eskenazi Hospital"] <- sl_ll[2]

# clean up
rm(sl, sl_ll)

# remove unneeded pd columns before join
pd <- pd %>% select(facility, lon, lat)

prov <- left_join(prov, pd, by = "facility")


# import county geometry for mapping
options(tigris_use_cache=TRUE)  # cache data

# import geometry for counties and GEOID
counties <- counties("IN", cb = T) %>% select(NAME, geometry)
#colnames(counties)  <- c("Name", "geometry")

# join county geometry to prov dataframe
prov <- left_join(prov, counties, by= c("county" ="NAME"))

prov <- st_sf(prov)
pts <- prov


# update: in plotting map, noticed Hamilton County lat was off. update:

prov$lat[prov$county == "Hamilton"] <- 39.965154

# convert lon and lat into numeric, not list
prov$lon <- as.numeric(prov$lon) 
prov$lat <- as.numeric(prov$lat) 

# creating combination state map w/insets of Marion, Lake and St. Joseph Counties ####
#
# get primary and secondary roads from TIGRIS
IN_roads <- primary_secondary_roads(state = "IN")

# get primary roads from TIGRIS
roads <- primary_roads()

# get polygon for Marion, Lake and St. Joseph Counties 
marion_poly <- getbb(place_name = "Marion County, IN",
                     format_out = "sf_polygon") 

lake_poly <- getbb(place_name = "Lake County, IN",
                     format_out = "sf_polygon")

sj_poly <- getbb(place_name = "St. Joseph County, IN",
                   format_out = "sf_polygon")


# check CRS of various components 
st_crs(prov) # 4269
st_crs(marion_poly) # 4326
st_crs(lake_poly) # 4326
st_crs(sj_poly) # 4326
st_crs(IN_roads) # 4269
st_crs(roads) # 4269
st_crs(counties) # 4269

# roads not compatible with county_polys or prov dataframe

# convert the roads to the right CRS ####
iroads <- st_transform(IN_roads, crs = 4326)
iroads2 <- st_transform(roads, crs = 4326)

# using mapshaper package, clip the roads to the boundaries of Marion County
mclip <- ms_clip(target = iroads,
                             clip = marion_poly, remove_slivers = T)

mclip2 <- ms_clip(target = iroads2,
                              clip = marion_poly, remove_slivers = T)


# clip boundaries for Lake County
lclip <- ms_clip(target = iroads,
                             clip = lake_poly, remove_slivers = T)

lclip2 <- ms_clip(target = iroads2,
                              clip = lake_poly, remove_slivers = T)

# clip boundaries for St. Joseph County
sjclip <- ms_clip(target = iroads,
                             clip = sj_poly, remove_slivers = T)

sjclip2 <- ms_clip(target = iroads2,
                              clip = sj_poly, remove_slivers = T)

# colors for facilities (fac) and hospitals (hosp) markers and panel
fac <- "#1a9641"
hosp <- "#fdae61"
panel_c <- "#fdfdf2"

#create inset map for Marion county ####

marion <- ggplot()+
  # add primary and secondary roads in gray
  geom_sf(data = mclip,
          color = "lightgray", alpha = .3)+
  # add primary interstates as orange/yellow
  geom_sf(data = mclip2,
          color = "#f6cf65", alpha = .3)+
  # add points
  geom_point(data=prov %>% distinct(facility, .keep_all = T) %>% 
               filter(!str_detect(facility, "Hospital") == TRUE,
                      county == "Marion"),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = fac,
             fill = fac,
             shape = 21,
             alpha = .3,
             show.legend = F)+
  geom_point(data=prov %>% distinct(facility, .keep_all = T) %>% 
               filter(str_detect(facility, "Hospital") == TRUE,
                      county == "Marion"),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = "white",
             fill = hosp,
             shape = 21,
             alpha = 1,
             show.legend = F)+
  # add boundary of Marion County
  geom_sf(data= counties %>% filter(NAME == "Marion"), 
          fill = NA,
          color = "darkgray",
          linewidth = .4)+
  theme_classic()+
  theme(
    plot.background = element_rect(fill=panel_c, color = panel_c),
    panel.background = element_rect(fill=panel_c, color = panel_c),
    plot.title = element_markdown(color = "gray50", lineheight = .1),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

# convert plot into grob
marion_grob <- ggplotGrob(marion)

# create inset map for Lake County ####
lake <- ggplot()+
  # add primary and secondary roads in gray
  geom_sf(data = lclip,
          color = "lightgray", alpha = .3)+
  # add primary interstates as orange/yellow
  geom_sf(data = lclip2,
          color = "#f6cf65", alpha = .3)+
  # add points
  geom_point(data=prov %>% distinct(facility, .keep_all = T) %>% 
               filter(!str_detect(facility, "Hospital") == TRUE,
                      county == "Lake"),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = fac,
             fill = fac,
             shape = 21,
             alpha = .3,
             show.legend = F)+
  geom_point(data=prov %>% distinct(facility, .keep_all = T) %>% 
               filter(str_detect(facility, "Hospital") == TRUE,
                      county == "Lake"),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = "white",
             fill = hosp,
             shape = 21,
             alpha = 1,
             show.legend = F)+
  # add boundary of Lake County
  geom_sf(data= counties %>% filter(NAME == "Lake"), 
          fill = NA,
          color = "darkgray",
          linewidth = .4)+
  theme_classic()+
  theme(
    plot.background = element_rect(fill=panel_c, color = panel_c),
    panel.background = element_rect(fill=panel_c, color = panel_c),
    plot.title = element_markdown(color = "gray50", lineheight = .1),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

# convert plot into grob
lake_grob <- ggplotGrob(lake)

# create inset for St. Joseph county ####

sj <- ggplot()+
  # add primary and secondary roads in gray
  geom_sf(data = sjclip,
          color = "lightgray", alpha = .3)+
  # add primary interstates as orange/yellow
  geom_sf(data = sjclip2,
          color = "#f6cf65", alpha = .3)+
  # add points
  geom_point(data=prov %>% distinct(facility, .keep_all = T) %>% 
               filter(!str_detect(facility, "Hospital") == TRUE,
                      county == "St. Joseph"),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = fac,
             fill = fac,
             shape = 21,
             alpha = .3,
             show.legend = F)+
  geom_point(data=prov %>% distinct(facility, .keep_all = T) %>% 
               filter(str_detect(facility, "Hospital") == TRUE,
                      county == "St. Joseph"),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = "white",
             fill = hosp,
             shape = 21,
             alpha = 1,
             show.legend = F)+
  # add boundary of St Joseph County
  geom_sf(data= counties %>% filter(NAME == "St. Joseph"), 
          fill = NA,
          color = "darkgray",
          linewidth = .4)+
  theme_classic()+
  theme(
    plot.background = element_rect(fill=panel_c, color = panel_c),
    panel.background = element_rect(fill=panel_c, color = panel_c),
    plot.title = element_markdown(color = "gray50", lineheight = 0),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

# convert plot into grob
sj_grob <- ggplotGrob(sj)

# create grob of state map ####

state <- ggplot()+
  # layer of all states
  geom_sf(data= counties, 
          fill = "white",
          color = "gray80",
          linewidth = .3)+
  # add border for counties with at least one non-hospital facility
  geom_sf(data= prov %>% distinct(facility, .keep_all = T), #%>% 
          # filter(!str_detect(facility, "Hospital") == TRUE),
          fill = NA, color = "darkgray",
          linewidth = .6)+
  geom_point(data=prov %>% arrange(desc(year)) %>% distinct(facility, .keep_all = T) %>% 
               filter(str_detect(facility, "Hospital") == FALSE &
                        year >=2020),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = fac,
             fill =  fac,
             #shape = 1,
             alpha = .3,
             show.legend = F)+
  # add points for hospitals
  geom_point(data=prov %>% arrange(desc(year)) %>% distinct(facility, .keep_all = T) %>% 
               filter(str_detect(facility, "Hospital") == FALSE &
                        year <=2020),
             aes(x=lon, y=lat,
                 size = tot_fac_pct),
             color = "white",
             fill = hosp,
             shape = 21,
             alpha = .5,
             show.legend = F)+
  # dummy layer to erase county borders for label clarity
  geom_label_repel(data=prov %>% distinct(county, .keep_all = T) %>% 
                     filter(county != "Lake", county != "Marion", county != "St. Joseph"),
                   aes(lon, lat, label = paste(county, "County", sep = "\n ")),
                   nudge_y = .15, nudge_x = -.4,
                   color = "white", segment.color = NA,
                   size = 2, fontface = "plain", lineheight = .8,
                   direction = "x"
  )+
  geom_text_repel(data=prov %>% distinct(county, .keep_all = T) %>% 
                    filter(county != "Lake", county != "Marion", county != "St. Joseph"),
                  aes(lon, lat, label = paste(county, "County", sep = "\n ")),
                  nudge_y = .15, nudge_x = -.4,
                  #force = 2,
                  color = "gray30", segment.color = "gray60",
                  size = 2, fontface = "plain", lineheight = .8,
                  direction = "x",
                  segment.size      = 0.2,
  )+
  theme(
    plot.background = element_rect(fill=panel_c),
    panel.background = element_rect(fill=panel_c, color = panel_c),
  )+
  theme_void()



state_grob <- ggplotGrob(state)

# create combination state and inset map ####

# create circle for key legend. x and y reference location placement
circle <- data.frame(x0 = .35:.1,    
                      y0 = .06:.06,
                      r = .016:.016)


# set blank canvas
map_inset <- ggplot() +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)

im <- 
  map_inset +
  # state map inset
  annotation_custom(state_grob,
                    xmin = 0, xmax = 0.6, ymin = 0,
                    ymax = 1)+
  # # Marion County inset
  annotation_custom(marion_grob,
                    xmin = 0.585, xmax = 1, 
                    ymin = 0, ymax = .25)+
  # line pointing to marion county inset
  geom_segment(aes(x = .4, xend = .67,
                   y = .46, yend = .21),
               color  = "gray60",
               linewidth = .2)+
  # Marion Co. inset label 
  annotate("text_box", x=.66, y=.26,
           label ="<span style = 'font-size:6pt;color:gray20'>Marion County:</span><span 
           style = 'font-size:6pt;color:gray60'> Indianapolis</span>",
           width = unit(1.4, "inch"), hjust = 0, vjust = .5, box.color="white", 
           fill = panel_c, size = 2.5)+
  # Lake County inset
  annotation_custom(lake_grob,
                    xmin = 0.67, xmax = .83, 
                    ymin = .255, ymax = .605)+
  # line pointing to Lake county inset
  geom_segment(aes(x = .19, xend = .67,
                   y = .80, yend = .5),
               color  = "gray60",
               linewidth = .2)+
  # Lake Co. inset label
  annotate("text_box", x=.66, y=.582,
           label ="<span style = 'font-size:6pt;color:gray20'>Lake County:</span><span 
           style = 'font-size:6pt;color:gray60'> Gary</span>",
           width = unit(1.4, "inch"), hjust = 0, vjust = .5, box.color="white", 
           fill = panel_c, size = 2.5)+
  # St. Joseph County inset
  annotation_custom(sj_grob,
                    xmin = 0.61, xmax = 1, 
                    ymin = .591, ymax = .851)+
  # line pointing to St. Joseph county inset
  geom_segment(aes(x = .38, xend = .67,
                   y = .87, yend = .75),
               color  = "gray60",
               linewidth = .2)+
  # St. Joseph inset label
  annotate("text_box", x=.66, y=.855,
           label ="<span style = 'font-size:6pt;color:gray20'>St. Joseph County:</span><span 
           style = 'font-size:6pt;color:gray60'> South Bend</span>",
           width = unit(1.4, "inch"), hjust = 0, vjust = .5, box.color="white", 
           fill = panel_c, size = 2.5)+
  # inset text block
  annotate("text_box", x=.66, y=.93,
           label ="The 3 counties with multiple abortion facilities are home to 
           large metropolitan populations",
           color = "gray40", lineheight = 1,
           width = unit(1.3, "inch"), hjust = 0, vjust = .5, box.color="white", 
           fill = panel_c, size = 2.5)+
  # create legend block
  annotate("text_box", x=.37, y=.06,
           label ="Size represents percent of all abortions.",
           color = "gray40", lineheight = 1,
           width = unit(1, "inch"), hjust = 0, vjust = .5, box.color="white", 
           fill = panel_c, size = 2.2)+
  geom_circle(data = circle, aes(x0 = x0, y0 = y0, r = r, col = r),
              color = "gray40",
              show.legend = F)+
  theme_void()+
  theme(
    plot.background = element_rect(fill=panel_c, color = panel_c),
    plot.title = element_textbox_simple(family = "serif",
                                        size = 14, lineheight = 1, color = "gray30"),
    plot.subtitle = element_textbox_simple(family = "sans", size = 11,
                                           color = "gray40"),
    plot.caption = element_textbox_simple(family = "sans", size = 6,
                                          color = "gray40"),
    plot.title.position = "panel",
    plot.margin = margin(0, .5, 0,.5, unit = "cm")
  )+
  labs(title = paste("Abortions were provided in 7 of Indiana's 92 counties from 2014 to 2021. 
  Nearly all, <span style = 'color:",fac,";'>99%</span>, 
  occured in a <span style = 'color:",fac,";'>clinic</span>; less than 
  <span style = 'color:",hosp,";'>1%</span> occured in a <span style = 
  'color:",hosp,";'>hospital</span>."),
       caption = "Data:<br>www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports
       <br><br> Graphic: Ted Schurter")



ggsave("Plots/png/prov_location_14_21.png", 
       plot = last_plot(), width = 5.4,height = 7.2, units = "in")

ggsave("Plots/svg/prov_location_14_21.svg", 
       plot = last_plot(), width = 5.4,height = 7.2, units = "in")






