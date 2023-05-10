library(ggtext)
library(tidyverse)


# import data
prov <- read_csv("Exported_Data/prov.csv")

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

# wrangle data ####


# calculate number of facility types by year ####

t1 <- prov %>% 
  group_by(year) %>% 
  summarise(tot = n_distinct(facility)) 

t2 <- prov %>% 
  ungroup() %>% 
  mutate(type = if_else(str_detect(facility, "Hospital") == TRUE, "H", "F")) %>% 
  filter(type == "F") %>% 
  group_by(year) %>% 
  summarise(ftot = n_distinct(facility))

t3 <- prov %>% 
  ungroup() %>% 
  mutate(type = if_else(str_detect(facility, "Hospital") == TRUE, "H", "F")) %>% 
  filter(type == "H") %>% 
  group_by(year) %>% 
  summarise(htot = n_distinct(facility))  


t4 <- left_join(t1, t2, by = "year")

t5 <- left_join(t3, t4, by = "year")

facility <- t5 %>% pivot_longer(
  cols = c(2:4), names_to = "type", values_to = "count")

facility$type <- factor(facility$type, levels = c("tot", "ftot", "htot"),
                        ordered = T)


# totals for captions ####
# total of all abortions 
tot <- facility %>%   
  filter(type == "tot") %>% #nrow
  summarise(sum(count))

# total of all abortions at non-hospital facilities
f_tot <- facility %>% 
  filter(type == "ftot") %>% 
  summarise(sum(count))

# total of all abortions at hospital facilities
h_tot <- facility %>% 
  filter(type == "htot") %>% 
  summarise(sum(count))

# percent of all abortions at non-hospital facilities
fpct <- round(f_tot/tot*100)  
# percent of all abortions at hospital facilities
hpct <- round(h_tot/tot*100)

# number of distinct hospitals
hosp_dist_tot <- prov %>% filter(str_detect(facility, "Hospital") == TRUE) %>% 
  distinct(facility, .keep_all = T) %>% nrow()

# total number of abortions
tot_ab <-  prov %>% ungroup() %>%  summarise(sum(count_by_yr))

# number of hospital abortions
hsp_ab <- prov %>% ungroup() %>% filter(str_detect(facility, "Hospital") == TRUE) %>% 
  summarise(sum(count_by_yr))

# percent of hospital abortions
hsp_ab_pct <- round(100*hsp_ab/tot_ab,1)

# add columns to prov dataframe:
# pct_by_yr: percent of all abortions at given location per year
# type = type of facility: hospital or clinic
# pct_type = percent of abortions that occurred at the given facility type; by year
# pct_fac = percent of facilities that are given type; by year
# tot_pct = total percent the abortions at x facility are of all abortions

prov <- prov %>% group_by(year) %>% 
  mutate(pct_by_yr = 100*(count_by_yr/sum(count_by_yr)),
         type = if_else(
           str_detect(facility, "Hospital") == TRUE,
           "Hospital", "Facility"
         )) %>%  
  group_by(year, type) %>%
  mutate(pct_type = sum(pct_by_yr),
         # to avoid total > 100 due to rounding, cap percent at 100
         pct_type = if_else(
           pct_type >=100, 100, pct_type)) %>%
  add_count(type) %>%
  group_by(year) %>%
  mutate(pct_fac = 100*(n/n_distinct(facility))) %>% 
  ungroup() %>% 
  group_by(facility, year) %>% 
  mutate(tot_pct = round(count_by_yr/tot_ab*100,2)) 


# clean up 
rm(t1, t2, t3, t4, t5)

# 

# colors ####
fac <- "#1a9641"
hosp <- "#fdae61"
tot_c <- "gray65"
panel_c <- "#fdfdf2"

# labels ####

type_lab <- c(ftot = "Facility",
              htot = "Hospital",
              tot = "Total")

# clinic colors - Marion Co. in color, everything else gray
mc_fac_lab <- c(
  "Affiliated Women's Services, Inc."            = "#a6cee3",
  "Clinic for Women"                             = "#33a02c",
  "The Women's Med Center of Indianapolis"       = "#b2df8a",
  "Planned Parenthood of Indianapolis"           = "#1f78b4",
  "Friendship Family Planning Clinic of Indiana" = "gray85",
  "Planned Parenthood of Bloomington"            = "gray85",
  "Planned Parenthood of Lafayette"              = "gray85",
  "Planned Parenthood of Merrillville"           = "gray85",
  "Women's Pavilion"                             = "gray85",
  "Whole Women's Health of South Bend"           = "gray85")

# hospital colors - Eskenazi in color, everything else gray

hosp_list <- c(
  "Indiana University Health University Hospital"  = "gray85", 
  "Sidney & Lois Eskenazi Hospital"                = "#fdae61",     
  "Indiana University Health North Hospital"       = "gray85",    
  "Indiana University Health Methodist Hospital"   = "gray85", 
  "Community Hospital North Surgery Center"        = "gray85",      
  "Indiana University Riley Hospital"              = "gray85",           
  "Deaconess Women's Hospital"                     = "gray85",
  "Indiana University Health West Hospital"        = "gray85")


# captions, titles and computed figures ####
caption <- "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
**Graphic:** Ted Schurter 2023"


# plot for number of facilities by type and year ####
#

#prov_plot <- 
ggplot()+
  geom_col(data = facility %>% filter(year != 2021),
           aes(year, count, fill = type,alpha = .5,),  
           position = position_dodge(0.8), 
           width = .65,  show.legend = F)+
  scale_fill_manual(values = c("ftot" = fac,
                               "htot" = hosp,
                               "tot"  = tot_c))+
  scale_x_continuous(breaks = c(2014:2021),
                     labels = c("2014", "2015","2016", "2017", "2018", "2019", 
                                "2020", "2021"))+
  scale_y_continuous(limits = c(0, 13.5),
                     breaks = seq(from = 0, to = 12, by = 4),
                     labels = c("0", "4", "8", "12"),
                     name = "Count\nof\nfacilities")+
  geom_text(data = facility %>% filter(year == 2021),
            aes(year, count, label = count, group = type),
            position = position_dodge(width = .8),
            hjust = .75, 
            vjust = -1, size = 2.75, color = "gray30")+
  geom_text(data = facility,
            aes(year, count, label = count, group = type),
            position = position_dodge(width = .8),
            hjust = .65, 
            vjust = 1.5, size = 2.25, color = "gray50")+
  geom_col(data = facility %>% filter(year == 2021),
           aes(year, count, fill = type),  
           position = position_dodge(0.8), width = .65, show.legend = F)+
  t_theme()+
  theme(
    panel.grid.major.y = element_line(color = "gray93"),
    panel.background = element_rect(fill = panel_c),
    plot.background = element_rect(fill = panel_c),
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = .5),
  )+
  labs(title = 
         paste0("The number of abortion<span style='color:",fac,"'> clinics</span> peaked in 2014
  but the number of <span style='color:",hosp,"'>hospitals</span> and the <span style= 
  'color:gray55'>total number</span> of facilities where abortions occurred peaked in 2021."),
       caption = caption)
#

#ggsave(filename = "Plots/fac_count_plot_1.svg", 
       # width = 10, height = 5.5, units = "in",
       # plot = last_plot())


# combo bar and line chart for percents of abortions and facilities ####

# labels for annotations

# abortion clinic label df
ldf <- data.frame(
  x = 2021, y = 90,
  label = paste0("The percent of abortions performed in <span style='color:",fac,"'>clinics</span> 
                 <br>has declined only a fraction since 2014."))

# abortion clinic facility label df
cl <- data.frame(
  x = 2014, y = 88,
  label = paste0("<span style='color:",fac,"'>Clinics</span> represented<br>",
                 round(prov$pct_fac[prov$year == 2014 & prov$type == "Facility" & 
                                      prov$county == "Tippecanoe"]),"% of abortion<br>facilities in 2014."))


ggplot()+
  geom_col(data = prov,
           aes(year, pct_fac, fill = type),
           position = position_dodge(0.8),
           width = .65, alpha = .5, show.legend = F)+
  geom_line(data = prov %>% group_by(year, type) %>% 
              summarise(n = sum(pct_by_yr)),
            aes(year, n, color = type, group = type),
            alpha = 1, linewidth = .75, show.legend = F)+
  # extension for left side of ab lines
  geom_segment(data = prov %>% filter(year == 2014) %>% group_by(year, type) %>% 
                 summarise(n = sum(pct_by_yr)),
               aes(x = 2013.62, xend = 2014,
                   y = n, yend = n, color = type, group = type),
               alpha = 1, linewidth = .75, show.legend = F)+
  # clinic abortion percent line label
  geom_richtext(data = ldf, aes(x, y, label = label), 
                size = 3, hjust = 1, lineheight = .85, fill = panel_c,
                label.color = NA, color = "gray 45", width = 3)+
  # clinic abortion percent line segment 
  # annotate("segment", x = 2020.8, xend = 2021.1,
  #          y = 89, yend = 96, color = "gray45", linewidth = .25)+
  # clinic abortion facility bar label
  geom_richtext(data = cl, aes(x, y, label = label), 
                size = 2.75, hjust = 0, lineheight = .85, fill = panel_c,
                label.color = NA, color = "gray 45")+
  # clinic facility percent line segment 
  annotate("segment", x = 2014.1, xend = 2014.4,
           y = 72, yend = 80, color = "gray45", linewidth = .25)+
  scale_fill_manual(values = c("Facility" = fac,
                               "Hospital" = hosp))+
  scale_color_manual(values = c("Facility" = fac,
                                "Hospital" = hosp))+
  scale_x_continuous(breaks = c(2014:2021),
                     labels = c("2014", "2015","2016", "2017", "2018", "2019", 
                                "2020", "2021"))+
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 50, 100),
                     labels = c("0", "50", "100%"),
                     name = "")+
  labs(title = 
    paste0("The percent of <span style='color:",fac,"'>clinics</span> versus <span 
    style='color:",hosp,"'>hospitals</span> varies annually but the rate of where
    most abortions occur is stable."), 
       subtitle = paste0("Nearly all abortions occur in a <span style='color:",fac,
                         "'>clinic</span>, only <span style='color:",hosp,"'>",hsp_ab_pct,"%</span>
      occur in a <span style='color:",hosp,"'>hospital</span>."),
       caption = caption)+
  t_theme()+
  theme(
    panel.background = element_rect(fill = panel_c),
    plot.background = element_rect(fill = panel_c),
    axis.title.x = element_blank(),
    legend.position = "none")

# ggsave(filename = "Plots/fac_ab_pct_plot_1.svg", 
#        width = 10, height = 5.5, units = "in",
#        plot = last_plot())

# 

# line chart for facilities 

ggplot()+
  # scale lines
  geom_segment(aes(x = 2014, xend = 2021,
                   y = c(0,20, 40), yend = c(0, 20, 40)),
               linetype = "dotted", color = "gray55")+
  
  # facilities NOT in Marion County - gray aesthetics
  geom_line(data = prov %>% filter(!county == "Marion" & type == "Facility")%>% 
              distinct(facility, .keep_all = T),  
            aes(year, pct_by_yr, group = facility, color = facility), 
            linewidth = .1, show.legend = F)+
  geom_text(data = prov %>% filter(county != "Marion" & type == "Facility" & 
                                     year == 2021) %>% distinct(facility, .keep_all = T), 
            aes(2021.10, pct_by_yr, 
                label = facility), color = "gray35",
            size = 2.75, hjust = 0, vjust = .2, show.legend = F)+
  # Women's Pavillion label closed in 2015;
  geom_text(data = prov %>% filter(facility == "Women's Pavilion" & year == 2015) %>% 
              distinct(facility, .keep_all = T), 
            aes(2015.10, pct_by_yr, 
                label = facility), color = "gray35",
            size = 2.75, hjust = 0, vjust = .2, show.legend = F)+
  # segment for Friendship Family Planning...
  geom_segment(aes(x = 2014, xend = 2014.5,
                   y = prov$pct_by_yr[prov$facility == "Friendship Family Planning Clinic of Indiana" & prov$year == 2014], 
                   yend = prov$pct_by_yr[prov$facility == "Friendship Family Planning Clinic of Indiana" & prov$year == 2014]),
               color = "gray85", size = .1)+
  # label for Friendship Family Planning...
  geom_text(data = prov %>% filter(facility == "Friendship Family Planning Clinic of Indiana") %>% distinct(facility, .keep_all = T), 
            aes(2014.6, pct_by_yr+3, 
                label = facility), color = "gray35",
            size = 2.75, hjust = 0, vjust = .4, show.legend = F)+
  # segment to label for Friendship Family Planning...
  geom_segment(aes(x = 2014.6, xend = 2014.8,
                   y = .35+prov$pct_by_yr[prov$facility == "Friendship Family Planning Clinic of Indiana" & prov$year == 2014], 
                   yend = 1.75+prov$pct_by_yr[prov$facility == "Friendship Family Planning Clinic of Indiana" & prov$year == 2014]),
               color = "gray35", size = .1)+
  
  # facilities in Marion County
  geom_line(data = prov %>% filter(county == "Marion" & type == "Facility") %>% 
              distinct(facility, .keep_all = T), 
            aes(year, pct_by_yr, group = facility, color = facility), size = .75,
            show.legend = F)+
  geom_text(data = prov %>% filter(county == "Marion" & type == "Facility" & 
                                     year == 2021) %>% distinct(facility, .keep_all = T), 
            aes(2021.10, pct_by_yr, 
                label = facility, color = facility),
            size = 2.75, hjust = 0, vjust = .25, show.legend = F)+
  # Affiliated Women's Services, Inc. open in 2014 only so doesn't appear in geom_line
  geom_segment(aes(x = 2014, xend = 2014.5,
                   y = prov$pct_by_yr[prov$facility == "Affiliated Women's Services, Inc." & prov$year == 2014], 
                   yend = prov$pct_by_yr[prov$facility == "Affiliated Women's Services, Inc." & prov$year == 2014]),
               color = "#a6cee3", size = 1, show.legend = F)+
  # label for Affiliated Women's Services
  geom_text(data = prov %>% filter(facility == "Affiliated Women's Services, Inc.") %>% distinct(facility, .keep_all = T), 
            aes(2014.6, pct_by_yr+.5, 
                label = facility, color = facility),
            size = 2.75, hjust = 0, vjust = .20, show.legend = F)+
  
  scale_color_manual(values = mc_fac_lab)+
  scale_x_continuous(limits = c(2014, 2023),
                     breaks = seq(from = 2014, to = 2023, by = 1),
                     labels = c("2014", "2015", "2016", "2017", "2018", "2019",
                                "2020", "2021", "", ""))+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  t_theme()+
  theme(
    panel.background = element_rect(fill = panel_c),
    plot.background = element_rect(fill = panel_c),
    panel.grid.major.y = element_blank(),#element_line(color = "gray90", linetype = "dashed"), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), #element_text(size = rel(.65),vjust = 1.05),
    legend.position = "none")+
  labs(title = 
         paste0("There were ", prov %>% ungroup() %>%  filter(type == "Facility") %>% 
               distinct(facility) %>% nrow()," distinct abortion clinics operating
               in ",prov %>% ungroup() %>%  filter(type == "Facility") %>% 
               distinct(county) %>% nrow(), 
               " Indiana counties between 2014 and 2021, ",
               100*(prov %>% ungroup() %>%  filter(county == "Marion" & type == "Facility") %>% 
               distinct(facility) %>% nrow())/(prov %>% ungroup() %>%  filter(type == "Facility") %>% 
               distinct(facility) %>% nrow()) ,
                "% were in Marion County and accounted for ",
               round(prov %>% ungroup() %>%  filter(county == "Marion" & type == "Facility") %>% 
               summarise(sum(tot_pct))),
               "% of all abortions."),
       subtitle = "The number of clinics reached nine in 2014 before shrinking to
       six by 2016. A clinic opened in South Bend in 2019, raising the total to seven.",
       caption = caption)

# line chart for hospitals

ggplot()+
  # lines for scale - 0, .5, 1%
  geom_segment(aes(x = 2014, xend = 2021,
                   y = c(0, .5, 1), yend = c(0, .5, 1)),
               linetype = "dotted", color = "gray55")+
  
  geom_line(data = prov %>% filter(facility != "Indiana University Health University Hospital" & 
            type == "Hospital") %>% distinct(facility, .keep_all = T), 
            aes(year, pct_by_yr, group = facility, color = facility), size = .75)+
  # labels for Marion Co. hospitals w/out Eskenazi with 2021 data - labels align at right
  geom_text(data = prov %>% ungroup() %>% arrange(desc(year)) %>% 
              filter(county == "Marion", type == "Hospital" & count_by_yr > 2 & facility 
                     != "Indiana University Health North Hospital" & facility != 	
                       "Indiana University Health University Hospital" &
                       facility != "Sidney & Lois Eskenazi Hospital") %>% distinct(facility, .keep_all = T),  
            aes(year + .1, pct_by_yr, label = facility), color = "gray35",
            size = 2.5, hjust = 0, vjust = .25)+
  # label for Eskenazi Hospital
  geom_text(data = prov %>% ungroup() %>% arrange(desc(year)) %>% 
              filter(facility == "Sidney & Lois Eskenazi Hospital") %>% distinct(facility, .keep_all = T),  
            aes(year + .1, pct_by_yr, label = facility, color = facility),
            size = 2.5, hjust = 0, vjust = .25)+
  # labels for non Marion Co. hospitals with 2021 data - labels align at right adjust up to avoid overlap
  geom_text(data = prov %>% ungroup() %>% arrange(desc(year)) %>% 
              filter(county != "Marion", type == "Hospital" & count_by_yr > 2 & facility 
                     != "Indiana University Health North Hospital" & facility != 	
                       "Indiana University Health University Hospital") %>% distinct(facility, .keep_all = T),  
            aes(year + .1, pct_by_yr, label = facility), color = "gray35",
            size = 2.5, hjust = 0, vjust = -1.85)+
  # label for IU Health North
  geom_text(data = prov %>% ungroup() %>% arrange(desc(year)) %>% 
              filter(facility == "Indiana University Health North Hospital" & 
              year == 2018) %>% distinct(facility, .keep_all = T),  
            aes(year + .1, pct_by_yr, label = facility), color = "gray35",
            size = 2.5, hjust = 1.2, vjust = -.8)+
  # add line and points and labels for IU Health University Hosp two locations
  # IUHU hosp 2014-15 line
  geom_line(data = prov %>% filter(facility == "Indiana University Health University Hospital" 
            & year %in% 2014:2015), 
            aes(year, pct_by_yr, group = facility, color = facility), size = .75)+
  # IUHU hosp 2020-21 line 
  geom_line(data = prov %>% filter(facility == "Indiana University Health University Hospital" 
                                   & year %in% 2020:2021), 
            aes(year, pct_by_yr, group = facility, color = facility), size = .75)+
  geom_text(data = prov %>% ungroup() %>% arrange(desc(year)) %>% 
              filter(facility == "Indiana University Health University Hospital") %>% 
              distinct(facility, .keep_all = T),  
            aes(year + .1, pct_by_yr, label = facility), color = "gray35",
            size = 2.5, hjust = 0, vjust = -.45)+
  geom_text(data = prov %>% ungroup() %>% arrange(desc(year)) %>% 
              filter(facility == "Indiana University Health University Hospital") %>% distinct(facility, .keep_all = T),  
            aes(2014, pct_by_yr, label = "IU Health University Hospital"), color = "gray35",
            size = 2.5, hjust = -.05, vjust = -2.5)+
 
  # points for hospitals with only one abortion in a year
  geom_point(data = prov %>% filter(count_by_yr == 1 & type == "Hospital") %>% 
               distinct(facility, .keep_all = T), 
             aes(year, pct_by_yr, group = facility), color = "#756bb1", size = 2)+
  # label for Community Hosp. North Surgery Cntr
  geom_text(data = prov %>% filter(facility == 
                                     "Community Hospital North Surgery Center") %>% 
              distinct(facility, .keep_all = T),  
            aes(year + .60, pct_by_yr, label = "Community Hospital\nNorth Surgery Center"),
            size = 2.5, hjust = -.5, vjust = -1.45, lineheight = 1, color = "gray35",)+
  # line to label for Community Hosp. North Surgery Cntr
  annotate("segment", x = 2018.0525, xend = 2018.95,
           y = .025, yend = .12, color = "gray75", linewidth = .15)+
  # label for Community Hosp. North Surgery Cntr
  geom_text(data = prov %>% filter(facility == 
                                     "Indiana University Health West Hospital") %>% 
              distinct(facility, .keep_all = T),  
            aes(year + .1, pct_by_yr, label = facility), color = "gray35",
            size = 2.5, hjust = 0, vjust = .2)+
  scale_x_continuous(limits = c(2014, 2023),
                     breaks = seq(from = 2014, to = 2023, by = 1),
                     labels = c("2014", "2015", "2016", "2017", "2018", "2019",
                                "2020", "2021", "", ""))+
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = c(0, .5, 1),
                     name = "")+
  scale_color_manual(values = hosp_list)+
  t_theme()+
  theme(
    panel.background = element_rect(fill = panel_c),
    plot.background = element_rect(fill = panel_c),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    legend.position = "none")+
  labs(title = 
         paste0("Abortions in hospitals comprise just ", 
                round(100*prov %>% ungroup() %>% filter(str_detect(facility, "Hospital") == TRUE) %>% 
                        summarise(sum(count_by_yr))/prov %>% ungroup() %>%  summarise(sum(count_by_yr)),1),
                "% of all abortions. <span style='color:#fdae61;'>Sidney & Lois Eskenazi Hospital</span>
             was one of ",
                prov %>% ungroup() %>% filter(type == "Hospital") %>% distinct(facility) %>% 
                  select(facility) %>% nrow(),"
             that performed them from 2014 to 2021. It accounted for <span style='color:#fdae61;'>",
                round(prov %>% ungroup() %>% filter(facility == "Sidney & Lois Eskenazi Hospital") %>% 
                        summarise(sum(count_by_yr))/prov %>% ungroup() %>% filter(str_detect(facility, "Hospital") == TRUE) %>% 
                        summarise(sum(count_by_yr))*100),
                        "%</span> of hospital abortions but just <span style='color:#fdae61;'>",
                round(prov %>% ungroup() %>% filter(facility == "Sidney & Lois Eskenazi Hospital") %>% 
                        summarise(sum(count_by_yr))/tot_ab*100,2),
            "%</span> of all and was the only facility that conducted 
            them each year."),
       
       subtitle = paste0("There were <span style = 'color:#756bb1;'>",
                         prov %>% ungroup() %>% filter(count_by_yr <= 1 & type == "Hospital") %>% distinct(facility) %>% 
                           select(facility) %>% nrow(),"</span> 
                         instances when hospitals performed a <span style = 'color:#756bb1;'>single</span> abortion in a year."),
       caption = caption)

