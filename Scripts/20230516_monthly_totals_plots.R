library(tidyverse)
library(ggrepel)
library(ggtext)

# import cleaned data 

mnth <- read_csv("Exported_Data/month_ab_IN_res_clean.csv")

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


# set colors 

# panel color
panel_c <- "#fdfdf2"

# monthly color 
mc <- "#1a9641"
avgc <- "#7b3294"
# high average month 
h_avgc <- "#006837"  
# low average month
l_avgc <- "#78c679"
# avg month
avg_mc <- "#af8dc3"

mnth$month <- factor(mnth$month, levels = c(
  "January", "February", "March", "April", "May", "June", "July","August",
  "September", "October", "November", "December"))
#
# create new dataframe with column to retain month grouping when faceting
mnth2 <- mnth
mnth2$m2 <- mnth2$month
# remove original month column so new dataframe isn't faceted by it when month is called
mnth2$month <- NULL

mnth_tot_lab <- mnth %>% group_by(month) %>% 
  summarise(m_avg = round(mean(res)))



#

ggplot()+ 
  geom_line(data = mnth2,
            aes(year, res, group = m2), color = "gray92", size = .2)+
  geom_line(data = mnth,
            aes(year, res, group = month),color = mc, show.legend = F)+
  geom_line(data = mnth %>% group_by(year) %>% mutate(avg = mean(res)),
            aes(year, avg, group = month),
            color = avgc)+
  # average monthly total
  geom_richtext(data = mnth_tot_lab,
                aes(x= 2018.7, y = 790, 
                    label = paste0("Average total:<span style = 'color:",mc,"'> ", 
                                   prettyNum(m_avg, big.mark = ",", scientific = FALSE),"</span>")),
                label.colour = NA, fill = panel_c,
                size = rel(2.25), color = "gray50", hjust = 0)+
# # annual mean
  geom_segment(data = mnth %>%
               summarise(res = mean(res)),
             aes(x = 2014, xend = 2021,
                 y = res, yend = res),
             color = "gray30", linewidth = .15,
             show.legend = F)+
  scale_x_continuous(limits = c(2014,2021),
                   breaks = c(2014, 2016, 2018, 2020),
                   labels = c("2014", "2016", "2018",
                              "2020" ))+
  scale_y_continuous(limits = c(400,820),
                     breaks = c(400, 600, 800),
                     labels = c("400", "600", "800"))+
  #annual average label
  # geom_text(data = mnth %>% filter( month == "January"),
  #           aes(x=2016.05, y = 420, label = "Annual average"),
  #           size = rel(2), color = avgc)+
  geom_text(data = mnth %>% filter( month == "January"),
            aes(x= 2014.05, y = 540, label = "Annual\nmonthly\naverage"),
            size = rel(2), color = "gray50", hjust = 0, lineheight = .8, 
            vjust = 1)+
  # annual average label line
  geom_segment(data = mnth %>% filter( month == "January"),
            aes(x=2014, xend = 2014,
                y = 550, yend = 590),
            size = .10, color = avgc)+
  t_theme()+
  facet_wrap(~ month, scales = "free")+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "gray35", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = panel_c,
                                    color = "gray93"),
    axis.text = element_text(color = "gray55",
                             size  = rel(.4)),
  )+
  labs(
  title = paste0("The variation in monthly <span style = 'color:",mc,"'>totals</span> 
                 reveals how much they vary month to month. Comparing the <span style =
                 'color:",mc,"'>totals</span> to the <span style = 'color:",avgc,"'>
                 monthly average</span> from 2014 to 2021 reveals how much a monthly total can vary in any year. "),
       
  subtitle = paste0("Between 2014 and 2021, there were an<span style = 'color:",avgc,"'>
                    average</span> of <span style = 'color:",avgc,"'>",
  round(mean(mnth$res)), 
  " </span>abortions per month."),
  caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
  **Graphic:** Ted Schurter 2023")

#ggsave(filename = "Plots/svg/ab_month.svg",width = 11, height = 6, units = "in")

#
# make facet label reflect average annual % above or below average

# to do so, need to add df with label, color, etc

yr_tot_lab <- mnth %>% group_by(year) %>% 
  mutate(annual = sum(res)) %>% 
  ungroup() %>% 
  mutate(mn_an = mean(annual),
         lab = paste0(if_else(annual>mean(annual), "Above annual average", "Below annual average"))) %>% 
  ungroup() %>% 
  mutate(
    pct = round((annual-mean(annual))/annual*100),
    col = if_else(pct>0, "black", "red"))

# create duplicate year column for faceting
mnth2$y2 <- mnth$year
mnth2$month <- NULL
mnth2$year <- NULL

mnth %>% group_by(month) %>% 
  mutate(av = round(mean(res))) %>% 
  ggplot()+ theme_classic()+
  # lines for all months
  geom_line(data = mnth2,
            aes(m2, res, group = y2), color = "gray92", size = .2)+
  # line for monthly average
  geom_line(data = mnth %>% group_by(month) %>%
              summarise(res = mean(res)) %>%
              ungroup(),
            aes(month, res, group = 1),  color = avgc, show.legend = F)+
  # line for annual average
  geom_segment(data = mnth %>%
                 summarise(res = mean(res)) %>%
                 ungroup(),
               aes(x = 1, xend = 12,
                   y = res, yend = res), size = .1, color = "gray30", 
               #linetype = "dotted", 
               show.legend = F)+
  # line for monthly totals
  geom_line(
    aes(month, res, group = year), color = mc, show.legend = F)+
  # label for monthly average
  geom_text(data = mnth %>% filter( year == "2014"),
            aes(x= 1.3, y = 560, label = "Overall\naverage"),
            size = rel(2), color = "gray50", hjust = 0, lineheight = .8, 
            vjust = 1)+
  # line for label for monthly average
  geom_segment(data = mnth %>% filter( year == "2014"),
               aes(x=1.1, xend = 1.1,
                   y = 550, yend = 600),
               size = .05, color = avgc)+
  # total abortions for year
  geom_richtext(data = yr_tot_lab,
                aes(x= 8.2, y = 770, 
                    label = paste0(pct,"% ",lab," ")),
                label.colour = NA, fill = panel_c,
                size = rel(2.15), color = yr_tot_lab$col, hjust = 0)+
  # invisible mark to preserve equal y scale for each facet
  geom_point(aes(x=1, y = 500), color = panel_c)+
  scale_y_continuous(limits = c(400,800),
                     breaks = c(400, 600, 800),
                     labels = c("400", "600", "800"))+
  scale_x_discrete(
    breaks = c("January", "February", "March", "April","May", "June",
               "July", "August", "September", "October", "November","December"),
    labels = c("Jan","Feb","Mar", "April", "May", "June", "July",
               "Aug", "Sept", "Oct", "Nov", "Dec"))+
  t_theme()+
  facet_wrap(~ year, scales = "free")+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "gray35", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = panel_c,
                                    color = "gray95"),
    axis.text = element_text(color = "gray55",
                             size  = rel(.4)),
  )+
  labs(title = paste0("The individual <span style = 'color:",avgc,"'>monthly average</span> of Indiana resident abortions from 2014 to 2021 reveals annual trends 
  and how far a month’s<span style = 'color:",mc,"'> 
  totals</span> fluctuate from its average in a given year."),
  subtitle = paste0("The average number of monthly abortions is ", 
  round(mean(mnth$res)),
  "; the average annual total is ",
  prettyNum(round(mnth %>% group_by(year) %>% summarise(tot = sum(res)) %>% summarise(mean(tot))), big.mark = ",", scientific = FALSE),"."),
       caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
  **Graphic:** Ted Schurter 2023")
# by year ####

# add another year column to existing separate dataframe - mnth2 - for faceting
mnth2$y2 <- mnth$year
#mnth2$year <- NULL

yr_tot_lab <- mnth %>% group_by(year) %>% 
  summarise(annual = sum(res))

mnth %>% group_by(month, ) %>% 
  mutate(av = round(mean(res))) %>% 
  ggplot()+theme_classic()+
  # lines for all months
  geom_line(data = mnth2,
            aes(m2, res, group = y2), color = "gray92", size = .2)+
  # line for monthly average
  geom_line(data = mnth %>% group_by(month) %>%
              summarise(res = mean(res)) %>%
              ungroup(),
            aes(month, res, group = 1),  color = avgc, show.legend = F)+
  # line for annual average
  geom_segment(data = mnth %>%
              summarise(res = mean(res)) %>%
              ungroup(),
            aes(x = 1, xend = 12,
                y = res, yend = res), size = .1, color = "gray30", 
            #linetype = "dotted", 
            show.legend = F)+
  # line for monthly totals
  geom_line(
              aes(month, res, group = year), color = mc, show.legend = F)+
  # label for monthly average
  geom_text(data = mnth %>% filter( year == "2014"),
            aes(x= 1.3, y = 560, label = "Annual\nmonthly\naverage"),
            size = rel(2), color = "gray50", hjust = 0, lineheight = .8, 
            vjust = 1)+
  # line for label for monthly average
  geom_segment(data = mnth %>% filter( year == "2014"),
               aes(x=1.1, xend = 1.1,
                   y = 550, yend = 600),
               size = .05, color = avgc)+
  # total abortions for year
  geom_richtext(data = yr_tot_lab,
            aes(x= 9.3, y = 770, 
                label = paste0("Annual total:<span style = 'color:",mc,"'> ", 
                               prettyNum(annual, big.mark = ",", scientific = FALSE),"</span>")),
            label.colour = NA, fill = panel_c,
             size = rel(2.25), color = "gray50", hjust = 0)+
  # invisible mark to preserve equal y scale for each facet
  geom_point(aes(x=1, y = 500), color = panel_c)+
  scale_y_continuous(limits = c(400,800),
                     breaks = c(400, 600, 800),
                     labels = c("400", "600", "800"))+
  scale_x_discrete(
    breaks = c("January", "February", "March", "April","May", "June",
               "July", "August", "September", "October", "November","December"),
    labels = c("Jan","Feb","Mar", "April", "May", "June", "July",
               "Aug", "Sept", "Oct", "Nov", "Dec"))+
  t_theme()+
  facet_wrap(~ year, scales = "free")+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "gray35", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = panel_c,
                                    color = "gray95"),
    axis.text = element_text(color = "gray55",
                             size  = rel(.4)),
  )+
  labs(title = paste0("The <span style = 'color:",avgc,"'>monthly average</span> 
  of Indiana resident abortions from 2014 to 2021 reveals annual trends 
  and also provides context for how far a month’s<span style = 'color:",mc,"'> 
  totals</span> can depart from its average in a given year."),
  subtitle = paste0("The average number of monthly abortions is ", 
  round(mean(mnth$res)),
  "; the average annual total is ",
  prettyNum(round(mnth %>% group_by(year) %>% summarise(tot = sum(res)) %>% summarise(mean(tot))), big.mark = ",", scientific = FALSE),"."),
       caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
  **Graphic:** Ted Schurter 2023")


#ggsave(filename = "Plots/svg/ab_month_yr.svg",width = 11, height = 6, units = "in")

#

# non faceted plot showing percent above normal for months by year ####

# short month labels
s_m_lab <-  c("January" = "Jan.",
              "February" = "February",
              "March" = "Mar.", 
              "April" = "April", 
              "May" = "May",
              "June" = "June",
              "July" = "July",
              "August" = "Aug.",
              "September" = "September",
              "October" = "Oct.",
              "November" = "Nov.",
              "December" = "Dec.")

# month labels colors 

m_lab_c <- c("January"    = "gray70",
             "February"   = l_avgc,
             "March"     = "gray70",
             "April"     = "gray70",
             "May"       = "gray70",
             "June"      = "gray70",
             "July"      = "gray70",
             "August"    = "gray70",
             "September" = h_avgc,
             "October"   = "gray70",
             "November"  = "gray70",
             "December"  = "gray70")

# 
# what is the percent above or below the average monthly total for each month?
t <-  mnth %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(mn_avg = mean(res),
         dif_mn = res-mn_avg,
         pct_df_m = (dif_mn/mn_avg)*100) 

ggplot()+ theme_classic()+
  # line for monthly totals
  geom_segment(data = mnth,
               aes(x=2013.3, xend = 2021,
                   y = 0, yend = 0),
               size = .25, color = "gray70")+
  geom_line(data = mnth %>% 
              ungroup() %>% 
              group_by(month) %>% 
              mutate(mn_avg = mean(res),
                     dif_mn = res-mn_avg,
                     pct_df_m = (dif_mn/mn_avg)*100),
            aes(year, pct_df_m, group = month), 
            size = .3, color = "gray92", show.legend = F)+
  geom_line(data = mnth %>% 
              ungroup() %>% 
              group_by(month) %>% 
              mutate(mn_avg = mean(res),
                     dif_mn = res-mn_avg,
                     pct_df_m = (dif_mn/mn_avg)*100) %>% filter(month == "September"),
            aes(year, pct_df_m, group = month), 
            size = .75, color = h_avgc, show.legend = F)+
  geom_line(data = mnth %>% 
              ungroup() %>% 
              group_by(month) %>% 
              mutate(mn_avg = mean(res),
                     dif_mn = res-mn_avg,
                     pct_df_m = (dif_mn/mn_avg)*100) %>% filter(month == "February"),
            aes(year, pct_df_m, group = month), 
            size = .75, color = l_avgc, show.legend = F)+
  
  geom_text_repel(data = mnth %>% 
                    ungroup() %>% 
                    group_by(month) %>% 
                    mutate(mn_avg = mean(res),
                           dif_mn = res-mn_avg,
                           pct_df_m = (dif_mn/mn_avg)*100) %>% filter(year == 2021),
    #data= t %>% filter(year == 2021),
                  aes(year, pct_df_m, label = s_m_lab),
                  xlim = c(2021.7), 
                  hjust = .25,
                  point.padding = 1.25,
                  nudge_y = 0, 
                  nudge_x = .7,
                  force = 2,
                  color = m_lab_c, 
                  segment.color = "gray75",
                  size = rel(2.35), 
                  fontface = "plain", 
                  direction = "y",
                  segment.size = 0.1,
                  min.segment.length = 1.5,
                  seed = 2, 
  )+
  geom_textbox(data = mnth %>% 
                 ungroup() %>% 
                 group_by(month) %>% 
                 mutate(mn_avg = mean(res),
                        dif_mn = res-mn_avg,
                        pct_df_m = (dif_mn/mn_avg)*100),
    aes(x= 2019.85, y = 15.15),
    label = paste0("<span style = 'color:",h_avgc,"'>September</span> 
                went from well **below** its monthly average in 2019 to well **above** it in 2020."),
    label.colour = NA, fill = panel_c,
    size = rel(3.25), color = "gray50", 
    hjust = 1, box.color = NA)+
  geom_textbox(data = mnth %>% 
                 ungroup() %>% 
                 group_by(month) %>% 
                 mutate(mn_avg = mean(res),
                        dif_mn = res-mn_avg,
                        pct_df_m = (dif_mn/mn_avg)*100),
    aes(x= 2017, y = -17.9),
    label = paste0("<span style = 'color:",h_avgc,"'>**September**</span> 
                was", 
                   round(abs(t$pct_df_m[t$year == 2019 & t$month == "September"])),
                   "% below its average in 2019."),
    label.colour = NA, fill = panel_c,
    size = rel(2.5), color = "gray50", 
    hjust = 0, box.color = NA)+
  geom_segment(aes(
    x = 2019.67, xend = 2019.95,
    y = 13, yend = 11.95), 
    color = "gray10", linewidth = .15)+
  geom_textbox(
    aes(x= 2013.3, y = 3.65),
    label = "Above 0 = more than the monthly average.", 
    label.colour = NA, fill = panel_c,
    size = rel(2), color = "gray50", 
    hjust = 0, box.color = NA,   width = unit(.6, "inch"))+
  geom_textbox(
    aes(x= 2013.3, y = -3.65),
    label = "Below 0 = less than the monthly average.", 
    label.colour = NA, fill = panel_c,
    size = rel(2), color = "gray50", 
    hjust = 0, box.color = NA,   width = unit(.6, "inch"))+
  
  
  scale_y_continuous(limits = c(-20,20),
                     breaks = c(-20, -10, 0, 10, 20),
                     labels = c("-20%", "-10%", "0", "10%", "20%"))+
  scale_x_continuous(limits = c(2013.3, 2022),
                     breaks = seq(2013, 2022, by = 1),
                     labels = c( "", "2014", "2015", "2016", "2017", "2018",
                                 "2019", "2020", "2021", ""))+
  t_theme()+
  #facet_wrap(~ month, scales = "free")+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "gray35", hjust = 0),
    panel.grid.major.y = element_blank(),
    # panel.background = element_rect(fill = panel_c,
    #                                 color = "gray95"),
    # axis.text.y = element_markdown(color = "gray55",
    #                          size  = rel(.6)),
  )+
  labs(title = paste0("The number of abortions to Indiana residents in a month fluctuates above 
  and below its eight-year average, sometimes dramatically. Between 2014 and 
  2021,<span style = 'color:",h_avgc,"'> ",
                      month.name[as.numeric(t %>% mutate(ab_pct_cg = pct_df_m-lag(pct_df_m)) %>% 
                                              arrange(desc(abs(ab_pct_cg))) %>% ungroup() %>% slice(1) %>% select(month))],"
  </span>had the highest year to year change, shifting <span style = 'color:",h_avgc,"'>",
                      round(t %>% mutate(ab_pct_cg = pct_df_m-lag(pct_df_m)) %>% arrange(desc(abs(ab_pct_cg))) %>% 
                              ungroup() %>% slice(1) %>% select(ab_pct_cg)),
                      "% </span>from 2019 to 2020. <span style = 'color:",l_avgc,"'> ",
                      month.name[as.numeric(mnth %>% group_by(month) %>% mutate(mn_df = 100*(res-lag(res))/lag(res)) %>% ungroup() %>% 
                                              group_by(month) %>% summarise(m = mean(mn_df, na.rm = T)) %>%  arrange(abs(m)) %>% select(month) %>% slice(1))],"
  </span>had the lowest average annual change."),
       
       subtitle = "Each line shows how much a single month's abortion totals differ
  from the month's multi-year average.",      
       caption = "<br>**Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports'<br>
  **Graphic:** Ted Schurter 2023")  



