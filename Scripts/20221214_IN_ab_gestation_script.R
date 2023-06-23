# load libraries ####
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(ggtext)

# import data scraped from Terminated Pregnancy Reports from 2014-2018 and consolidate into one df ####
# data is limited to those years due to changes in categorizations of abortion procedures.

# list of files where gestation and procedure were reported
files <- list.files("External Data/reports",full.names = T, 
                    pattern = "(2014)|(2015)|(2016)|(2017)")

for(i in 1:n_distinct(files)){
  gest <- read_xlsx(files[i],
                    sheet = 9) %>%  
    # extract year from filename from files object
    mutate(year = str_extract(files[i], pattern = "\\d{4}"),
           .before = age)
  # shorten column names 
  colnames(gest) <- c("year", "age", "de", "rx", "sc", "ma") 
  
  # standardize age column ranges
  gest$age <- c("8", "13", "15", "17", "20")
  
  # save and bind all years into one dataframe
  
  if (i==1){
    gest2 <- gest}
  else {
    gest2 <- rbind(gest2, gest)
  }
  rm(gest)
  gest <- gest2
} 

# clean up objects, lists ####
rm(gest2, files, i)

# write all years to csv file
write_csv(gest, file = "Exported_Data/raw_gest.csv")

# calculate total count ####
# dividing by four to accomodate upcoming mean calculations for all years.
tot <- gest[3:6] %>% sum(na.rm = T)/4


# calculate mean of all types
gest <- 
  gest %>% group_by(age) %>% 
  summarise(de = round(mean(de),0),
            rx = round(mean(rx),0),
            sc = round(mean(sc),0),
            ma = round(mean(ma, na.rm=T),0))

# pivot longer, create percentages column ####
gest <- 
  gest %>% 
  pivot_longer(3:5,
               names_to = "type", values_to="count") %>% 
  group_by(type, age) %>% 
  summarise(count=sum(count,na.rm = T)) %>% 
  mutate(pct_all = round(100*(count/tot),2)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(pct_age_type = round(100*(count/sum(count)),2),) %>% 
  select(age, type, count, pct_age_type, pct_all) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(pct_age_all =round(100*sum(count,na.rm = T)/tot,2)) 

# convert age to factor to preserve ordering ####
gest <- gest[order(as.numeric(as.character(gest$age))),]

# write gest to csv
write_csv(gest, file = "Exported_Data/gest.csv")

# add blanks for empty weeks after 20 for plot x axis 

gest[16:33,3:6] <- (0)

# assign levels to object ed_lev ####
ed_lev <- c(
  "8",
  "13",
  "15",
  "17",
  "20",
  "23", 
  "26", 
  "29", 
  "32", 
  "35",
  "39")
#
ed_lab <- c(
  "0 to 8 \nweeks",
  "9 to 13",
  "14 to 15",
  "16 to 17",
  "18 to 20",
  "21 to 23",
  "24 to 26",
  "27 to 29",
  "30 to 32",
  "33 to 35",
  "Full term")

# calculation objects for labels, titles ####
# total all abortions that are medical 
tot_rx <- gest %>% 
  filter(type == "rx") %>%
  ungroup %>% 
  summarise(n=sum(count))%>% 
  pull(n)

# total all abortions that are suction curettage 
tot_sc <- gest %>% filter(type == "sc") %>%
  ungroup %>% 
  summarise(n=sum(count))%>% 
  pull(n)

# total all abortions that are dilation and evacuation  
tot_de <- gest %>% filter(type == "de") %>%
  ungroup %>% 
  summarise(n=sum(count))%>% 
  pull(n)

# total all abortions that are menstrual aspiration 
tot_ma <- gest %>% filter(type == "ma") %>%
  ungroup %>% 
  summarise(n=sum(count))%>% 
  pull(n)

# total of all abortions that are surgical
tot_surg <- 
  sum(tot_ma+tot_de+tot_sc)
# pct all abortions that are done via sc
tot_pct_sc <-  
  gest %>% filter(type == "sc") %>%
  ungroup() %>% 
  summarise(pct = round(100*(sum(count)/tot),2)) %>% 
  pull(pct)

# pct all abortions that are done via de
tot_pct_de <- 
  gest %>% filter(type == "de") %>%
  ungroup() %>% 
  summarise(pct = round(100*(sum(count)/tot),2)) %>% 
  pull(pct)

# pct all abortions that are done via rx
tot_pct_rx <- 
  gest %>% filter(type == "rx") %>%
  ungroup() %>% summarise(pct = round(100*(sum(count)/tot),2)) %>% 
  pull(pct)

# pct of all surgical abortions that are suction curettage
pct_sc_sg <- 
  round(100*(tot_sc/tot_surg),2)

# pct of all surgery abortions that are dilation and evacuation
pct_de_sg <- 
  gest %>% 
  filter(type == "de") %>%
  ungroup() %>% summarise(pct = round(100*(tot_de/sum(tot_sc,tot_ma)),2)) %>% 
  pull(pct)

# percent of all abortions that are surgical 
pct_surg <- 
  round(100*(tot_surg/tot),0)


# total percent of all abortions that happen after 13 weeks
tot_pct_14_39_wk <- 
  gest %>% 
  filter(age != "8") %>% 
  filter(age != "13") %>% 
  ungroup() %>% summarise(pct = round(100*(sum(count)/tot),2)) %>% 
  pull(pct)

# pct of abortions that happen in first 8 weeks
wk_8_tot <- 
  gest %>% 
  filter(age=="8") %>% 
  summarise(n=sum(count), 
            pct = round(100*(n/tot),2)) %>% 
  pull(pct)

# total percent of all abortions that happen in weeks 9-13
wk_9_13_tot <- 
  gest %>% 
  filter(age=="13") %>% 
  summarise(n=sum(count), 
            pct = round(100*(n/tot),2)) %>% 
  pull(pct)
# total for 0 to 13 weeks
wk_0_13_tot <- wk_9_13_tot+wk_8_tot

# color ####
panel_c <- "#fdfdf2"
sc_c <- "#029ba2" 
rx_c <- "#9db6c9"
rx_c_txt <- "#9db6c9" 
de_c <- "#029ba2"
de_c_txt <- "#029ba2" 
body_text <- "#000000"

type_colors <- c("sc" = sc_c,
                 "rx" = rx_c,
                 "de" = sc_c)


# titles and labels ####
# title
title <-  paste(
  "<span style = 'color:",body_text,";font-size:17pt'>Nearly all abortions in Indiana
  happen by 13 weeks of gestational age.</span>")
#
subtitle <- paste(
  "<br><span style  = 'color:",body_text,";'>In 2018 Indiana reduced the classification 
  of procedures in its annual terminated pregancy reports to</span>
  <span style = 'color:",rx_c_txt,";'>medical</span> or
  <span style = 'color:",sc_c,";'>surgical</span>. 
  <span style = 'color:",rx_c_txt,";'>Medical abortion</span> uses medicine to end 
  a pregnancy.</span> 
  <span style = 'color:",sc_c,";'>Surgical abortion</span> encompasses many procedures, 
  sometimes called different names, used at different gestational ages and with 
  varying degrees of complexity. Indiana classifies menstrual aspiration, 
  suction curettage, dilation and evacuation, and dilation and extraction as surgical.<br></span>")
#
med_lab <- paste(
  "<span style = 'color:",rx_c,";'>Medical abortions</span>
  <span style = 'color:",body_text,";'>use medicine to end pregnancies during the 
  first 10 weeks of gestational age.</span>")
#

asp_lab <- paste(
  "<span style = 'color:",sc_c,";'>Surgical: Aspiration abortions</span>
  <span style = 'color:",body_text,";'>involve opening the cervix and using a
  suction device to empty the uterus, generally in the first trimester and early 
  into the second.</span>")

sc_lab <- paste(
  "<span style = 'color:",sc_c,";'>Surgical: Suction curretage abortions</span>
  <span style = 'color:",body_text,";'>involve opening the cervix and using a
  suction device to empty the uterus. A curette may be used to scrape the walls 
  of the uterus, generally in the first trimester and early 
  into the second.</span>")

# dc lab 
dc_lab <- paste(
  "<span style = 'color:",sc_c,";'>Surgical: Dilation and curettage abortions</span>
  involve opening the cervix and using a curette to scrape and empty the uterus, 
  generally in the first trimester and early into the second.</span>")
#
de_lab <- paste(
  "<span style = 'color:",sc_c,";'>Surgical: Dilation and evacuation (D&E) abortions</span>
   <span style = 'color:",body_text,";'>involve opening the cervix 
   and using a combination of methods, including medication and suction, and 
   tools like forceps and a curette, to end a pregnancy and empty the uterus,
   generally in the second trimester.</span>")

dx_lab <- paste(
  "<span style = 'color:",sc_c,";'>Surgical: Dilation and extraction (D&X) abortions</span>
  <span style = 'color:",body_text,";'>involve opening the cervix and removing the
  fetus through the birth canal, generally late in the second trimester and in 
  the third.</span>")

#
post_22_wk_lab <- paste(
  "<span style = 'color:",body_text,";'>After 22 weeks (gestational age), abortion is legal only to 
  prevent serious health risk to, or save the life of, the mother; or if the fetus has 
  been diagnosed with a lethal fetal anomaly.</span>")

# #
rx_lab <- paste(
  "<span style = 'color:",rx_c_txt,";'>Medical abortions</span>
  <span style  = 'color:",body_text,";'>are
  <span style  = 'color:",rx_c_txt,";'>",tot_pct_rx,"%</span> of
  all abortions and prohibited after 10 weeks.</span>")
# 

# plot ####
p <- ggplot(gp)+
  theme_classic()+
  geom_col(data=gp,
           aes(x=as_factor(age), y=pct_all,
               fill=type, 
           ),
           color=NA, show.legend = F)+
  scale_x_discrete(name   = "",
                   breaks = NULL,
                   labels = NULL)+
  scale_y_continuous(name = "",
                     breaks = c(-30,0,if_else(gp$pct_age_all>1,gp$pct_age_all,0), 100),
                     labels = c("",paste(0,""),paste(if_else(gp$pct_age_all>1,gp$pct_age_all,0),"%"),"100%"))+
  scale_fill_manual(values = type_colors)+
  t_theme()+
  # theme adjustments ####
theme(
  panel.grid.major.y = element_blank(),
  #panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.line.y = element_blank(),
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = panel_c),
  plot.background = element_rect(color = panel_c, fill  = panel_c, size = .35),
  axis.text.x = element_text(size  = 7,
                             color = "gray55",
                             hjust = 0),
  axis.text.y = element_text(size  = 7,
                             color = "gray55"),
  # strip.text.x = element_text(size = 8,
  #                             color = "dark gray"),
  axis.title.y = element_markdown(angle = 0,
                                  size  = 8,
                                  color = body_text,
                                  vjust = .85,
                                  hjust = 1),
  axis.title.x = element_markdown(angle = 0,
                                  size  = 9,
                                  color = body_text,
                                  vjust = .5),
  # plot.title = (plot.title = element_textbox_simple(
  #   lineheight = 1, padding = margin(0, 0, 1, 0),
  #   family = "serif", size = 19
  # )),
  # plot.subtitle = element_textbox_simple(
  #   size = 11, lineheight = 1, family = "sans", padding = margin(0, 0, 5, 0)),
  plot.title.position = "plot",
  plot.margin = unit(c(.5, .5, .5, .5), "cm"),
  # plot.caption = element_textbox_simple(family = "sans", size = 7,
  #                                         color = "gray40", halign = 1,
  #                                         lineheight = 1.2),
  legend.position = "none"
)+
  labs(
    title = title,
    subtitle  = subtitle,
    caption = "**Abortion Data:** 'www.in.gov/health/vital-records/vital-statistics/terminated-pregnancy-reports/'<br>
    **Graphic:** Ted Schurter 2023"
  ) +
  # annotations and markers above 0 on x axis ####
# rx abortion label
annotate("text_box",x=1.75, y=67,
         label = rx_lab,
         size = 3.2, hjust = 0, color="black", box.color=panel_c,
         fill = NA, label.colour = panel_c, lineheight = 1)+
  # total pct label
  annotate("text_box",x=2.5, y=38,
           label = paste(wk_0_13_tot,"% of abortions happen within the first 13 weeks."),
           size = 4.5, hjust = 0, color="black", box.color=panel_c,
           fill = NA, label.colour = panel_c, lineheight = 1)+
  # segment for total pct label
  geom_segment(x=2.5, y = 44, xend = 1.85, yend = 44, color = sc_c, size = .15,
               arrow = arrow(length = unit(0.1, "cm")))+
  geom_curve(x=2.5, y = 44, xend = 2, yend = 30, color = sc_c, size = .15,
             curvature = list(0.45), arrow = arrow(length = unit(0.1, "cm")))+
  
  # not legal after 22 weeks
  annotate("text_box",x=6, y=45.5,
           label = post_22_wk_lab, size = 3.5, hjust = 0, color="black", box.color=panel_c,
           fill = NA, label.colour = panel_c, lineheight = 1, width = unit(3, "inch"))+
  geom_segment(x=5.85, y = 80, xend = 5.85, yend = 0, color = "light gray", size = .05,
               linetype = "dashed")+
  # annotations and markers below 0 on x axis ####
# gestational age labels ####
# 8 wks
annotate("text_box",x=.5, y=-8,label = "0 to<br> 8 weeks",size = 2.5, hjust = 0, color=body_text, 
         box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 13 wks
  annotate("text_box",x=1.5, y=-8,label = "9 to 13",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 15 wks
  annotate("text_box",x=2.5, y=-8,label = "14 to 15",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 17 wks
  annotate("text_box",x=3.5, y=-8,label = "16 to 17",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 20 wks
  annotate("text_box",x=4.5, y=-8,label = "18 to 20",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 23 wks
  annotate("text_box",x=5.5, y=-8,label = "21 to 23",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 23 wks
  annotate("text_box",x=6.5, y=-8,label = "24 to 26",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 29 wks
  annotate("text_box",x=7.5, y=-8,label = "27 to 29",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 32 wks
  annotate("text_box",x=8.5, y=-8,label = "30 to 32",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 35 wks
  annotate("text_box",x=9.5, y=-8,label = "33 to 35",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # 39 wks
  annotate("text_box",x=10.5, y=-8,label = "36 to 39",size = 3, hjust = 0, color= body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # procedure labels, trimester labels  ####
# horizontal line below gestational age labels and to mark trimesters
# first trimester
geom_segment(x=.55, xend = 2.20, y = -14.5, yend = -14.5, color="light gray", size = 1.4)+
  # "1st trimester"
  annotate("text_box",x=.55, y=-18.75,label = "1st trimester (not to scale)",size = 2.2, hjust = .045, 
           color=body_text, box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # second trimester
  geom_segment(x=2.20, xend = 7.6, y = -14.5, yend = -14.5, color="dark gray", size = 1.4)+
  annotate("text_box",x=2.2, y=-18.75,label = "2nd trimester",size = 2.2, hjust = .035, color=body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # third trimester
  geom_segment(x=7.6, xend = 11.2, y = -14.5, yend = -14.5, color="black", size = 1.4)+
  annotate("text_box",x=7.6, y=-18.75,label = "3rd trimester",size = 2.2, hjust = .035, color=body_text, 
           box.color=NA, fill = NA, label.colour = panel_c, lineheight = 1)+
  # bottom marker line
  # geom_hline(x=0, xend = 11, yintercept = -50, color=panel_c, size = .1)+
  # segment for medical abortion label
  # geom_segment(x=1.75, y = -26, xend = 1.75, yend = -17, color = sc_c, size = .15,
  #              arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text_box",
           
           #x=.45,y = -21.5
           x=0.45, y=-21.5,
           label = med_lab,
           size = 3, hjust = 0, color="black", box.color=panel_c,vjust = 1,
           fill = NA, label.colour = panel_c, lineheight = 1,width = unit(1.15, "inch"))+
  #unit(1.25, "inch"))+
  # segment for surgical aspiration abortion label
  # geom_segment(x=3.65, y = -26, xend = 3.65, yend = -17, color = sc_c, size = .15,
  #              arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text_box",x=1.79, y=-21.5,
           label = asp_lab,
           size = 3, hjust = 0, color="black", box.color=panel_c,vjust = 1,
           fill = NA, label.colour = panel_c, lineheight = 1,width = unit(1.5, "inch"))+
  #unit(1.65, "inch"))+
  # sc label 
  annotate("text_box",
           x = 3.54, y = -21.5,
           #x=3.75, y=-21.5,
           label = sc_lab,
           size = 3, hjust = 0, color="black", box.color=panel_c,vjust = 1,
           fill = NA, label.colour = panel_c, lineheight = 1,width = unit(1.45, "inch"))+
  
  # dc label
  annotate("text_box",
           x=5.31, y=-21.5,
           #x=5.55, y=-21.5,
           label = dc_lab,
           size = 3, hjust = 0, color="black", box.color=panel_c,vjust = 1,
           fill = NA, label.colour = panel_c, lineheight = 1,width = unit(1.3, "inch"))+
  annotate("text_box",
           x=6.91, y=-21.5,
           #x=7.15, y=-21.5,
           label = de_lab,
           size = 3, hjust = 0, color="black", box.color=panel_c, vjust = 1,
           fill = NA, label.colour = panel_c, lineheight = 1, width = unit(1.7, "inch"))+
  annotate("text_box",
           x=8.81, y=-21.5,
           #x=9.2, y=-21.5,
           label = dx_lab,
           size = 3, hjust = 0, color="black", box.color=panel_c, vjust = 1,
           fill = NA, label.colour = panel_c, lineheight = 1, width = unit(1.7, "inch"))+
  # test label
  # annotate("text_box",
  #         x= -.4, y=-25.5,
  #         #x=9.2, y=-21.5,
  #         label = "Types<br>of<br>abortions",
  #         size = 3, hjust = 0, color="black", box.color=panel_c, vjust = 1,
  #         fill = NA, label.colour = panel_c, lineheight = 1, width = unit(1.7, "inch"))+
  
  # hidden line used to ensure appropriate depth for labels
  geom_hline(x=0, xend=11.4, yintercept = -85, color = panel_c, size =1)

# # save ####
ggsave(filename = paste("Plots/svg/",Sys.Date(),"gestation",panel_c,sc_c,rx_c,de_c,".svg"),
       plot = p,
       width = 3300, height =1800, units = "px")


