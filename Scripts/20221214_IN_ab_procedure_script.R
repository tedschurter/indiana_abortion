# load libraries ####
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(ggtext)

# load data ####
# create list of files from 2014 to 2018 when procedure types were broken
# down into categories

files <- list.files("External Data/reports",full.names = T, 
                    pattern = "(2014)|(2015)|(2016)|(2017)|2018")

# proc df 2014-18 ####
# create loop to read the right sheet from those years and make dataframe
for(i in 1:n_distinct(files)){
  proc <- read_xlsx(files[i],
                    sheet = 5) %>%  
    # extract year from filename from files object
    mutate(year = str_extract(files[i], pattern = "\\d{4}"),
           .before = Procedure,
           Percent = round(Percent,2)) %>% 
    # remove percent column
    select(-Percent)
  # save and bind all years into one dataframe
  
  if (i==1){
    proc_2 <- proc}
  else {
    proc_2 <- rbind(proc_2, proc)
  }
  rm(proc)
  proc <- proc_2
} 
# rename proc to proc_14_18 and clean up environment ####
proc_14_18 <- proc
rm(proc, proc_2, files, i)

# proc_19_21 data frame 2019-21 ####
# create df with totals from 2019:2021

files <- list.files("External Data/reports",full.names = T, 
                    pattern = "(2019)|(2020)|(2021)")


for(i in 1:n_distinct(files)){
  proc_2 <- read_xlsx(files[i],
                      sheet = 5) %>%  
    # extract year from filename from files object
    mutate(year = str_extract(files[i], pattern = "\\d{4}"),
           .before = Procedure,
           Percent = round(Percent,2)) %>% 
    # remove percent column
    select(-Percent)
  # save and bind all years into one dataframe
  
  if (i==1){
    proc_3 <- proc_2}
  else {
    proc_3 <- rbind(proc_3, proc_2)
  }
  rm(proc_2)
  proc_2 <- proc_3
} 
# rename to proc_19_21 and clean up environment ####
proc_19_21 <- proc_2
rm(proc_2, proc_3, files, i)

# standardize procedure type

proc_19_21 <- proc_19_21 %>% 
  mutate(
    Procedure = 
      case_when(Procedure == "Non Surgical" ~ "Medical",
                Procedure == "Surgical" ~ "Surgical",
                Procedure == "Medical" ~ "Medical",
                Procedure == "Non surgical" ~ "Medical")) 

# proc_14_18_consol consolidate procedure categories 2014-2018 #### 
#for easier comparison in one chart with data from 2019:21 consolidates 
# medical, non-surgical together

proc_14_18_consol <- proc_14_18 %>% 
  mutate(
    Procedure =
      case_when(
        Procedure =="Mifepristone Misoprostol" ~ "Medical",
        Procedure == "Medical (Non-Surgical)" ~ "Medical",
        Procedure == "Non surgical" ~ "Medical",
        Procedure == "Medical" ~ "Medical",
        # departure from previous version that runs without problem in different
        # r session: insert: Procedure == "non surgical other" ~ "Medical", 
        # unresolved why identical script works differently in different sessions.
        Procedure == "non surgical other" ~ "Medical",
        Procedure == "Unknown" ~ "Unknown",
        Procedure == "surgical other" ~ "Surgical",
        Procedure == "Menstrual Aspiration" ~ "Surgical",
        Procedure == "Suction Curettage" ~ "Surgical",
        Procedure == "Dilation Evacuation" ~ "Surgical",
        Procedure == "Medical (Surgical)" ~ "Surgical",)
  ) %>% 
  pivot_wider(
    names_from=Procedure,
    values_from = Count) %>% 
  rowwise() %>% 
  mutate(Surgical = sum(c_across(cols=Surgical)),
         Medical = sum(c_across(cols=Medical))) %>%  
  select(-Unknown) %>% 
  pivot_longer(
    cols = 2:3,
    names_to = "Procedure",
    values_to = "Count"
  )
# compare totals to ensure we've not added or lost terminations

sum(proc_14_18_consol$Count) == sum(proc_14_18$Count)
# = TRUE so good to move forward.

# 
# proc_14_18_surg_consol df with surgical not suction curettage from 2014-2018 ####
# used to illustrate distinction between suction curettage abortions and all other 
# surgical abortions
proc_14_18_surg_consol <- proc_14_18 %>% 
  filter(Procedure != "Suction Curettage") %>% 
  mutate(
    Procedure =
      case_when(
        Procedure =="Mifepristone Misoprostol" ~ "Medical",
        Procedure == "Medical (Non-Surgical)" ~ "Medical",
        Procedure == "Non surgical" ~ "Medical",
        Procedure == "Medical" ~ "Medical",
        Procedure == "Unknown" ~ "Unknown",
        Procedure == "surgical other" ~ "Surgical",
        Procedure == "Menstrual Aspiration" ~ "Surgical",
        Procedure == "Dilation Evacuation" ~ "Surgical",
        Procedure == "Medical (Surgical)" ~ "Surgical")
  ) %>% 
  pivot_wider(
    names_from=Procedure,
    values_from = Count) %>% 
  rowwise() %>% 
  mutate(Surgical = sum(c_across(cols=Surgical)),
         Medical = sum(c_across(cols=Medical))) %>% 
  select(-Unknown, -"NA") %>% 
  pivot_longer(
    cols = 2:3,
    names_to = "Procedure",
    values_to = "Count"
  ) 


# create proc_tot df to show total number of abortions from 2014:2021
# joins proc_14_18_consol and proc_19_21 and sumarises total ####
proc_tot <- rbind(proc_14_18_consol, proc_19_21) %>% 
  group_by(year) %>% 
  summarise(total = sum(Count)) 

# add column for grouping on plot 
proc_tot$Procedure <- "both"

# create proc_tot_type df with totals for medical and surgical procedures for each year
proc_tot_type <- rbind(proc_14_18_consol, proc_19_21) 

# pull numbers for start and stopping points of connecting line segments #### 
# medical segment start
rx_seg_1 <-  proc_14_18 %>% filter(year == 2018) %>% 
  filter (Procedure == "Mifepristone Misoprostol") %>% pull()
# medical segment end
rx_seg_2 <-  proc_19_21 %>% filter(year == 2019) %>% 
  filter (Procedure == "Medical") %>% pull()
# surgical segment start 
sg_seg_1 <-  proc_14_18_consol %>% filter(year == 2018) %>% 
  filter (Procedure == "Surgical") %>% pull()
# surgical segment end
sg_seg_2 <-  proc_19_21 %>% filter(year == 2019) %>% 
  filter (Procedure == "Surgical") %>% pull()
# calculate percentage change year to year ####
# proc_tot df with pct change in all procedures ####
proc_tot <- proc_tot %>% 
  mutate(
    pct_cng = (total-lag(total))/lag(total)*100)

# surg_cng df with pct change in surgical procedures  #####
surg_cng <- rbind(proc_14_18_consol, proc_19_21) %>% 
  filter(Procedure=="Surgical") %>% 
  mutate(
    pct_cng = (Count-lag(Count))/lag(Count)*100
  )
# med_cng df with pct change in medical procedures ####
med_cng <- rbind(proc_14_18_consol, proc_19_21) %>% 
  filter(Procedure=="Medical") %>% 
  mutate(
    pct_cng = (Count-lag(Count))/lag(Count)*100
  )
# save data as csv ####
#totals by procedure type and year with annual percent change as csv files
write_csv(rbind(surg_cng, med_cng), 
          file = 'Exported_Data/procedure_totals_pct_chng.csv')

# 2014 to 2018 abortion totals by procedure type and year
write_csv(proc_14_18, 
          file = 'Exported_Data/procedure_14_18.csv')

# # 2014 to 2018 totals by year and procedure with surgical subtypes consolidated 
# write_csv(proc_14_18_consol, 
#           file = 'Exported_Data/procedure_14_18_consol.csv')
# 
# # 2019 to 2021 totals by year and procedure type 
# write_csv(proc_19_21, 
#           file = 'Exported_Data/procedure_19_21.csv')



# assign colors ####
panel_c <- "#fdfdf2" 
body_text <- "black"
procedure_colors <- list(
  "Medical" = "#9db6c9",
  "Medical (Surgical)" = "#9db6c9",
  "Mifepristone Misoprostol" = "#9db6c9",
  "Surgical" = "#029ba2",
  "Suction Curettage"  = "#029ba2",
  "Dilation Evacuation" = "#029ba2",
  "surgical other" = "#029ba2")

# labels and annotations ####
# surgery line label
surg_def_lab <- paste(
  "<span style = 'color:",procedure_colors$'Suction Curettage',";'>Surgical procedures</span>
  <span style = 'color:",body_text,";'>including</span>
  <span style = 'color:",procedure_colors$'Suction Curettage',";'>menstrual 
  aspiraiton, suction curetttage, dilation and evacuation, and 'surgical other.'</span>")

surg_def_lab <- paste(
  "<span style = 'color:",body_text,";'>All</span>
  <span style = 'color:",procedure_colors$'Suction Curettage',";'>surgical</span>
  <span style = 'color:",body_text,";'>procedures including</span>
  <span style = 'color:",procedure_colors$'Suction Curettage',";'>suction curettage</span>")

# medical line label
med_lab <- paste(
  "<span style = 'color:",procedure_colors$Medical,";'>Medical Procedures</span>")

# suction curettage label
sc_lab <- paste(
  "<span style = 'color:",procedure_colors$'Suction Curettage',";'>Suction curettage</span> 
  <span style = 'color:",body_text,";'>only</span>")

# all surgical label
surg_lab <- paste(
  "<span style = 'color:",procedure_colors$'Suction Curettage',";'>Surgical Procedures</span>")

# non sc surgical types label
non_sc_surg <- paste(
  "<span style = 'color:",body_text,";'>All procedures reported as</span>
  <span style = 'color:",procedure_colors$'Suction Curettage',";'>surgical</span>
  <span style = 'color:",body_text,";'>except</span>
  <span style = 'color:",procedure_colors$'Suction Curettage',";'>suction curetttage,</span>
  <span style = 'color:",body_text,";'>including</span>
  <span style = 'color:",procedure_colors$'Suction Curettage',";'>dilation and 
  evacuation, 'medical (surgical)', menstrual aspiration, and 'surgical other.'</span>")

# title, subtitle and caption ####

pct_title <- paste(
  "<span style = 'color:",procedure_colors$Medical,";'>Medical</span>
  abortion procedures grew an average</span>
  <span style = 'color:",procedure_colors$Medical,";'>",
  paste(round(sum(med_cng$pct_cng, na.rm=T)/length(proc_tot$year),2),'%'),"</span>
  annually from 2014 to 2021 and eclipsed</span>
  <span style = 'color:",procedure_colors$Surgical,";'>surgical</span>
  procedures, which declined an average</span>
  <span style = 'color:",procedure_colors$Surgical,";'>",
  paste(abs(round(sum(surg_cng$pct_cng, na.rm=T)/length(proc_tot$year),2)),"%"),
  "</span>
  annually, as the predominant abortion procedure in 2019. The rate for all 
  abortions increased an average</span>
  <span style = 'color:gray;'>",
  round(sum(proc_tot$pct_cng, na.rm=T)/length(proc_tot$year),2),"%</span>
  annually."
)

subtitle <- paste(
  "<br><span style = 'color:",procedure_colors$Surgical,";'>Suction curettage</span><span style = 'color:",body_text,";'> can be identified as the most common</span><span style = 'color:",procedure_colors$Surgical,";'> surgical</span><span style = 'color:",body_text,";'> procedure until 2018 when Indiana consolidated all distinct</span><span style = 'color:",procedure_colors$Surgical,";'> surgical</span> procedures into one category.<br></span>")


# plot ####
ggplot()+
  # 2014:2018 medical data
  geom_line(data=proc_14_18 %>% 
              filter(Procedure =="Mifepristone Misoprostol"),
            aes(year, Count, group=Procedure),
            size = 1.25, color=procedure_colors$Medical,show.legend=F)+
  # 2014:2018 suction curettage data
  geom_line(data=proc_14_18 %>% 
              filter(Procedure =="Suction Curettage"),
            aes(year, Count, group=Procedure),
            size = .5, linetype = "solid", show.legend=F,
            color=procedure_colors$Surgical)+
  # 2014:2018 surgical non suction-curettage 
  geom_line(data=proc_14_18_surg_consol %>% filter(Procedure=="Surgical"),
            aes(year, Count, group=Procedure),
            size = .5, linetype = "dashed", show.legend=F,
            color=procedure_colors$Surgical)+
  # medical line with data from 2019:2021
  geom_line(data=proc_19_21 %>% filter(Procedure == "Medical"), 
            aes(year, Count, group=Procedure),
            size = 1.25, show.legend=F, color=procedure_colors$Medical)+
  # surgical line with data from 2019:2021
  geom_line(data=proc_19_21 %>% filter(Procedure == "Surgical"), 
            aes(year, Count, group=Procedure),
            color=procedure_colors$Surgical,
            size = 1.25, show.legend=F, linetype = "solid")+
  # line segment to connect medical lines from 2018:2019
  geom_segment(aes(
    x = 5, xend = 6,
    y = rx_seg_1, yend = rx_seg_2),
    show.legend=F, size = 1.25, color = procedure_colors$Medical)+
  geom_segment(aes(
    x = 5, xend = 6,
    y = sg_seg_1, yend = sg_seg_2),
    show.legend=F, size =1.25, color = procedure_colors$Surgical, linetype="solid")+
  # line for total number of abortions over time
  geom_line(data=proc_tot, 
            aes(year, total, group = "year"),
            size = .35, show.legend=F, linetype = "solid",
            color= "gray")+
  # dashed line for average number of abortions
  # geom_line(data=proc_tot, 
  #           aes(year, mean(total), group = "year"),
  #           size = .35, show.legend=F, linetype = "dashed",
  #           color= "gray")+
  # 0 baseline on x axis
  geom_segment(aes(x=1, xend=8,
                   y = 0, yend=0),
               size=.1, color="light gray")+
  scale_x_discrete(expand = expansion(mult = c(0,.15)))+
  scale_y_continuous(labels = scales::comma)+
  # annotations ####
# total label
annotate("text_box", x=8, y = proc_tot$total[proc_tot$year == "2021"],
         label = "All Procedures", color = "gray", lineheight = 1,
         width = unit(1.5, "inch"), hjust = 0, vjust = .5, box.color=panel_c, 
         fill = panel_c, size = 4)+
  # surgical label  
  annotate("text_box", x=8, y = pull(proc_19_21[proc_19_21$Procedure =="Surgical" & 
                                                  proc_19_21$year=="2021" & proc_19_21$Count,3]),
           label = surg_lab, color = panel_c, lineheight = 1,
           width = unit(1.5, "inch"), hjust = 0, vjust = .5, box.color=panel_c, 
           fill = panel_c, size = 4)+
  # medical label  
  annotate("text_box", x=8, y = pull(proc_19_21[proc_19_21$Procedure =="Medical" & 
                                                  proc_19_21$year=="2021" & proc_19_21$Count,3]),
           label = med_lab, color = panel_c, lineheight = 1,
           width = unit(1.5, "inch"), hjust = 0, vjust = .5, box.color=panel_c, 
           fill = panel_c, size = 4)+
  # suction curettage label
  annotate("text_box", x=5, 
           y = pull(proc_14_18[proc_14_18$Procedure =="Suction Curettage"& 
                                 proc_14_18$year=="2018" &proc_14_18$Count,3]),
           label = sc_lab, color = panel_c, lineheight = 1,
           hjust =1, vjust = 1.25, box.color=panel_c, 
           fill = panel_c, size = 2.5, halign = 1)+
  # surgical definition post 2018 label
  annotate("text_box", x=5, 
           y = pull(proc_14_18[proc_14_18$Procedure =="Suction Curettage"& 
                                 proc_14_18$year=="2018" &proc_14_18$Count,3]),
           label = surg_def_lab, color = panel_c, lineheight = 1.2,
           hjust =0, vjust = -.25, box.color=panel_c, 
           fill = panel_c, size = 2.5, halign = 0,width = unit(1.5, "inch"))+
  # vertical line denoting end of data with surgical subtypes
  geom_segment(aes(x=5,xend=5, 
                   y = 700+pull(proc_14_18[proc_14_18$Procedure =="Suction Curettage"& 
                                             proc_14_18$year=="2018" & proc_14_18$Count,3]),
                   yend=-700+pull(proc_14_18[proc_14_18$Procedure =="Suction Curettage"& 
                                               proc_14_18$year=="2018" & proc_14_18$Count,3])),
               color="dark gray", linetype = "solid",
               size = .25, show.legend = F)+
  # # non sc pre 2018 label
  annotate("text_box", x=5, 
           y = pull(proc_14_18_surg_consol[proc_14_18_surg_consol$year == "2018" &
                                             proc_14_18_surg_consol$Procedure == "Surgical" & 
                                             proc_14_18_surg_consol$Count,3]),
           label = non_sc_surg, color = panel_c, lineheight = 1,
           width = unit(2, "inch"), hjust =1, vjust = -.05, box.color=panel_c, 
           fill = panel_c, size = 2.5, halign = 1)+
  #  theme adjustments ####  
theme_classic()+
  theme(
    #axis.line.y = element_line(color="light gray", size = .05),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7,
                             color = "dark gray"),
    panel.background = element_rect(fill=panel_c),
    plot.title = element_textbox_simple(
      size = 16, lineheight = 1, family = "serif", padding = margin(0, 0, 1, 0)),
    plot.subtitle = element_textbox_simple(
      size = 12, lineheight = 1, family = "sans", padding = margin(0, 0, 1, 0)),
    plot.background = element_rect(fill = panel_c),
    plot.title.position = "plot",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(hjust = 0),
    legend.position = "none"
  )+
  labs(
    title = pct_title,
    subtitle  = subtitle,
    caption = paste("panel_c",panel_c,"medical",procedure_colors$Medical,
                    "surgical",procedure_colors$Surgical))
#
# save ####
ggsave(filename = paste("Plots/svg/",Sys.Date(),"proc.svg"),
       plot = last_plot(),
       width = 3300, height =1800, units = "px")
#

# BREAK ####
# percent change plot ####
# plot ####
ggplot()+
  geom_line(data =surg_cng,
            aes(year, pct_cng, group=Procedure),
            color=procedure_colors$Surgical,
            size = 1.25)+
  geom_line(data =med_cng,
            aes(year, pct_cng, group=Procedure),
            color=procedure_colors$Medical,
            size = 1.25)+
  geom_line(data=proc_tot,
            aes(year, pct_cng, group=Procedure),
            color="gray",
            size = .45
  )+
  geom_segment(aes(x=1, xend=8,
                   y = 0, yend=0),
               size=.05, color="black")+
  scale_x_discrete(expand = expansion(mult = c(0,.15)))+
  scale_y_continuous(breaks = c(-20,0,20,40),
                     labels = c("-20%", "0", "20%", "40%"))+
  # annotations ####
# total label
annotate("text_box", x=8, y = proc_tot$pct_cng[proc_tot$year == "2021"],
         label = "Total", color = "gray", lineheight = 1,
         width = unit(1.5, "inch"), hjust = 0, vjust = .5, box.color=panel_c, 
         fill = panel_c, size = 3)+
  # surgical label  
  annotate("text_box", x=8, y = surg_cng$pct_cng[surg_cng$year=="2021"],
           label = surg_lab, color = panel_c, lineheight = 1,
           width = unit(1.5, "inch"), hjust = 0, vjust = .5, box.color=panel_c, 
           fill = panel_c, size = 3)+
  # medical label  
  annotate("text_box", x=8, y = med_cng$pct_cng[med_cng$year=="2021"],
           label = med_lab, color = panel_c, lineheight = 1,
           width = unit(1.5, "inch"), hjust = 0, vjust = .5, box.color=panel_c, 
           fill = panel_c, size = 3)+
  theme_classic()+
  theme(
    #axis.line.y = element_line(color="light gray", size = .05),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7,
                             color = "dark gray"),
    panel.background = element_rect(fill=panel_c),
    plot.title = element_textbox_simple(
      size = 16, lineheight = 1, family = "sans", padding = margin(0, 0, 1, 0)),
    plot.subtitle = element_textbox_simple(
      size = 12, lineheight = 1, family = "sans", padding = margin(0, 0, 1, 0)),
    plot.background = element_rect(fill = panel_c),
    plot.title.position = "plot",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(hjust = 0),
    legend.position = "none"
  )
