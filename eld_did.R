library(data.table)
library(tidyr)
library(dplyr)
library(collapse)
library(fixest)
library(fastDummies)
library(ggplot2)
library(hexbin)
library(vtable) #summary tables
library(MASS) #ordinal logistic regression package
#one day, try kableExtra package for reg compilation

#setup
rm(list = ls())
set.seed = 666

### Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### load data
main1 = fread("rtruck_analysis.csv")

### create vars
main1$rate_log = main1$rate %>% log()

###filtering
test2 <- main1
test3 <- test2[test2$Year > 2014]
test3 <- test3[test3$count>299]
test3 <- test3[test3$Distance>200]

### Summary Table: Controls
sumtable(test3, out='csv', file='output/table1-1.csv', fixed.digits=TRUE, digits=3,
         vars=c('IDX_AIR_RTMFM',
                'IDX_RAIL_FRT_CARLOADS',
                'IDX_RAIL_FRT_INTERMODAL',
                'IDX_WATERBORNE_D11',
                'IDX_TRUCK_D11',
                'diesel_ppi',
                'cpi_food',
                'unemploy_rate',
                'trans_emply'),
         summ=c('mean(x)',
                'median(x)',
                'sd(x)',
                'min(x)',
                'max(x)'))

sum1 <- test3[test3$post1_pre==0]
sum1 %>% sumtable( out='csv', file='output/table1-2.csv', fixed.digits=TRUE, digits=3,
         vars=c('IDX_AIR_RTMFM',
                'IDX_RAIL_FRT_CARLOADS',
                'IDX_RAIL_FRT_INTERMODAL',
                'IDX_WATERBORNE_D11',
                'IDX_TRUCK_D11',
                'diesel_ppi',
                'cpi_food',
                'unemploy_rate',
                'trans_emply'),
         summ=c('mean(x)'))

sum2 <- test3[test3$post1_pre==1 & test3$covid==0]
sum2 %>% sumtable( out='csv', file='output/table1-3.csv', fixed.digits=TRUE, digits=3,
                   vars=c('IDX_AIR_RTMFM',
                          'IDX_RAIL_FRT_CARLOADS',
                          'IDX_RAIL_FRT_INTERMODAL',
                          'IDX_WATERBORNE_D11',
                          'IDX_TRUCK_D11',
                          'diesel_ppi',
                          'cpi_food',
                          'unemploy_rate',
                          'trans_emply'),
                   summ=c('mean(x)'))

sum3 <- test3[test3$covid==1]
sum3 %>% sumtable( out='csv', file='output/table1-4.csv', fixed.digits=TRUE, digits=3,
                   vars=c('IDX_AIR_RTMFM',
                          'IDX_RAIL_FRT_CARLOADS',
                          'IDX_RAIL_FRT_INTERMODAL',
                          'IDX_WATERBORNE_D11',
                          'IDX_TRUCK_D11',
                          'diesel_ppi',
                          'cpi_food',
                          'unemploy_rate',
                          'trans_emply'),
                   summ=c('mean(x)'))

### Summary Table: Controls
sumtable(test3, out='csv', file='output/table2-1.csv', fixed.digits=TRUE, digits=3,
         vars=c('treat2',
                'treat_cat',
                'treat_cont'),
         summ=c('mean(x)',
                'median(x)',
                'sd(x)',
                'min(x)',
                'max(x)'))

### Visualization
#histogram of route distance
hist(test3$Distance, xlab="Route Distance (mi)", main = "")

hist_data <- test3 %>% subset(select=c(Distance))
hist1 <- hist_data %>%
  ggplot(aes(x=Distance))+
  geom_histogram(bins=28, 
                 color="black", 
                 alpha=0.5) +
  xlab("Distance (mi)") +
  ylab("Count") +
  theme_light() 

#save hist
png(file="output/figure2.png",
    width=600, height=400)
hist1
dev.off()

#get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(test3$Distance)

#scatter plot of distance v rate
scat1 <- test3 %>% subset(select=c(Distance,rate,truck_ref_rpm_ppi,post1_pre))
#scat1 <- collap(test3, rate + truck_ref_rpm_ppi ~ Distance + post1_pre, FUN = fmean)

scat_1 <- scat1 %>% ggplot(aes(Distance, rate)) +
  geom_hex(bins = 30, color = "white")+
  scale_fill_gradient(low =  "light gray", high = "black")+
  ylab("Rate 1/2020 $")+
  xlab("Distance (mi)")+
  guides(fill=guide_legend(title="Count"))+
  geom_smooth(data= scat1[scat1$post1_pre==1 ], se=FALSE, col='red', size=1.5)+
  geom_smooth(data= scat1[scat1$post1_pre==0 ], se=FALSE, col='red4', size=1.5, linetype = "twodash")+
  theme_minimal() +
  theme(text = element_text(size = 20))

scat_2 <- scat1 %>% ggplot(aes(Distance, truck_ref_rpm_ppi)) +
  geom_hex(bins = 30, color = "white")+
  scale_fill_gradient(low =  "light gray", high = "black")+
  ylab("Rate per Mile 1/2020 $")+
  xlab("Distance (mi)")+
  guides(fill=guide_legend(title="Count"))+
  geom_smooth(data= scat1[scat1$post1_pre==1 ], se=FALSE, col='red', size=1.5)+
  geom_smooth(data= scat1[scat1$post1_pre==0 ], se=FALSE, col='red4', size=1.5, linetype = "twodash")+
  theme_minimal() +
  theme(text = element_text(size = 20))

# #gen change in time for rate distance
# scat2 <- collap(test3, rate + truck_ref_rpm_ppi ~ Distance + post1_pre, FUN = fmean)
# scat2 %>% ggplot(aes(Distance, rate, group=post1_pre, color=post1_pre)) +
#   geom_line()

#save scatters
png(file="output/figure4.png",
    width=900, height=750)
scat_1
dev.off()

png(file="output/figure6.png",
    width=900, height=750)
scat_2
dev.off()


##month graph rate
test4 <- collap(test3, rate ~ tm1 + treat_cat, FUN = fmean)
test4$treat_cat <- as.factor(test4$treat_cat)

#make labels and breaks for xaxis
year_lab = tibble(etime =c(-35,-23,-11,1,13,25,37,49),
                  lab = c("2015","2016","2017", "2018", "2019", "2020", "2021", "2022"))

#graph
g_rate_month <- test4 %>%
  ggplot( aes(x=tm1,y=rate, group=treat_cat, colour=treat_cat)) + 
  geom_rect(aes(xmin = -24, xmax = -12, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "grey90", linetype=0, show.legend = FALSE) +
  geom_rect(aes(xmin = 0, xmax = 24, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "grey90", linetype=0, show.legend = FALSE) +
  # geom_rect(aes(xmin = 27, xmax = Inf, ymin = -Inf, ymax = Inf),
  #           alpha = 1/5, fill = "light gray", linetype=0, show.legend = FALSE) +
  geom_line(size=1.5) +
  geom_vline(xintercept = -4, linetype="dashed", 
             color = "black", size=1.5) +
  geom_vline(xintercept = 27, linetype="dashed", 
             color = "red", size=1.5) +
  coord_cartesian(ylim=c(1200,9000), clip='off') +
  xlab("Year") +
  ylab("Rate 1/2020 $") +
  scale_x_continuous(breaks = year_lab$etime, labels = year_lab$lab) +
  scale_color_grey(name = "Distance", labels = c("200-500", "501-1000", 
                                                     "1001-1500","1501-2000",
                                                     "2001+")) +
  annotate("text", x=-18.1, y=9200, label= "Rule Final", size=5) +
  annotate("text", x=-6, y=9500, label= "Phase 1", size=5) +
  annotate("text", x=12, y=9200, label= "Phase 2", size=5) +
  annotate("text", x=37, y=9500, label= "Phase 3", size=5) +
  annotate("text", x=31.5, y=9200, label= "COVID", size=5) +
  theme_light() +
  theme(text = element_text(size = 20))+
  theme(plot.margin = margin(0.6,0.2,0.2,0.2, "cm"))

#save
png(file="output/figure3.png",
    width=900, height=750)
g_rate_month
dev.off()

##month graph rate/mi
test4 <- collap(test3, truck_ref_rpm_ppi ~ tm1 + treat_cat, FUN = fmean)
test4$treat_cat <- as.factor(test4$treat_cat)

#make labels and breaks for xaxis
year_lab = tibble(etime =c(-35,-23,-11,1,13,25,37,49),
                  lab = c("2015","2016","2017", "2018", "2019", "2020", "2021", "2022"))

#graph
g_rpm_month <- test4 %>%
  ggplot( aes(x=tm1,y=truck_ref_rpm_ppi, group=treat_cat, colour=treat_cat)) + 
  geom_rect(aes(xmin = -24, xmax = -12, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "grey90", linetype=0, show.legend = FALSE) +
  geom_rect(aes(xmin = 0, xmax = 24, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "grey90", linetype=0, show.legend = FALSE) +
  geom_line(size=1.5) +
  geom_vline(xintercept = -4, linetype="dashed", 
             color = "black", size=1.5) +
  geom_vline(xintercept = 27, linetype="dashed", 
             color = "red", size=1.5) +
  coord_cartesian(ylim=c(2,6), clip='off') +
  xlab("Year") +
  ylab("Rate per Mile 1/2020 $") +
  scale_x_continuous(breaks = year_lab$etime, labels = year_lab$lab) +
  scale_color_grey(name = "Distance", labels = c("200-500", "501-1000", 
                                                     "1001-1500","1501-2000",
                                                     "2001+")) +
  annotate("text", x=-18.1, y=6.10, label= "Rule Final", size=5) +
  annotate("text", x=-6, y=6.25, label= "Phase 1", size=5) +
  annotate("text", x=12, y=6.10, label= "Phase 2", size=5) +
  annotate("text", x=37, y=6.25, label= "Phase 3", size=5) +
  annotate("text", x=31.5, y=6.10, label= "COVID", size=5) +
  theme_light() +
  theme(text = element_text(size = 20))+
  theme(plot.margin = margin(0.6,0.2,0.2,0.2, "cm"))

#save
png(file="output/figure5.png",
    width=900, height=750)
g_rpm_month
dev.off()

##month graph availability
test4 <- collap(test3, Availability ~ tm1 + treat_cat, FUN = fmean)
test4$treat_cat <- as.factor(test4$treat_cat)

#make labels and breaks for xaxis
year_lab = tibble(etime =c(-35,-23,-11,1,13,25,37,49),
                  lab = c("2015","2016","2017", "2018", "2019", "2020", "2021", "2022"))

#graph
g_avail_month <- test4 %>%
  ggplot( aes(x=tm1,y=Availability, group=treat_cat, colour=treat_cat)) + 
  geom_rect(aes(xmin = -24, xmax = -12, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "grey90", linetype=0, show.legend = FALSE) +
  geom_rect(aes(xmin = 0, xmax = 24, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "grey90", linetype=0, show.legend = FALSE) +
  geom_line(size=1.5) +
  geom_vline(xintercept = -4, linetype="dashed", 
             color = "black", size=1.5) +
  geom_vline(xintercept = 27, linetype="dashed", 
             color = "red", size=1.5) +
  coord_cartesian(ylim=c(1,5), clip='off') +
  xlab("Year") +
  ylab("Availability Index") +
  scale_x_continuous(breaks = year_lab$etime, labels = year_lab$lab) +
  scale_color_grey(name = "Distance", labels = c("200-500", "501-1000", 
                                                     "1001-1500","1501-2000",
                                                     "2001+")) +
  annotate("text", x=-18.1, y=5.10, label= "Rule Final", size=5) +
  annotate("text", x=-6, y=5.25, label= "Phase 1", size=5) +
  annotate("text", x=12, y=5.10, label= "Phase 2", size=5) +
  annotate("text", x=37, y=5.25, label= "Phase 3", size=5) +
  annotate("text", x=31.5, y=5.10, label= "COVID", size=5) +
  theme_light() +
  theme(text = element_text(size = 20))+
  theme(plot.margin = margin(0.6,0.2,0.2,0.2, "cm"))

#save
png(file="output/figure9.png",
    width=900, height=750)
g_avail_month
dev.off()

##month graph total employment in truck transport
test4 <- collap(test3, trans_emply ~ tm1, FUN = fmean)
#rescale var
test4$trans_emply <- test4$trans_emply / 1000
#make labels and breaks for xaxis
year_lab = tibble(etime =c(-35,-23,-11,1,13,25,37,49),
                  lab = c("2015","2016","2017", "2018", "2019", "2020", "2021", "2022"))

#graph
g_employ <- test4 %>%
  ggplot( aes(x=tm1,y=trans_emply)) + 
  geom_rect(aes(xmin = -24, xmax = -12, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "light gray", linetype=0, show.legend = FALSE) +
  geom_rect(aes(xmin = 0, xmax = 24, ymin = -Inf, ymax = Inf),
            alpha = 1/5, fill = "light gray", linetype=0, show.legend = FALSE) +
  geom_line(size=1.5) +
  geom_vline(xintercept = -4, linetype="dashed", 
             color = "black", size=1.5) +
  geom_vline(xintercept = 27, linetype="dashed", 
             color = "red", size=1.5) +
  coord_cartesian(ylim=c(1400,1550), clip='off') +
  xlab("Year") +
  ylab("Truck Transportation Employment (thousands)") +
  scale_x_continuous(breaks = year_lab$etime, labels = year_lab$lab) +
  scale_color_discrete(name = "Distance", labels = c("200-500", "501-1000", 
                                                     "1001-1500","1501-2000",
                                                     "2001+")) +
  annotate("text", x=-18.1, y=1555, label= "Rule Final", size=5) +
  annotate("text", x=-6, y=1560, label= "Phase 1", size=5) +
  annotate("text", x=12, y=1555, label= "Phase 2", size=5) +
  annotate("text", x=37, y=1560, label= "Phase 3", size=5) +
  annotate("text", x=31.5, y=1555, label= "COVID", size=5) +
  theme_light() +
  theme(text = element_text(size = 20))+
  theme(plot.margin = margin(0.6,0.2,0.2,0.2, "cm"))
#save
png(file="output/figure10.png",
    width=900, height=750)
g_employ
dev.off()

# ##year graph self-employed in trucking
# test5 <- fread("data/selfemploy.csv")
# g_selfemp <- test5 %>%
#   ggplot( aes(x=Year,y=self_employ)) +
#   geom_line(size=1.5) +
#   xlab("Year") +
#   ylab("Transportation and Warehousing Self-Employment (thousands)") +
#   theme_light() +
#   theme(text = element_text(size = 20))+
#   theme(plot.margin = margin(0.6,0.2,0.2,0.2, "cm"))
# 
# #save
# png(file="output/figure11.png",
#     width=600, height=500)
# g_selfemp
# dev.off()

### Event Studies
##quart small subset
main4 = main1[main1$tq1 >= -23 & main1$tq1 <= 16]
main4 <- main4[main4$Year > 2014]
#additional filters to solve issue of changing sample
main4 <- main4[main4$count>299]
main4 <- main4[main4$Distance>200]

## create interaction var: dist<1000 as nontreated
main6 <- main4
#trim vars for speed
main6 <- subset(main6, select=c(Distance, tq1, treat, treat2, treat3, treat_cat, truck_ref_rpm_ppi, rate, IDX_AIR_RTMFM, IDX_RAIL_FRT_CARLOADS, IDX_RAIL_FRT_INTERMODAL, IDX_WATERBORNE_D11, IDX_TRUCK_D11, diesel_cpi,unemploy_rate,cpi_food,Week,Year,Quarter,Month,yquart,trans_emply))
#need to bin never treated (<1000) into the omitted quarter, in this case -2
#main6$tq1[main6$Distance<1000] <- -2
#above does not impact graph; iplot does this already

## iplot Event Studies
#all model, rate. 200-1000 as reference.
es1 = feglm(rate ~ 
              i(tq1,treat2,-2) +
              IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
              diesel_cpi + unemploy_rate + cpi_food + trans_emply | Week + yquart + treat2, main6)
#plot
iplot(es1, main="", xlab="Year Quarter Event Time")
#export save above plot at 900 by 600! cant figure out how to export iplot auto

#subset model, rate/mile. 500-1000 as reference.
main6_ <- main6#[main6$treat_cat!=0]
es2 = feglm(truck_ref_rpm_ppi ~ 
              i(tq1,treat2,-2) + 
              IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
              diesel_cpi + unemploy_rate + cpi_food + trans_emply | Week + yquart + treat2, main6_)
#plot
iplot(es2, main="", xlab="Year Quarter Event Time")
#export save above plot at 900 by 600! cant figure out how to export iplot auto

### Model
## Total Rate
#binary treat
rate_glm1 <- feglm(rate ~ post1_pre * treat2 + covid + covid:treat2 +
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_ppi + unemploy_rate + cpi_food + trans_emply | Week + yquart, test3) 
#categorical treat
rate_glm2 <- feglm(rate ~ post1_pre * treat_cat +  covid + covid:treat_cat +
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_ppi + unemploy_rate + cpi_food + trans_emply | Week + yquart, test3) 
#continuous treat
rate_glm3 <- feglm(rate ~ post1_pre * treat_cont + covid + covid:treat_cont +
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_ppi + unemploy_rate + cpi_food + trans_emply | Week + yquart, test3) 
#compile
etable(rate_glm1,rate_glm2,rate_glm3)

## Total Rate: best time subset
#this filter again...
test3.1 <- test3[test3$Year > 2014]
test3.1 <- test3[test3$tq1 > -9]
test3.1 <- test3.1[test3.1$tq1 < 5]

#binary treat
rate_glm1.1 <- feglm(rate ~ post1_pre*treat2 + 
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_ppi + unemploy_rate + cpi_food + trans_emply  | Week + yquart, test3.1) 
#categorical treat
rate_glm2.1 <- feglm(rate ~ post1_pre*treat_cat + 
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_ppi + unemploy_rate + cpi_food + trans_emply | Week + yquart, test3.1) 
#continuous treat
rate_glm3.1 <- feglm(rate ~ post1_pre*treat_cont + 
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_ppi + unemploy_rate + cpi_food + trans_emply | Week + yquart, test3.1) 

#continuous treat logged; robust
rate_glm3.1.lg <- feglm(rate_log ~ post1_pre*treat_cont + 
                       IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                       diesel_ppi + unemploy_rate + cpi_food + trans_emply  | Week + yquart, test3.1) 
#compile
etable(rate_glm1,rate_glm2,rate_glm3,rate_glm3.1)

## compile prior two sections for paper
library(writexl)
etable(rate_glm1,rate_glm2,rate_glm3,rate_glm3.1) %>% write_xlsx("output/reg_output.xlsx")


##Vary FE for cont treat
#continuous treat
month <- feglm(rate ~ post1_pre * treat_cont + covid + covid:treat_cont +
                 IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                 diesel_ppi + unemploy_rate + cpi_food + trans_emply  | Month, test3) 

week <- feglm(rate ~ post1_pre * treat_cont + covid + covid:treat_cont +
                       IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                       diesel_ppi + unemploy_rate + cpi_food + trans_emply  | Week, test3) 

week_year <- feglm(rate ~ post1_pre * treat_cont + covid + covid:treat_cont +
                IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                diesel_ppi + unemploy_rate + cpi_food + trans_emply  | Week + Year, test3) 

week_yquart <- feglm(rate ~ post1_pre * treat_cont + covid + covid:treat_cont +
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_ppi + unemploy_rate + cpi_food + trans_emply  | Week + yquart, test3) 
#compile
etable(month,week,week_year,week_yquart)

library(writexl)
etable(month,week,week_year,week_yquart) %>% write_xlsx("output/reg_robust1.xlsx")


## Placebo tests
test2 <- main1
test3_ <- test2[test2$Year >= 2012]
test3_ <- test3_[test3_$tm1 < -4]
test3_ <- test3_[test3_$count>299]
test3_ <- test3_[test3_$Distance>200]

#Rule Final 12/16/2015: continuous treat; 1/2012-7/2017 
rate_glm1.p <- feglm(rate ~ post0 * treat_cont + 
                     IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                     diesel_cpi + unemploy_rate + cpi_food + trans_emply  | Week + yquart, test3_) 
#Phase 1 2/16/2016: continuous treat; 1/2012-7/2017 
rate_glm2.p <- feglm(rate ~ post05 * treat_cont + 
                       IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                       diesel_cpi + unemploy_rate + cpi_food + trans_emply  | Week + yquart, test3_) 
#Phase 3 12/16/2019: continuous treat; 1/2015-2/2020 
rate_glm3.p <- feglm(rate ~ post2 * treat_cont + covid + covid:treat_cont +
                       IDX_AIR_RTMFM + IDX_RAIL_FRT_CARLOADS + IDX_RAIL_FRT_INTERMODAL + IDX_WATERBORNE_D11 + IDX_TRUCK_D11 +
                       diesel_cpi + unemploy_rate + cpi_food + trans_emply  | Week + yquart, test3) 

#compile
etable(rate_glm1.p,rate_glm2.p,rate_glm3.p)

library(writexl)
etable(rate_glm1.p,rate_glm2.p,rate_glm3.p) %>% write_xlsx("output/reg_placebo.xlsx")