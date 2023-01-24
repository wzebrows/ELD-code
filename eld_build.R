library(data.table)
library(tidyr)
library(dplyr)
library(collapse)

#setup
rm(list = ls())

### Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### load data
truck_ref1 = fread("data/Refrigerated_Truck_Rates_and_Availability.csv")
truck_all = fread("data/Truck_Availability_Index.csv")
fatal = fread("data/table_02_01_051822.csv")
trans_stats = fread("data/Monthly_Transportation_Statistics.csv")
indexs = fread("data/Transportation_Services_Index_and_Seasonally-Adjusted_Transportation_Data.csv")
conv = fread("data/inflation conversion cpi.csv")
cpi = fread("data/cpi.csv")
truck_ppi = fread("data/ppi truck transport.csv")
cpi_food = fread("data/cpi all food.csv")
diesel = fread("data/diesel price.csv")
diesel_ppi = fread("data/diesel ppi noadjust.csv")
unemp = fread("data/unemployment.csv")
tfatal = fread("data/truck_fatalities.csv")

### these data are not used in analysis, but interesting nonetheless. Removing them likely to cause coding issues due to variable references; little payoff to fixing! Not THAT many MBs of data
rail_coal = fread("data/Coal_Unit_Train_Loadings.csv")
rail_grain1 = fread("data/Grain_Car_Order_Fulfillment_for_Manifest_Service.csv")
rail_grain2 = fread("data/Grain_Rail_Cars_Loaded_and_Billed.csv")
truck_grain = fread("data/Grain_Truck_Use_Index.csv")
rail_inactive = fread("data/Rail_Cars_Not_Moved.csv")
rail_bids = fread("data/Secondary_Railcar_Auction_Market_Bids.csv")
rail_grain3 = fread("data/U.S._Rail_Tariff_Rates_for_Grain_and_Soybeans.csv")
rail_grain4 = fread("data/U.S_Rail_Tariff_Rates_for_Ethanol.csv")
freight_tons= fread("data/table_01_50_042522.csv")

### melt cpi
#melt
cpi <- melt(cpi, id=c("Year"), variable.name = "Month", value.name = "cpi")
cpi$Month = as.numeric(cpi$Month)

cpi_food <- melt(cpi_food, id=c("Year"), variable.name = "Month", value.name = "cpi_food")
cpi_food$Month = as.numeric(cpi_food$Month)

diesel <- melt(diesel, id=c("Year"), variable.name = "Month", value.name = "diesel")
diesel$Month = as.numeric(diesel$Month)

diesel_ppi <- melt(diesel_ppi, id=c("Year"), variable.name = "Month", value.name = "diesel_ppi")
diesel_ppi$Month = as.numeric(diesel_ppi$Month)

#scale CPI/PPI to 2020 jan
cpi_food$cpi_food = cpi_food$cpi_food / cpi_food$cpi_food[cpi_food$Year==2020 & cpi_food$Month==1]
diesel_ppi$diesel_ppi = diesel_ppi$diesel_ppi / diesel_ppi$diesel_ppi[diesel_ppi$Year==2020 & diesel_ppi$Month==1]

###Restructure Truck PPI format
truck_ppi$Month = sub('.','',truck_ppi$Period) %>% as.numeric 
truck_ppi$truck_ppi = truck_ppi$Value
truck_ppi = truck_ppi %>% subset(select=c(Year,Month,truck_ppi))
truck_ppi$truck_ppi = truck_ppi$truck_ppi / truck_ppi$truck_ppi[truck_ppi$Year==2020 & truck_ppi$Month==1]

### Collapse by Year and year-month
##gen quarters
truck_ref1$quarter <- 1 + floor((truck_ref1$Month-1)/3)
rail_grain1$quarter <- 1 + floor((rail_grain1$Month-1)/3)
rail_grain2$quarter <- 1 + floor((rail_grain2$Month-1)/3)
rail_grain3$quarter <- 1 + floor((rail_grain3$Month-1)/3)
rail_grain4$quarter <- 1 + floor((rail_grain4$Month-1)/3)
rail_inactive$quarter <- 1 + floor((rail_inactive$Month-1)/3)
rail_bids$quarter <- 1 + floor((rail_bids$Month-1)/3)
rail_coal$quarter <- 1 + floor((rail_coal$Month-1)/3)
conv$quarter <- 1 + floor((conv$Month-1)/3)
cpi$quarter <- 1 + floor((as.numeric(cpi$Month)-1)/3)
cpi_food$quarter <- 1 + floor((as.numeric(cpi_food$Month)-1)/3)
unemp$quarter <- 1 + floor((as.numeric(unemp$Month)-1)/3)
diesel$quarter <- 1 + floor((as.numeric(diesel$Month)-1)/3)
tfatal$quarter <- 1 + floor((as.numeric(tfatal$Month)-1)/3)
truck_ppi$quarter <- 1 + floor((as.numeric(truck_ppi$Month)-1)/3)
diesel_ppi$quarter <- 1 + floor((as.numeric(diesel_ppi$Month)-1)/3)

##Refridgeration
truck_ref1$truck_ref_rate <- as.numeric(gsub(",", "", truck_ref1$Midpoint))
truck_ref1$truck_ref_rpm <- truck_ref1$"Rate Per Mile"
truck_ref1$truck_ref_dist <- truck_ref1$Distance
truck_ref1$truck_ref_avail <- truck_ref1$Availability

#collapse
a <- collap(truck_ref1, truck_ref_rate + truck_ref_dist + truck_ref_rpm + truck_ref_avail ~ Year, FUN = fmean)
a1 <- collap(truck_ref1, truck_ref_rate + truck_ref_dist + truck_ref_rpm + truck_ref_avail ~ Year + Month, FUN = fmean)
a2 <- collap(truck_ref1, truck_ref_rate + truck_ref_dist + truck_ref_rpm + truck_ref_avail ~ Year + quarter, FUN = fmean)

##Grain Rail
#rename
rail_grain1$rail_grain_unfilled <- rail_grain1$"Total Unfilled Orders"
rail_grain1$rail_grain_filled <- rail_grain1$Filled

#collapse
c <- collap(rail_grain1, rail_grain_unfilled + rail_grain_filled ~ Year, FUN = sum)
c1 <- collap(rail_grain1, rail_grain_unfilled + rail_grain_filled ~ Year + Month, FUN = sum)
c2 <- collap(rail_grain1, rail_grain_unfilled + rail_grain_filled ~ Year + quarter, FUN = sum)

#rename
rail_grain2$rail_grain_loaded <- rail_grain2$All

#collapse
d <- collap(rail_grain2, rail_grain_loaded ~ Year, FUN = sum)
d1 <- collap(rail_grain2, rail_grain_loaded ~ Year + Month, FUN = sum)
d2 <- collap(rail_grain2, rail_grain_loaded ~ Year + quarter, FUN = sum)

#gen tariff per 1k miles
rail_grain3$rail_grain_tariff.bu.1k <- rail_grain3$"Tariff plus FSC, per BU" / 
  as.numeric(gsub(",", "", rail_grain3$Mileage)) * 1000

#collapse
e <- collap(rail_grain3, rail_grain_tariff.bu.1k ~ Year, FUN = list(sum,fmean))
e1 <- collap(rail_grain3, rail_grain_tariff.bu.1k ~ Year + Month, FUN = list(sum,fmean))
e2 <- collap(rail_grain3, rail_grain_tariff.bu.1k ~ Year + quarter, FUN = list(sum,fmean))


#gen tariff per 1k miles
rail_grain4$"Rate plus Fuel Surcharge/Gallon"[is.na(rail_grain4$"Rate plus Fuel Surcharge/Gallon")] <- rail_grain4$"Rate/Gallon"[is.na(rail_grain4$"Rate plus Fuel Surcharge/Gallon")]
rail_grain4$rail_eth_tariff.g.1k <- rail_grain4$"Rate plus Fuel Surcharge/Gallon" / 
  as.numeric(gsub(",", "", rail_grain4$Mileage)) * 1000

#drop NA from some logs missing mileage
rail_grain4 <- rail_grain4 %>% drop_na(rail_eth_tariff.g.1k)

#collapse
f <- collap(rail_grain4, rail_eth_tariff.g.1k ~ Year, FUN = list(sum,fmean))
f1 <- collap(rail_grain4, rail_eth_tariff.g.1k ~ Year + Month, FUN = list(sum,fmean))
f2 <- collap(rail_grain4, rail_eth_tariff.g.1k ~ Year + quarter, FUN = list(sum,fmean))

##Inactive Rail
#make var name usable
rail_inactive$rail_cars.not.moved <- rail_inactive$"Cars Not Moved in Over 48 Hours"
#turn NA to 0s
rail_inactive$rail_cars.not.moved[is.na(rail_inactive$rail_cars.not.moved)] <- 0
#gen commodity dummies
library(fastDummies)
rail_inactive.1<-dummy_cols(rail_inactive, select_columns = c("Commodity"))

#collapse
g <- collap(rail_inactive.1, rail_cars.not.moved ~ Year + Commodity_Ethanol + Commodity_Grain, FUN = list(sum))
g1 <- collap(rail_inactive.1, rail_cars.not.moved ~ Year + Month + Commodity_Ethanol + Commodity_Grain, FUN = list(sum))
g2 <- collap(rail_inactive.1, rail_cars.not.moved ~ Year + quarter + Commodity_Ethanol + Commodity_Grain, FUN = list(sum))


#simplify commodity var
g$commodity = 1 + g$Commodity_Grain + 2*g$Commodity_Ethanol
g1$commodity = 1 + g1$Commodity_Grain + 2*g1$Commodity_Ethanol
g2$commodity = 1 + g2$Commodity_Grain + 2*g2$Commodity_Ethanol

g$commodity[g$commodity==1] <- "other"
g$commodity[g$commodity==2] <- "grain"
g$commodity[g$commodity==3] <- "ethanol"
g1$commodity[g1$commodity==1] <- "other"
g1$commodity[g1$commodity==2] <- "grain"
g1$commodity[g1$commodity==3] <- "ethanol"
g2$commodity[g2$commodity==1] <- "other"
g2$commodity[g2$commodity==2] <- "grain"
g2$commodity[g2$commodity==3] <- "ethanol"

#drop old commodity vars
g = subset(g, select = -c(Commodity_Grain,Commodity_Ethanol))
g1 = subset(g1, select = -c(Commodity_Grain,Commodity_Ethanol))
g2 = subset(g2, select = -c(Commodity_Grain,Commodity_Ethanol))

#turn long to wide by commodity type
g <- reshape(g,idvar="Year",timevar="commodity", direction = "wide")
g1 <- reshape(g1,idvar=c("Year","Month"),timevar="commodity", direction = "wide")
g2 <- reshape(g2,idvar=c("Year","quarter"),timevar="commodity", direction = "wide")

##Coal Rail
#rename and turn to numeric
rail_coal$rail_coal_avgloadings <- as.numeric(gsub(",", "", rail_coal$Loadings_Average))

#collapse
h <- collap(rail_coal, rail_coal_avgloadings ~ Year, FUN = list(sum))
h1 <- collap(rail_coal, rail_coal_avgloadings ~ Year + Month, FUN = list(sum))
h2 <- collap(rail_coal, rail_coal_avgloadings ~ Year + quarter, FUN = list(sum))


## Tranportation Indexs
#gen year and month and quarter
library(stringi)
indexs$Month <- stri_sub(indexs$ID,-2,-1) %>% as.numeric()
indexs$Year <- stri_sub(indexs$ID,-6,-3) %>% as.numeric()
indexs$quarter <- 1 + floor((indexs$Month-1)/3)

# keep vars we want
i1 <- subset(indexs, select = c(Year,Month,quarter,
                                      IDX_AIR_RTMFM,
                                      IDX_RAIL_FRT_CARLOADS,
                                      IDX_RAIL_FRT_INTERMODAL,
                                      IDX_WATERBORNE_D11,
                                      IDX_TRUCK_D11))

#collapse 
i <- collap(i1, IDX_AIR_RTMFM+IDX_RAIL_FRT_CARLOADS+IDX_RAIL_FRT_INTERMODAL+IDX_WATERBORNE_D11+IDX_TRUCK_D11 ~ Year, FUN = fmean)
i2 <- collap(i1, IDX_AIR_RTMFM+IDX_RAIL_FRT_CARLOADS+IDX_RAIL_FRT_INTERMODAL+IDX_WATERBORNE_D11+IDX_TRUCK_D11 ~ Year + quarter, FUN = fmean)


## Transport Stats
#gen year and month and quarter
trans_stats$Month <- stri_sub(trans_stats$Date,-14,-13) %>% as.numeric()
trans_stats$Year <- stri_sub(trans_stats$Date,-9,-6) %>% as.numeric()
trans_stats$quarter <- 1 + floor((trans_stats$Month-1)/3)

#rename dumb vars not named for R usability
trans_stats$fatal_high <- trans_stats$"Highway Fatalities Per 100 Million Vehicle Miles Traveled"
trans_stats$trans_emply <- trans_stats$"Transportation Employment - Truck Transportation"


# keep vars we want
j1 <- subset(trans_stats, select = c(Year,Month,quarter,
                                fatal_high,trans_emply))

#collapse 
j <- collap(j1, fatal_high + trans_emply ~ Year, FUN = fmean)
j2 <- collap(j1, fatal_high + trans_emply ~ Year + quarter, FUN = fmean)

##inflation conversion
#rename set
k1 <- conv 
k <- collap(k1, conversion ~ Year, FUN = fmean)
k2 <- collap(k1, conversion ~ Year + quarter, FUN = fmean)

##cpi control
#rename set
l1 <- cpi
l <- collap(l1, cpi ~ Year, FUN = fmean)
l2 <- collap(l1, cpi ~ Year + quarter, FUN = fmean)

##cpi_food control
#rename set
m1 <- cpi_food
m <- collap(m1, cpi_food ~ Year, FUN = fmean)
m2 <- collap(m1, cpi_food ~ Year + quarter, FUN = fmean)

##unemployment control
#rename set
o1 <- unemp
o <- collap(o1, unemploy_rate ~ Year, FUN = fmean)
o2 <- collap(o1, unemploy_rate ~ Year + quarter, FUN = fmean)

##diesel control
#rename set
p1 <- diesel
p <- collap(p1, diesel ~ Year, FUN = fmean)
p2 <- collap(p1, diesel ~ Year + quarter, FUN = fmean)

##truck fatalities, monthly
q1 <- tfatal
q <- collap(q1, lg_truck_fatal ~ Year, FUN = fmean)
q2 <- collap(q1, lg_truck_fatal ~ Year + quarter, FUN = fmean)

##diesel ppi
r1 <- diesel_ppi
r <- collap(r1, diesel_ppi ~ Year, FUN = fmean)
r2 <- collap(r1, diesel_ppi ~ Year + quarter, FUN = fmean)

##truck ppi
t1 <- truck_ppi
t <- collap(t1, truck_ppi ~ Year, FUN = fmean)
t2 <- collap(t1, truck_ppi ~ Year + quarter, FUN = fmean)

### Aggregate Data
## Yearly Data
yearly <- full_join(a,c, by = "Year") %>% 
            full_join(d, by = "Year") %>%
            full_join(e, by = "Year") %>%
            full_join(f, by = "Year") %>%
            full_join(g, by = "Year") %>%
            full_join(h, by = "Year") %>%
            full_join(i, by = "Year") %>%
            full_join(j, by = "Year") %>%
            full_join(k, by = "Year") %>%
            full_join(l, by = "Year") %>%
            full_join(m, by = "Year") %>%
            #full_join(n, by = "Year") %>%
            full_join(o, by = "Year") %>%
            full_join(p, by = "Year") %>%
            full_join(q, by = "Year") %>%
            full_join(r, by = "Year") %>%
            full_join(t, by = "Year") 

#drop 2022 as incomplete data
yearly <- yearly[yearly$Year != 2022]

#drop some bad values from only partial year data
yearly$rail_grain_unfilled[yearly$Year==2017] <- NA
yearly$rail_grain_filled[yearly$Year==2017] <- NA
yearly$rail_grain_loaded[yearly$Year==2014] <- NA
yearly$rail_cars.not.moved.other[yearly$Year==2017] <- NA
yearly$rail_cars.not.moved.grain[yearly$Year==2017] <- NA
yearly$rail_cars.not.moved.ethanol[yearly$Year==2017] <- NA
yearly$rail_coal_avgloadings[yearly$Year==2017] <- NA

## Monthly Data
monthly <- full_join(a1,c1, by = c("Year","Month")) %>% 
            full_join(d1, by = c("Year","Month")) %>%
            full_join(e1, by = c("Year","Month")) %>%
            full_join(f1, by = c("Year","Month")) %>%
            full_join(g1, by = c("Year","Month")) %>%
            full_join(h1, by = c("Year","Month")) %>%
            full_join(i1, by = c("Year","Month")) %>%
            full_join(j1, by = c("Year","Month")) %>%
            full_join(k1, by = c("Year","Month")) %>%
            full_join(l1, by = c("Year","Month")) %>%
            full_join(m1, by = c("Year","Month")) %>%
            #full_join(n1, by = c("Year","Month")) %>%
            full_join(o1, by = c("Year","Month")) %>%
            full_join(p1, by = c("Year","Month")) %>%
            full_join(q1, by = c("Year","Month")) %>%
            full_join(r1, by = c("Year","Month")) %>%
            full_join(t1, by = c("Year","Month")) 

#drop some bad values from months with only partial week data
monthly$rail_grain_unfilled[monthly$Year==2017 & monthly$Month==3] <- NA
monthly$rail_grain_filled[monthly$Year==2017 & monthly$Month==3] <- NA
monthly$rail_grain_loaded[monthly$Year==2014 & monthly$Month==10] <- NA
monthly$rail_cars.not.moved.other[monthly$Year==2017 & monthly$Month==3] <- NA
monthly$rail_cars.not.moved.grain[monthly$Year==2017 & monthly$Month==3] <- NA
monthly$rail_cars.not.moved.ethanol[monthly$Year==2017 & monthly$Month==3] <- NA
monthly$rail_coal_avgloadings[monthly$Year==2017 & monthly$Month==3] <- NA
monthly <- monthly[monthly$Year!=2022 | monthly$Month!=5]

monthly <- monthly[monthly$Year>=2000]

## Quarterly Data
#rename a few vars for merging
truck_all$rail_all_avail <- truck_all$Index
truck_grain$rail_grain_avail <- truck_grain$Index
truck_all$quarter <- truck_all$Quarter
truck_grain$quarter <- truck_grain$Quarter

#subset to national
truck_all.1 <- truck_all[truck_all$Region=="National"]
truck_grain.1 <- truck_grain[truck_grain$Region=="National"]

#drop unneeded vars
truck_all.1 <- subset(truck_all.1, select = c(Year,quarter,rail_all_avail))
truck_grain.1 <- subset(truck_grain.1, select = c(Year,quarter,rail_grain_avail))

#aggregate
quarterly <- full_join(a2,c2, by = c("Year","quarter")) %>% 
            full_join(d2, by = c("Year","quarter")) %>%
            full_join(e2, by = c("Year","quarter")) %>%
            full_join(f2, by = c("Year","quarter")) %>%
            full_join(g2, by = c("Year","quarter")) %>%
            full_join(h2, by = c("Year","quarter")) %>%
            full_join(i2, by = c("Year","quarter")) %>%
            full_join(j2, by = c("Year","quarter")) %>%
            full_join(truck_all.1, by = c("Year","quarter")) %>%
            full_join(truck_grain.1, by = c("Year","quarter")) %>%
            full_join(k2, by = c("Year","quarter")) %>%
            full_join(l2, by = c("Year","quarter")) %>%
            full_join(m2, by = c("Year","quarter")) %>%
            #full_join(n2, by = c("Year","quarter")) %>%
            full_join(o2, by = c("Year","quarter")) %>%
            full_join(p2, by = c("Year","quarter")) %>%
            full_join(q2, by = c("Year","quarter")) %>%
            full_join(r2, by = c("Year","quarter")) %>%
            full_join(t2, by = c("Year","quarter")) 

#drop some bad values from quarters with missing values, based on month data
quarterly$sum.rail_grain_tariff.bu.1k[quarterly$Year==2010 & quarterly$quarter==2] <- NA
quarterly$fmean.rail_grain_tariff.bu.1k[quarterly$Year==2010 & quarterly$quarter==2] <- NA

quarterly$truck_ref_dist[quarterly$Year==2021 & quarterly$quarter==4] <- NA
quarterly$truck_ref_rpm[quarterly$Year==2021 & quarterly$quarter==4] <- NA
quarterly$truck_ref_avail[quarterly$Year==2021 & quarterly$quarter==4] <- NA

quarterly$rail_grain_unfilled[quarterly$Year==2017 & quarterly$quarter==1] <- NA
quarterly$rail_grain_filled[quarterly$Year==2017 & quarterly$quarter==1] <- NA
quarterly$rail_cars.not.moved.other[quarterly$Year==2017 & quarterly$quarter==1] <- NA
quarterly$rail_cars.not.moved.grain[quarterly$Year==2017 & quarterly$quarter==1] <- NA
quarterly$rail_cars.not.moved.ethanol[quarterly$Year==2017 & quarterly$quarter==1] <- NA
quarterly$rail_coal_avgloadings[quarterly$Year==2017 & quarterly$quarter==1] <- NA

quarterly <- quarterly[quarterly$Year!=2022 | quarterly$quarter!=2] 

## export
write.csv(yearly,"ELD_year.csv")
write.csv(monthly,"ELD_month.csv")
write.csv(quarterly,"ELD_quarter.csv")


###########################
### Gen analysis set

library(data.table)
library(tidyr)
library(dplyr)
library(collapse)
library(fixest)
library(fastDummies)
library(ggplot2)
#one day, try kableExtra package for reg compilation

#setup
rm(list = ls())
set.seed = 666

### Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Load indv route data

truck_ref1 = fread("data/Refrigerated_Truck_Rates_and_Availability.csv")
#truck_ref2 = fread("data/Refrigerated_Truck_Volumes.csv")

### Load and manipulate indp vars
vars = fread("ELD_month.csv")

#specify main data, merge vars
main <- left_join(truck_ref1, vars, by = c("Year","Month"))
main <- main %>% subset(select=-c(truck_ref_rate,truck_ref_rpm,truck_ref_dist,truck_ref_avail))

#reformat Distance to numeric and removes commas
main$Distance <- as.numeric(gsub(",", "", main$Distance))
#rename some vars
main$truck_ref_rpm <- main$"Rate Per Mile"
main$truck_ref_avail <- main$Availability
#calc inflation 
main$truck_ref_rpm_ppi <- main$truck_ref_rpm / main$truck_ppi
main$diesel_cpi <- main$diesel * main$conversion
#gen ppi rate
main$rate = main$Midpoint / main$truck_ppi

#drop a bad obsv
main = main[!is.na(main$Distance)]

## gen various treatment specifications
#binary for non-local trips
main$treat <- 0
main$treat[main$Distance > 500] <- 1

#NEW binary treat for >500 subset
main$treat2 <- 0
main$treat2[main$Distance > 1000] <- 1

#another binary treat for >700; 1 day drive
main$treat3 <- 0
main$treat3[main$Distance > 700] <- 1

#categorical, based on local, short, medium, long specification (i think # are right)
main$treat_cat <- 0
main$treat_cat[main$Distance > 500 & main$Distance <= 1000] <- 0.25
main$treat_cat[main$Distance > 1000 & main$Distance <= 1500] <- 0.5
main$treat_cat[main$Distance > 1500 & main$Distance <= 2000] <- 0.75
main$treat_cat[main$Distance > 2000] <- 1

# cat n values
# x <= 500 :        24072
# 500 < x <= 1000:  31551
# 1000 < x <= 1500: 31815
# 1500 < x <= 2000: 24061
# 2000 < x :        67397

#categorical , treat for >500 subset
main$treat_cat2[main$Distance > 500 & main$Distance <= 1000] <- 0
main$treat_cat2[main$Distance > 1000 & main$Distance <= 1500] <- 0.333
main$treat_cat2[main$Distance > 1500 & main$Distance <= 2000] <- 0.666
main$treat_cat2[main$Distance > 2000] <- 1

#continuous, based on distance in 500mi (to be close to categories)
main$treat_cont <- 0
main$treat_cont[main$Distance > 500] <-  (main$Distance[main$Distance > 500] -500) /500

#NEW Cont. treat for >500 subset
main$treat_cont2 <- 0
main$treat_cont2[main$Distance > 1000] <-  (main$Distance[main$Distance > 1000] - 1000) /500

## gen various post specifications
#Phase in: ELD and AOBRD (phase 2) 12/18/2017
main$post1 <- 0
main$post1[main$Year>=2018] <- 1
main$post1[main$Year>=2017 & main$Week >=50] <- 1

#implementation deadline (phase 3) 12/16/2019
main$post2 <- 0
main$post2[main$Year>=2020] <- 1
main$post2[main$Year>=2019 & main$Week >=50] <- 1

#Phase in: ELD and AOBRD (phase 2) 12/18/2017
main$post1_pre <- 0
main$post1_pre[main$Year>=2018] <- 1
main$post1_pre[main$Year>=2017 & main$Month >=8] <- 1

#Phase in: ELD rule finalized 12/16/2015 (phase 1 "starts" 2/16/2016) 
main$post0 <- 0
main$post0[main$Year>=2016] <- 1
main$post0[main$Year>=2015 & main$Week >=50] <- 1

#phase 1
main$post05 <- 0
main$post05[main$Year>=2016] <- 1
main$post05[main$Year==2016 & main$Week <=6] <- 0

#covid shock
main$covid <- 0
main$covid[main$Year>=2021] <- 1
main$covid[main$Year>=2020 & main$Month >=3] <- 1

## Gen continous, uniform time
#week
main$t1 = (main$Year - 2000)*52 + main$Week - ((2017 - 2000)*52 + 50)
main$t2 = (main$Year - 2000)*52 + main$Week - ((2019 - 2000)*52 + 50)
#month
main$tm1 = (main$Year - 2000)*12 + main$Month - ((2017 - 2000)*12 + 12)
main$tm2 = (main$Year - 2000)*12 + main$Month - ((2019 - 2000)*12 + 12)
#quarter
main$tq1 = (main$Year - 2000)*4 + main$Quarter - ((2017 - 2000)*4 + 4)
main$tq2 = (main$Year - 2000)*4 + main$Month - ((2019 - 2000)*4 + 4)

## Gen Origin-Destination
main$route = paste0(main$Origin,"-",main$Destination)

## cut to pre 12/21, since data missing for that month
main <- main[main$Year < 2022]

## specify fresh dataset
main1 <- main

## add route-level occurance data for filtering purposes
#collapse to count # of obsv per route and avg rate per route
test1 <- main1
test1$count <- 1

test1 <- test1[test1$Year > 2011]
test1$rate_avg = test1$rate

merg1 <- collap(test1, count ~ route + treat_cat, FUN = sum)
merg2 <- collap(test1, rate_avg ~ route + treat_cat , FUN = fmean)

#merge onto main
main1 <- left_join(main1, merg1, by = c("route", "treat_cat")) %>%
  left_join(merg2, by = c("route", "treat_cat"))

#create year quarter var
main1$yquart <- main1$tq1

###write
write.csv(main1,"rtruck_analysis.csv")