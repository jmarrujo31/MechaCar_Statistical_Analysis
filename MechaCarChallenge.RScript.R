library(jsonlite)
library(tidyverse)
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD ,MechaCar_mpg) 


summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD ,MechaCar_mpg))


#deliverable2
Suspension_Coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

Suspension_Coilts <- Suspension_Coil %>% summarize(Mean=mean(PSI), 
                                                   Median=median(PSI),
                                                   Variance=var(PSI),
                                                   SD=sd(PSI))

Suspension_Coil1 <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), 
                                                                                  Median=median(PSI),
                                                                                  Variance=var(PSI),
                                                                                  SD=sd(PSI),
                                                                                  .groups = 'keep')


t.test(Manufacturing_lot, mu=mean(PSI))
