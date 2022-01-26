library(dplyr)
library(tidyverse)
## Mechanical car Data Summary
MechaCar_data <- read.csv('MechaCar_mpg.csv') #import dataset

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = MechaCar_data)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = MechaCar_data)) #summarize linear model

library(dplyr)
?summarize()
Suspension_data <- read.csv('Suspension_Coil.csv') #import dataset

total_summary<- summarize(Suspension_data, Mean = mean(PSI),Median = median(PSI),Variance = var(PSI),SD = sd(PSI) ,.groups = "keep")

total_summary

lot_summary<- Suspension_data %>%  group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI),Median = median(PSI),
                                                                             Variance = var(PSI),SD = sd(PSI) ,.groups = "keep")

lot_summary


## Box Plot for 3 Manufaturing_Lots
plt <- ggplot(Suspension_data,aes(y=PSI,color=Manufacturing_Lot)) #import dataset into ggplot2

plt + geom_boxplot() + facet_wrap(vars(Manufacturing_Lot)) 

## T - test
sample_table <- Suspension_data %>% sample_n(50)
t.test((sample_table$PSI),mu=1500) #compare sample versus population means


## T - test for Manufacturing Lot 1
mgf_LOT1 <- Suspension_data %>% filter(Manufacturing_Lot=='Lot1')

t.test((mgf_LOT1$PSI),mu=1500) #compare sample versus population means

## T - test for Manufacturing Lot 2
mgf_LOT2 <- Suspension_data %>% filter(Manufacturing_Lot=='Lot2')
t.test((mgf_LOT2$PSI),mu=1500) #compare sample versus population means
## T - test for Manufacturing Lot 3
mgf_LOT3 <- Suspension_data %>% filter(Manufacturing_Lot=='Lot3')
t.test((mgf_LOT1$PSI),mu=1500) #compare sample versus population means
