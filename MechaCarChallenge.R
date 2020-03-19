library(tidyverse)

#Load the Mecha Car data
mecha_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#Print out a Covariance matrix
mpg_matrix <-as.matrix(mecha_mpg[,c("vehicle_length", "vehicle_weight", "spoiler_angle", "ground_clearance", "AWD", "mpg")])
cor(mpg_matrix)

#Create our multiple linear regression model and print out the summary table
mecha_multi <- lm(mpg~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)
summary(mecha_multi)

mecha_length<-lm(mpg~vehicle_length, data=mecha_mpg)
summary(mecha_length)
yvals <- mecha_length$coefficients['vehicle_length']*mecha_mpg$vehicle_length + mecha_length$coefficients['(Intercept)']

plt <- ggplot(mecha_mpg,aes(x=vehicle_length,y=mpg))
yvals
plt + geom_point()  + geom_line(aes(y=yvals), color = "red")

#Read in the suspension coil data
susp <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

susp_sum <- susp %>%
  group_by(Manufacturing_Lot) %>%
  summarize(Mean_PSI= mean(PSI),Median_PSI= median(PSI),Variance_PSI= var(PSI), StdDev_PSI= sd(PSI))

susp_sum

#Run T-Test for the whole sample group against the expected mean of 1500 PSI
t.test(susp$PSI, mu = 1500)

#Subset our dataset by lot number
Lot1 <- susp %>%
  subset(Manufacturing_Lot == "Lot1", select = PSI)

Lot2 <- susp %>%
  subset(Manufacturing_Lot == "Lot2", select = PSI)

Lot3 <- susp %>%
  subset(Manufacturing_Lot == "Lot3", select = PSI)

#Run T-Test for each lot against the expected mean of 1500 PSI
t.test(Lot1$PSI, mu = 1500)

t.test(Lot2$PSI, mu = 1500)

t.test(Lot3$PSI, mu = 1500)