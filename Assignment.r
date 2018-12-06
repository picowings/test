#########################################################
## Special Topics in Quality Control 2018 Fall Term
## ID 201874116 
## Name : SUNGCHUL KIM
## -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
## Assignment #1 - Sample Data analysis with Graph
## Sample Data example is a case
## from Nut Runner (Torque Tools) 
## for Powertrain Assembly Line
## Version : v1.0
## Final Update : 2018.12.06
#########################################################

## Get random external data import

data <- read.table("c:\\data.txt",header=TRUE) # import data from Tab delimited text file


## table data import check by visual

head(data) # data import check for head area
tail(data) # data import check for tail area



## Data transfer from table

data_Torque <- table(data$Torque)
data_Angle <- table(data$Angle)
data_Time <- table(data$Time) 



### data transfer for relativity check

## Table data transfer 
## Criteria - Tighetning Result Torque value by Engine Number

table(data$Engine_Number,data$Torque)
xtabs(~data$Engine_Number + data$Torque)
Torque_data_by_Engine_Number <- table(data$Engine_Number,data$Torque)


## Table data transfer 
## Criteria - Tighetning Result Torque value by 10 axes Screw (Bolt/Nut) Number

table(data$Screw_Number,data$Torque)
xtabs(~data$Screw_Number + data$Torque)
Torque_data_by_Screw_Number <- table(data$Screw_Number,data$Torque)


## Table data transfer 
## Criteria - Tightening Result Angle value by Engine Number

table(data$Engine_Number,data$Angle)
xtabs(~data$Engine_Number + data$Angle)
Angle_data_by_Engine_Number <- table(data$Engine_Number,data$Angle)


## Table data transfer 
## Criteria - Tightening Result Angle value by Engine Number

table(data$Screw_Number,data$Angle)
xtabs(~data$Screw_Number + data$Angle)
Angle_data_by_Screw_Number <- table(data$Screw_Number,data$Angle)


## Table data transfer 
## Criteria - Tightening Duration (Time) by Engine Number

table(data$Engine_Number,data$Time)
xtabs(~data$Engine_Number + data$Time)
Time_data_by_Engine_Number <- table(data$Engine_Number,data$Time)


## Table data transfer 
## Criteria - Tightening Duration (Time) by Screw Number

table(data$Screw_Number,data$Time)
xtabs(~data$Screw_Number + data$Time)
Time_data_by_Screw_Number <- table(data$Screw_Number,data$Time)


## Table data transfer 
## Criteria - Tightening Angle by Tightening Torque

table(data$Torque,data$Angle)
xtabs(~data$Torque + data$Angle)
Angle_data_by_Torque <- table(data$Torque,data$Angle)


## Table data transfer 
## Criteria - Tightening Angle by Tightening Time

table(data$Time,data$Angle)
xtabs(~data$Time + data$Angle)
Angle_data_by_Time <- table(data$Time,data$Angle)


## Table data transfer 
## Criteria - Tightening Time by Tightening Torque

table(data$Time,data$Torque)
xtabs(~data$Time + data$Torque)
Torque_data_by_Time <- table(data$Time,data$Torque)


### check for the data

## mean
mean(data_Torque)
mean(data_Angle)
mean(data_Time)

## variance
var(data_Torque)
var(data_Angle)
var(data_Time)

## standard deviation
sd(data_Torque)
sd(data_Angle)
sd(data_Time)


### Plot check

## Histogram (Torque)
hist(data_Torque)

## Cumulative normal distribution (Torque)
pnorm(data_Torque)

## Plot Cumulative normal distribution (Torque)
plot(pnorm(data_Torque))

## Probablity density distribution (Torque)
dnorm(data_Torque)

## Plot Probablity density distribution (Torque)
plot(dnorm(data_Torque))

## Plot density of cumulative normal distriution (Torque)
plot(density(pnorm(data_Torque)))


## Histogram (Angle)
hist(data_Angle)

## Cumulative normal distribution (Angle)
pnorm(data_Angle)

## Plot Cumulative normal distribution (Angle)
plot(pnorm(data_Angle))

## Probablity density distribution (Angle)
dnorm(data_Angle)

## Plot Probablity density distribution (Angle)
plot(dnorm(data_Angle))

## Plot density of cumulative normal distriution (Angle)
plot(density(pnorm(data_Angle)))


## Histogram (Time)
hist(data_Time)

## Cumulative normal distribution (Time)
pnorm(data_Time)

## Plot Cumulative normal distribution (Time)
plot(pnorm(data_Time))

## Probablity density distribution (Time)
dnorm(data_Time)

## Plot Probablity density distribution (Time)
plot(dnorm(data_Time))

## Plot density of cumulative normal distriution (Time)
plot(density(pnorm(data_Time)))




### Hypothesis testing

## Hypothesis testing (Torque and Engine Number)
chisq.test(Torque_data_by_Engine_Number)

## Hypothesis testing (Torque and Axis Screw Number)
chisq.test(Torque_data_by_Screw_Number)

## Hypothesis testing (Angle and Axis Screw Number)
 chisq.test(Angle_data_by_Screw_Number)
 


