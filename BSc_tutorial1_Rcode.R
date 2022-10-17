#Load the Data

R_MetData_BSc <- read.csv("~/Desktop/R_MetData_BSc.csv")
View(R_MetData_BSc)

# Install some packages to get some spefici code we will use
# ggplot2 and dplyr; you can do this in the "Packages" tab to the right
# or use this command
install.packages ("ggplot2")
installed.packages("dplyr")

# Data summary of Precip: N (counts), mean, standard deviation, min, max
table(R_MetData_BSc['Site']) # N
tapply(R_MetData_BSc$Precip, R_MetData_BSc$Site,mean) #means
tapply(R_MetData_BSc$Precip, R_MetData_BSc$Site,sd) #standard deviations
tapply(R_MetData_BSc$Precip, R_MetData_BSc$Site,min) #min 
tapply(R_MetData_BSc$Precip, R_MetData_BSc$Site,max) #max

# Notice that you get to data via this phrase "filename$paramater_name
# How would you extract data from Tmax?

table(R_MetData_BSc['Site']) # N
tapply(R_MetData_BSc$Tmax, R_MetData_BSc$Site,mean) #means
tapply(R_MetData_BSc$Tmax, R_MetData_BSc$Site,sd) #standard deviations
tapply(R_MetData_BSc$Tmax, R_MetData_BSc$Site,min) #min 
tapply(R_MetData_BSc$Tmax, R_MetData_BSc$Site,max) #max

# If you need a command explained use Help or this code
help("tapply")

#Histogram of Precip
library(ggplot2)

rainhist<-ggplot(R_MetData_BSc, aes(x = Precip)) +
  geom_histogram(aes(color = Site, fill = Site), 
                 position = "identity", bins = 15, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

rainhist + labs(title= "Mean Annual Monthly Precip")

#Bar graphs of Temperature
library(dplyr)
Temps<-R_MetData_BSc %>%
  group_by(Site) %>%
  summarize (Max = mean(Tmax), Maxsd =sd(Tmax), Min = mean(Tmin), Minsd = sd(Tmin))

# What are these stange symbols (%>%)?
# Check this page (https://www.datacamp.com/tutorial/pipe-r-tutorial)
#  "%>% are called pipes and this operator essentially means "and then", 
# so you can chain multiple actions together without having to store 
# intermediate values as separate variables. 
# The output from one action is used as the input for the next action.
# In the code above we are creating a variable called "Temps" which is 
# our original data "and then" we are grouping the data by Sites
# "and then" we are getting the summary data of Tmax and Tmin (means and standard deviations)

View(Temps)

# To rearage the data we need to make a dataframe from the summary.
# With a small amount of data, we can manually enter it

bardata<-data.frame ("Site"=c("Oxford","Oxford","Tiree","Tiree"), "param" = c("Tmax", "Tmin", "Tmax", "Tmin"), "avg" = c(14.44,6.59, 11.67, 6.79), "sd" = c(7.25,4.26, 3.35, 3.18))

View(bardata)

# Now we can make the bar plot

barplot<-ggplot(bardata, aes(x=Site, y=avg, fill=param)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(1))
# View it 
barplot

# Make it pretty
barplot + labs(title = "Max & Min Temps", x = "Site", y = "Temp (C)") + 
  scale_fill_manual(values=c("#619CFF","#F8766D"))

#Now let's do the time series graph

library(dplyr)

# Precip by year

syp<- R_MetData_BSc %>%
  group_by(Site,year) %>%
  summarise(avg=mean(Precip))

View(syp)

rainyear<-ggplot(syp, aes(x = year, y = avg)) + 
  geom_line(aes(color = Site, linetype = Site)) + 
  scale_color_manual(values = c("darkred", "steelblue"))

rainyear + labs(title = "Mean Annual Monthly Precip", x = "year", y = "precip (mm)")



