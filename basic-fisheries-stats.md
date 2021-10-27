basic fisheries stats
================

library(‘tidyverse’) library(‘lubridate’) library(‘reshape2’)
library(‘ggplot2’)

#CALCULATING ANNUAL LANDINGS

# Start with the landings data frame

annual_landings \<- landings_data %>% # Add colomn for kilograms by
dividing gram column by 1000 mutate(Weight_kg = Weight_g / 1000) %>% #
Group the data by year group_by(Year) %>% # Next, summarize the total
annual landings per year summarize(Annual_Landings_kg =
sum(Weight_kg,na.rm=TRUE))

## Display a table of the annual landings data

annual_landings

# Start with the landings data frame

annual_gear_landings \<- landings_data %>% # Add column for kilograms by
dividing gram column by 1000 mutate(Weight_kg = Weight_g / 1000) %>% #
Group the data by year and gear type group_by(Year,Gear) %>% # Next,
summarize the total annual landings per year and gear type
summarize(Annual_Landings_kg = sum(Weight_kg,na.rm=TRUE))

## Display a table of the annual landings data by gear type

annual_gear_landings

#MEDIAN CPUE # Start with the landings data frame cpue_median \<-
landings_data %>% # Add colomn for kilograms by dividing gram column by
1000 mutate(Weight_kg = Weight_g / 1000) %>% # Group by year and Trip ID
so that you can calculate CPUE for every trip in every year
group_by(Year,Trip_ID) %>% # For each year and trip ID, calculate the
CPUE for each trip by dividing the sum of the catch, converted from
grams to kilograms, by the trip by the number of fishing hours
summarize(Trip_CPUE = sum(Weight_kg) / mean(Effort_Hours)) %>% # Next,
just group by year so we can calculate median CPUE for each year across
all trips in the year group_by(Year) %>% # Calculate median CPUE for
each year summarize(Median_CPUE_kg_hour = median(Trip_CPUE))

# Display a table of the CPUE data

cpue_median

#save median data write_csv(cpue_median,“cpue_median.csv”)

#PERCENTAGE MATURE # Define m95, the length at which 95% of fish are
mature m95 = 15.9

# Start with the landings data frame

landings_data %>% # Add a column to the data that indicates whether each
length measurement is from a mature or immature fish. If it’s mature,
this value should be TRUE; if immature, FALSE. mutate(Mature = Length_cm
\> m95) %>% # Group by year so we can see the percent mature for every
year group_by(Year) %>% # The percentage mature is equal to the number
of mature fish divided by the total number of fish and multiplied by 100
summarize(Percent_Mature = sum(Mature) / n() \* 100)

#MEAN CPUE # Start with the landings data frame cpue_mean \<-
landings_data %>% # Add colomn for kilograms by dividing gram column by
1000 mutate(Weight_kg = Weight_g / 1000) %>% # Group by year and Trip ID
so that you can calculate CPUE for every trip in every year
group_by(Year,Trip_ID) %>% # For each year and trip ID, calculate the
CPUE for each trip by dividing the sum of the catch, converted from
grams to kilograms, by the trip by the number of fishing hours
summarize(Trip_CPUE = sum(Weight_kg) / mean(Effort_Hours)) %>% # Next,
just group by year so we can calculate mean CPUE for each year across
all trips in the year group_by(Year) %>% # Calculate mean CPUE for each
year summarize(Mean_CPUE_kg_hour = mean(Trip_CPUE))

# Display a table of the CPUE data

cpue_mean

#save mean data write_csv(cpue_mean,“cpue_mean.csv”)

#PLOT CPUE MEAN
plot(cpue_mean*Y**e**a**r*, *c**p**u**e*<sub>*m*</sub>*e**a**n*Mean_CPUE_kg_hour,
xlab = “Year”, ylab= “Mean CPUE”)

#PLOT CPUE MEDIAN plot(cpue_median$Year, cpue\_$Median_CPUE_kg_hour,
xlab= “Year”, ylab= “CPUE Median”)

#combine mean and median data mean_median \<-
merge(cpue_mean,cpue_median, by=“Year”) #save merge
write_csv(mean_median,“cpue_mean_median.csv”)

#comparison plot mm \<- melt(mean_median, id=“Year”) m\<-
ggplot(data=mm, aes(x= Year , y= value, colour = variable))+
geom_point() + xlim(2002, 2015)

write_png(m, “mean_vs_median_cpue.png)

jpeg(file=“mean_vs_median_cpue.jpeg”)

## =======

title: “basic fisheries stats” output: github_document —

library(‘tidyverse’) library(‘lubridate’) library(‘reshape2’)
library(‘ggplot2’)

#CALCULATING ANNUAL LANDINGS

# Start with the landings data frame

annual_landings \<- landings_data %>% # Add colomn for kilograms by
dividing gram column by 1000 mutate(Weight_kg = Weight_g / 1000) %>% #
Group the data by year group_by(Year) %>% # Next, summarize the total
annual landings per year summarize(Annual_Landings_kg =
sum(Weight_kg,na.rm=TRUE))

## Display a table of the annual landings data

annual_landings

# Start with the landings data frame

annual_gear_landings \<- landings_data %>% # Add column for kilograms by
dividing gram column by 1000 mutate(Weight_kg = Weight_g / 1000) %>% #
Group the data by year and gear type group_by(Year,Gear) %>% # Next,
summarize the total annual landings per year and gear type
summarize(Annual_Landings_kg = sum(Weight_kg,na.rm=TRUE))

## Display a table of the annual landings data by gear type

annual_gear_landings

#MEDIAN CPUE # Start with the landings data frame cpue_median \<-
landings_data %>% # Add colomn for kilograms by dividing gram column by
1000 mutate(Weight_kg = Weight_g / 1000) %>% # Group by year and Trip ID
so that you can calculate CPUE for every trip in every year
group_by(Year,Trip_ID) %>% # For each year and trip ID, calculate the
CPUE for each trip by dividing the sum of the catch, converted from
grams to kilograms, by the trip by the number of fishing hours
summarize(Trip_CPUE = sum(Weight_kg) / mean(Effort_Hours)) %>% # Next,
just group by year so we can calculate median CPUE for each year across
all trips in the year group_by(Year) %>% # Calculate median CPUE for
each year summarize(Median_CPUE_kg_hour = median(Trip_CPUE))

# Display a table of the CPUE data

cpue_median

#save median data write_csv(cpue_median,“cpue_median.csv”)

#PERCENTAGE MATURE # Define m95, the length at which 95% of fish are
mature m95 = 15.9

# Start with the landings data frame

landings_data %>% # Add a column to the data that indicates whether each
length measurement is from a mature or immature fish. If it’s mature,
this value should be TRUE; if immature, FALSE. mutate(Mature = Length_cm
\> m95) %>% # Group by year so we can see the percent mature for every
year group_by(Year) %>% # The percentage mature is equal to the number
of mature fish divided by the total number of fish and multiplied by 100
summarize(Percent_Mature = sum(Mature) / n() \* 100)

#MEAN CPUE # Start with the landings data frame cpue_mean \<-
landings_data %>% # Add colomn for kilograms by dividing gram column by
1000 mutate(Weight_kg = Weight_g / 1000) %>% # Group by year and Trip ID
so that you can calculate CPUE for every trip in every year
group_by(Year,Trip_ID) %>% # For each year and trip ID, calculate the
CPUE for each trip by dividing the sum of the catch, converted from
grams to kilograms, by the trip by the number of fishing hours
summarize(Trip_CPUE = sum(Weight_kg) / mean(Effort_Hours)) %>% # Next,
just group by year so we can calculate mean CPUE for each year across
all trips in the year group_by(Year) %>% # Calculate mean CPUE for each
year summarize(Mean_CPUE_kg_hour = mean(Trip_CPUE))

# Display a table of the CPUE data

cpue_mean

#save mean data write_csv(cpue_mean,“cpue_mean.csv”)

#PLOT CPUE MEAN
plot(cpue_mean*Y**e**a**r*, *c**p**u**e*<sub>*m*</sub>*e**a**n*Mean_CPUE_kg_hour,
xlab = “Year”, ylab= “Mean CPUE”)

#PLOT CPUE MEDIAN plot(cpue_median$Year, cpue\_$Median_CPUE_kg_hour,
xlab= “Year”, ylab= “CPUE Median”)

#combine mean and median data mean_median \<-
merge(cpue_mean,cpue_median, by=“Year”) #save merge
write_csv(mean_median,“cpue_mean_median.csv”)

#comparison plot mm \<- melt(mean_median, id=“Year”) ggplot(data=mm,
aes(x= Year , y= value, colour = variable))+ geom_point() + xlim(2002,
2015)
