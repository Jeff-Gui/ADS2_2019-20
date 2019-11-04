library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)

data = read.csv("/Users/jefft/Library/Mobile Documents/com~apple~CloudDocs/ADS/Problem sets/GDP.csv", na.strings = "", header = T, check.names = F)
# remember to use na.strings = "" to define NA
# header = F, file contains the names of the variables as its fiest line
# check.names = T, check if names of variables are [syntactically] valid variabel names
# If check names = T as default, it will add "X" before the year
# data$Year = as.numeric(gsub("X","",data$Year))
data = gather(data, key = "Year", value = "GDP", "1960":"2018", factor_key = T, convert = T)
# "1960:2018" is the same as -CountryName, different ways to select columns
# convert = F, if true, will convert "Year" to factor
# factor_key = F, if true, will apply type.convert() to the key, use on numeric, integer and logical
data.noNA = drop_na(data)
anyNA(data.noNA)
mydt = dplyr::filter(data.noNA, CountryName == "China" | CountryName == "Japan" | CountryName == "India")
mydt = dplyr::filter(mydt, Year == 1960 | Year == 1970 | Year == 1980 | Year == 1990 | Year == 2000 | Year == 2010 | Year == 2018)
summary(mydt)
#============================
p = ggplot(mydt, aes(Year, GDP, color=CountryName)) +
  geom_point(shape = 19) +
  geom_line(aes(Year, GDP)) +
  geom_text(aes(label = GDP), hjust = 0.1, nudge_x = 2.5, size = 2.5) +
  labs(title = "GDP trands in the countries")
p

p1 = p +
  scale_y_continuous(trans = "log2") +
  facet_wrap(aes(group = CountryName))
p1 # with groups of country

p2 = p +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "loess") # ? loess
p2 # with areas of smooth shadow

p3 = ggplot(mydt, aes(Year, GDP, color=CountryName)) +
  geom_area(aes(fill = CountryName), position = "fill") 
p3
# should not plot points before, otherwise will overlap the area!!!

p4 = ggplot(mydt, aes(Year, GDP)) +
  scale_y_continuous(trans = "log10") +
  geom_bar(aes(fill = CountryName), stat = "identity", position = "dodge")
p4

#============ bonous ===========
library(maps)
asia_map <- map_data("world", region = c("China", "Japan", "India"))
ggplot(asia_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=region)) # fill: give each its color

gdp2018 = dplyr::filter(mydt, Year == 2018)
gdpdt = dplyr::left_join(gdp2018, asia_map, by = c("CountryName"="region"))

ggplot(gdpdt, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=GDP)) +
  scale_fill_gradient(low = "green", high = "red") + # should not use scale_color_gradient, must use scale_fill_gradient to fill in the area
  labs(title="2018 GDP in east asian countries")
