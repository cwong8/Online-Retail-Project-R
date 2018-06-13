# http://www.statmethods.net/input/dates.html
# http://stackoverflow.com/questions/19327020/split-subset-a-data-frame-by-factors-in-one-column
# https://www.timeanddate.com/calendar/seasons.html?year=2000
# http://stackoverflow.com/questions/16566799/change-variable-name-in-for-loop-using-r
# http://stackoverflow.com/questions/21218438/how-to-sort-list-by-first-element-in-r
# http://stackoverflow.com/questions/1828742/rotating-axis-labels-in-r
# http://stackoverflow.com/questions/4128526/how-can-i-make-my-vertical-labels-fit-within-my-plotting-window
# http://stackoverflow.com/questions/11509267/how-to-make-the-row-names-and-column-names-into-factor-or-one-of-the-fields-in-r
# https://www.r-bloggers.com/r-beginners-plotting-locations-on-to-a-world-map/



setwd("C:/Users/Christopher/Desktop/STA 141A/Project")
library(data.table)
# save.image("data.Rdata")
# load("data.Rdata")


# http://archive.ics.uci.edu/ml/datasets/Online+Retail
# This is a transnational data set which contains all the transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store online retail.The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers.





# Ggmap to visualize the countries and plot on a world map.
# Ggplot2 to visualize the data
# Stringr and other string manipulation functions to work with invoice dates
# Dplyr and plyr to make manipulating the data easier
# Linear Regression and Analysis of Variance


online.retail <- as.data.frame(fread("Online Retail.csv"))

attach(online.retail)
summary(Quantity)


################################################################################
# Does the popularity of products change with respect to time (season, month, etc.)? 

library(dplyr)
library(stringr)

### Seasons
## Equinox/Solstice times
# 2010: Fall: Sep 23, 2010  Winter: Dec 21, 2010
# 2011: Spring:	Mar 20, 2011 Summer:	Jun 21, 2011	Fall:	Sep 23, 2011  Winter: Dec 21, 2011
Date2 <- str_extract(InvoiceDate, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}") 
Date2 <- as.Date(Date2, "%m/%d/%Y")

f2010 <- seq(as.Date("2010-9-23"), as.Date("2010-12-20"), "day")
w2010 <- seq(as.Date("2010-12-21"), as.Date("2011-3-19"), "day")
s2011 <- seq(as.Date("2011-3-20"), as.Date("2011-6-20"), "day")
su2011 <- seq(as.Date("2011-6-21"), as.Date("2011-9-22"), "day")
f2011 <- seq(as.Date("2011-9-23"), as.Date("2011-12-20"), "day")

# Data split by seasons (data frames)
library(dplyr)
online.retail.f2010 <- filter(online.retail, Date2 %in% f2010)
online.retail.w2010 <- filter(online.retail, Date2 %in% w2010)
online.retail.s2011 <- filter(online.retail, Date2 %in% s2011)
online.retail.su2011 <- filter(online.retail, Date2 %in% su2011)
online.retail.f2011 <- filter(online.retail, Date2 %in% f2011)

# List of 5 data frames, each with a season name
online.retail.seasons <- list("Fall 2010" = online.retail.f2010, 
                              "Winter 2011" = online.retail.w2010,
                              "Spring 2011" = online.retail.s2011, 
                              "Summer 2011" = online.retail.su2011,
                              "Fall 2011" = online.retail.f2011)

# Separating quantities by product and season
quantities.seasons <- lapply(1:length(online.retail.seasons), function(i){
  a <- online.retail.seasons[[i]]
  # Summing over all quantities to get the total instead of single quantity
  # observations using an lapply.
  quantities <- lapply(split(a$Quantity, a$StockCode), sum)
  return (quantities)
})
names(quantities.seasons) <- names(online.retail.seasons)
# Calls can be made by quantities$'(Season Year)'$(StockCode)

# Making sure we only compare the same items sold throughout the year
# There is no way around a for loop here since this is a recursive process
same.products.seasons <- intersect(names(quantities.seasons[[1]][which(quantities.seasons[[1]] > 0)]), names(quantities.seasons[[2]][which(quantities.seasons[[2]] > 0)]))
for (i in 3:(length(quantities.seasons))) {
  same.products.seasons <- intersect(same.products.seasons, names(quantities.seasons[[i]][which(quantities.seasons[[i]] > 0)]))
}
# There are 1933 products that net at least 1 sale every season

# Sorting the quantities sold
quantities.sorted.seasons <- lapply(1:length(quantities.seasons), function(i){
  L <- quantities.seasons[[i]][same.products.seasons]
  sorted <- L[order(sapply(L, function(x) x[1], simplify=TRUE), decreasing=TRUE)]
  return(sorted)
})
names(quantities.sorted.seasons) <- names(online.retail.seasons)

top.quantities.sorted.seasons <- lapply(quantities.sorted.seasons, function(x) head(x, 3))

# All top 3 items sold per season over the entire year
top.products.seasons <- union(names(top.quantities.sorted.seasons[[1]]), names(top.quantities.sorted.seasons[[2]]))
for (i in 3:(length(quantities.seasons))) {
  top.products.seasons <- union(top.products.seasons, names(top.quantities.sorted.seasons[[i]]))
}

# Finding the quantity sold of each of these items per season
quantities.sold.seasons <- lapply(1:length(quantities.seasons), function(i){
  a <- quantities.seasons[[i]][top.products.seasons]
  return(a)
})
names(quantities.sold.seasons) <- names(online.retail.seasons)

# Finding the item descriptions from StockCode
items.seasons <- sapply(1:length(top.products.seasons), function(i) {head(filter(online.retail, StockCode == top.products.seasons[i]), 1)$Description})

# Plotting all of this information
item.seasons <- lapply(1:length(items.seasons), function(i){
  d <- sapply(quantities.sold.seasons, '[[', i)
  return(d)
})
names(item.seasons) <- items.seasons

d.seasons <- data.frame(item.seasons)


dev.new()
par(mfrow = c(2, 4), mar = c(6, 4, 4, 2) + 0.1)
PlotQuantitiesSeasons <- function(d.seasons){
  for (i in 1:ncol(d.seasons)) {
    barplot(d.seasons[, i], col = rainbow(length(rownames(d.seasons))), names.arg = rownames(d.seasons), las = 2,
            main = items.seasons[i])
  }
}
PlotQuantitiesSeasons(d.seasons)
# All of these items were at one point the 3 most popular items in a month
# Note we do not have full data for Fall 2010!!! Only about 3 weeks worth of data.
# Also, Fall 2011 is missing 11 days of data!!!

# Reset plotting parameters
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)



### Month
# Get the month, day, year
Date <- str_extract(InvoiceDate, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}")
# Reformat the dates
Date <- as.Date(Date, "%m/%d/%Y")
Date <- format(Date, format = "%B %d, %Y")

# Getting the month and year
mon <- str_extract(Date, "[A-Z][a-z]+")
year <- str_extract(Date, "(?<=\\,\\s)\\d{4}")

# Adding factors containing month and year to the data
MonthYear <- as.factor(str_c(mon, year, sep = " "))
online.retail$MonthYear <- MonthYear

# Data split by month and year (big list)
online.retail.monthyear <- split(online.retail, MonthYear)

# Separating quantities by product and month/year
quantities <- lapply(1:length(online.retail.monthyear), function(i){
  a <- online.retail.monthyear[[i]]
  # Summing over all quantities to get the total instead of single quantity
  # observations using an lapply.
  quantities <- lapply(split(a$Quantity, a$StockCode), sum)
  return (quantities)
})
names(quantities) <- names(online.retail.monthyear)
# Calls can be made by quantities$'(Month Year)'$(StockCode)

# Making sure we only compare the same items sold throughout the year
# There is no way around a for loop here since this is a recursive process
same.products.monthyear <- intersect(names(quantities[[1]][which(quantities[[1]] > 0)]), names(quantities[[2]][which(quantities[[2]] > 0)]))
for (i in 3:(length(quantities))) {
  same.products.monthyear <- intersect(same.products.monthyear, names(quantities[[i]][which(quantities[[i]] > 0)]))
}
# There are 1116 products that net at least 1 sale and was sold every month for the year

# Sorting the quantities sold
quantities.sorted <- lapply(1:length(quantities), function(i){
  L <- quantities[[i]][same.products.monthyear]
  sorted <- L[order(sapply(L, function(x) x[1], simplify=TRUE), decreasing=TRUE)]
  return(sorted)
})
names(quantities.sorted) <- names(online.retail.monthyear)

top.quantities.sorted <- lapply(quantities.sorted, function(x) head(x, 3))

# All top 3 items sold per month over the entire year
top.products.monthyear <- union(names(top.quantities.sorted[[1]]), names(top.quantities.sorted[[2]]))
for (i in 3:(length(quantities))) {
  top.products.monthyear <- union(top.products.monthyear, names(top.quantities.sorted[[i]]))
}

# Finding the quantity sold of each of these items per month
quantities.sold.monthyear <- lapply(1:length(quantities), function(i){
  a <- quantities[[i]][top.products.monthyear]
  return(a)
})
names(quantities.sold.monthyear) <- names(online.retail.monthyear)

# Reordering the months into chronological order
month.order <- c(3,6,5,9,1,10,8,7,2,13,12,11,4)
quantities.sold.monthyear <- quantities.sold.monthyear[month.order]

# Finding the item descriptions from StockCode
items <- sapply(1:length(top.products.monthyear), function(i) {head(filter(online.retail, StockCode == top.products.monthyear[i]), 1)$Description})

# Plotting all of this information
item.monthyear <- lapply(1:length(items), function(i){
  d <- sapply(quantities.sold.monthyear, '[[', i)
  return(d)
})
names(item.monthyear) <- items

d <- data.frame(item.monthyear)


dev.new()
par(mfrow = c(4, 4), mar = c(6, 4, 4, 2) + 0.1)
PlotQuantities <- function(d){
  for (i in 1:ncol(d)) {
    barplot(d[, i], col = rainbow(length(rownames(d))), names.arg = rownames(d), las = 2,
            main = items[i])
  }
}
PlotQuantities(d)
# All of these items were at one point the 3 most popular items in a month
# Note we do not have full data for December 2011!!! Only 9 days worth of data.

# Reset plotting parameters
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)



################################################################################
# Which products are the most profitable for this company? What about least profitable?

# Split the data frame by products
# Note the abnormalities in Description including 'amazon' and 'refund', so instead
# we use the StockCode to identify products
online.retail.products <- split(online.retail, StockCode)

# Total sales from each item. Note that some of them are adjusting bad debt and
# Amazon fees, bank charges, discounts, etc.
total.profit <- sapply(1:length(online.retail.products), function(i){
  total.profit <- sum(online.retail.products[[i]]$Quantity * online.retail.products[[i]]$UnitPrice)
  return (total.profit)
})

# Which products made more than $50,000
max.profit.ind <- which(total.profit > 50000)

max.profit.items <- sapply(1:length(max.profit.ind), function(i){
  # Get the product descriptions
  products <- sapply(online.retail.products[max.profit.ind], '[', 3)[[i]][1]
  # Return only one of them
  return (products)
})
# Corresponding item and profit
max.profit.items.df <- data.frame(Item = max.profit.items,
                                  Profit = total.profit[max.profit.ind])

# Which profits made less than $1 (but more than $0)?...
min.profit.ind <- which(total.profit < 1 & total.profit > 0)

min.profit.items <- sapply(1:length(min.profit.ind), function(i){
  # Get the product descriptions
  products <- sapply(online.retail.products[min.profit.ind], '[', 3)[[i]][1]
  # Return only one of them
  return (products)
})
min.profit.ind[which(min.profit.items == "")] <- NA
min.profit.ind <- min.profit.ind[!is.na(min.profit.ind)]
min.profit.items[which(min.profit.items == "")] <- NA 
min.profit.items <- min.profit.items[!is.na(min.profit.items)]
# Corresponding item and profit
min.profit.items.df <- data.frame(Item = min.profit.items,
                                  Profit = total.profit[min.profit.ind])

# Which profits made no money ($0)?
zero.profit.ind <- which(total.profit == 0)

zero.profit.items <- sapply(1:length(zero.profit.ind), function(i){
  # Get the product descriptions
  products <- sapply(online.retail.products[zero.profit.ind], '[', 3)[[i]][1]
  # Return only one of them
  return (products)
})
zero.profit.items[which(zero.profit.items == "")] <- NA 
zero.profit.items <- zero.profit.items[!is.na(zero.profit.items)]
zero.profit.items <- unique(zero.profit.items)
zero.profit.items <- str_extract(zero.profit.items, "[A-Z]{2,}[A-Z\\,\\s\\-]+")
zero.profit.items <- zero.profit.items[!is.na(zero.profit.items)]
# Corresponding item and profit
zero.profit.items.df <- data.frame(Item = zero.profit.items,
                                   Profit = 0)

# Which item generated the most profit/sales/income (that is not postage)?
best.profit.ind <- which(total.profit == 164762.19)

best.profit.item <- sapply(1:length(best.profit.ind), function(i){
  # Get the product descriptions
  products <- sapply(online.retail.products[best.profit.ind], '[', 3)[[i]][1]
  # Return only one of them
  return (products)
})

################################################################################
# Which products are the most popular in certain countries? Is there a difference in price between countries? How does this affect sales?

# Data split by Country
online.retail.countries <- split(online.retail, Country)

# Separating quantities by product and country
quantities.countries <- lapply(1:length(online.retail.countries), function(i){
  a <- online.retail.countries[[i]]
  # Summing over all quantities to get the total instead of single quantity
  # observations using an lapply.
  quantities <- lapply(split(a$Quantity, a$StockCode), sum)
  return (quantities)
})
names(quantities.countries) <- names(online.retail.countries)

# Sorting the quantities sold
quantities.sorted.countries <- lapply(1:length(quantities.countries), function(i){
  L <- quantities.countries[[i]]
  sorted <- L[order(sapply(L, function(x) x[1], simplify=TRUE), decreasing=TRUE)]
  return(sorted)
})
names(quantities.sorted.countries) <- names(online.retail.countries)

# Finding the best seller within each country
top.quantities.sorted.countries <- lapply(quantities.sorted.countries, function(x) head(x, 1))

# Finding the item descriptions from StockCode
items.countries <- sapply(1:length(top.quantities.sorted.countries), function(i){
  a <- head(filter(online.retail, StockCode == names(top.quantities.sorted.countries[[i]])), 1)$Description
  return (a)
})

# Finding the average unit price of these items
items.price <- sapply(1:length(top.quantities.sorted.countries), function(i){
  a <- mean(filter(online.retail, StockCode == names(top.quantities.sorted.countries[[i]]))$UnitPrice)
  return (a)
})

# Putting everything together into a data frame
items.sold.countries <- data.frame(quantity = unlist(top.quantities.sorted.countries),
                                   item = items.countries,
                                   avg.price = items.price)
rownames(items.sold.countries) <- names(quantities.sorted.countries)

# Finding the duplicate items so we can compare them between countries
length(which(table(items.sold.countries$item) > 1))
duplicate.items <- unique(items.sold.countries[duplicated(items.sold.countries$item), ]$item)

items.sold.countries.dup <- lapply(1:length(duplicate.items), function(i){
  a <- subset(items.sold.countries, item == duplicate.items[i])
  return (a)
})
names(items.sold.countries.dup) <- duplicate.items
# As it turns out, there is no difference in price between countries



# Plotting all of this information
library(ggmap)

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="white") # create a layer of borders
mp <- ggplot() +   mapWorld

# Get the country names
country.names <- unique(Country)
country.names[which(country.names == "Unspecified")] <- NA
country.names[which(country.names == "European Community")] <- NA
country.names <- as.character(na.omit(country.names))

# Get the country locations (lat and lon)
country.locations <- geocode(country.names)
country.locations[which(country.names == "Brazil"), ] <- geocode("Brasilia, Federal District, Brazil")
country.locations[which(country.names == "RSA"), ] <- geocode("South Africa")

# Quantity = size
# avg price = color

items.sold.countries.plot <- items.sold.countries[-c(33, 36), ]
mp + geom_point(data = items.sold.countries.plot, aes(x = country.locations$lon, y = country.locations$lat, col = avg.price, size = quantity))

################################################################################
# Model price as a time series across days

cakestand <- subset(online.retail, Description == best.profit.item)
cakestand$TimeFactor <- factor(str_extract(cakestand$InvoiceDate, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}"))
relevel(cakestand$TimeFactor, order(as.Date(cakestand$TimeFactor)))
total.time <- seq(as.Date("2010-12-1"), as.Date("2011-12-9"), "day")




### THIS IS LAM'S GARBAGE IDEA
# Model price as a linear function of country
model <- lm(UnitPrice ~ Country)

# Making sure we only compare the same items sold throughout all countries
# There is no way around a for loop here since this is a recursive process


same.products.countries <- intersect(names(quantities.countries[[1]][which(quantities.countries[[1]] > 0)]), names(quantities.countries[[2]][which(quantities.countries[[2]] > 0)]))
for (i in 3:(length(quantities.countries))) {
  same.products.countries <- intersect(same.products.countries, names(quantities.countries[[i]][which(quantities.countries[[i]] > 0)]))
}
# No products are sold to each country


# Most popular and profitable customers

