#*********************************************************************************************************
#
# Chapter 03 - Algorithms
#
#*********************************************************************************************************

# read from XLS file for Manhattan and do a EDA
require(gdata)
manhattan.sales <- read.xls("rollingsales_manhattan.xls", pattern="BOROUGH")
head(manhattan.sales)
summary(manhattan.sales)

# cleans the data
manhattan.sales$sale.price.n <- as.numeric(gsub("[^[:digit:]]", "", manhattan.sales$SALE.PRICE))
a <- count(is.na(manhattan.sales$SALE.PRICE.N))
names(manhattan.sales) < tolower(names(manhattan.sales))

# clean/format the data with regular expressions
manhattan.sales$gross.sqft <- as.numeric(gsub("[^[:digit:]]", "", manhattan.sales$GROSS.SQUARE.FEET))
manhattan.sales$land.sqft <- as.numeric(gsub("[^[:digit:]]", "", manhattan.sales$LAND.SQUARE.FEET))
manhattan.sales$sale.date <- as.Date(manhattan.sales$SALE.DATE)
manhattan.sales$year.built <- as.numeric(as.character(manhattan.sales$YEAR.BUILT))

# do some exploration to make sure there's not anything weird going on with sales price
attach(manhattan.sales)
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(gross.sqft[sale.price.n==0])
detach(manhattan.sales)

# keep only the actual sales
manhattan.real.sales <- manhattan.sales[manhattan.sales$sale.price.n != 0,]

plot(manhattan.real.sales$gross.sqft, manhattan.real.sales$sale.price.n)
plot(log(manhattan.real.sales$gross.sqft), log(manhattan.real.sales$sale.price.n))

# for now, let's look at 1-, 2- and 3-family home
manhattan.home <- manhattan.real.sales[which(grepl("FAMILY", 
                                                   manhattan.real.sales$BUILDING.CLASS.CATEGORY)), ]
plot(log(manhattan.home$gross.sqft), log(manhattan.home$sale.price.n))

manhattan.home[which(manhattan.home$sale.price.n < 100000), ][order(manhattan.home[which(manhattan.home$sale.price.n < 1000000), ]$sale.price.n),]

# remove outliers that seem like they weren't actual sales
manhattan.home$outliers <- (log(manhattan.home$sale.price.n) <= 5) + 0
manhattan.home <- manhattan.home[which(manhattan.home$outliers == 0), ]
plot(log(manhattan.home$gross.sqft), log(manhattan.home$sale.price.n))
