#Analysis of the relationship between the size of domestic knowledge pools and internationality in innovation networks as measured by the country dispersion index

#load("./datasource/TPF/disp_inv.RData")


#How to measure the size of knowledge pools? Number of patent applications. Using inventor data to determine the origin of patents makes sense,
#because inventors actually reside in a country, whereas applicant organizations may have access to knowledge pools from different countries

load("./datasource/TPF/ann_counts.RData")

#others function for all years. The year parameter determines for which year the limit value is checked (e.g. year = 2013, limit = 800, var = 'app' means that
#all countries with less than 800 in the count_app variable in 2013 are subsumed in the others category)
others3 <- function (data, year, limit, var = 'app') {
  #determine which countries should be subsumed in others, using var to determine whether inv or app counts should be compared to the limit
  if (var == 'inv') {del <- data[data$year == year & data$count_inv < limit, "country"]}
  else {del <- data[data$year == year & data$count_app < limit, "country"]}
  
  #initialize data.frame that holds annual values for others
  others3 <- data.frame(unique(data$year), "others", 0 , 0)
  names(others3) <- c("year", "country", "count_app", "count_inv")
  #calculate the values for each year
  others3$count_app <- sapply(others3$year, function(x) {sum(data[data$country %in% del & data$year == x, ]$count_app)})
  others3$count_inv <- sapply(others3$year, function(x) {sum(data[data$country %in% del & data$year == x, ]$count_inv)})
  #bind others data.frame and originial data
  others3 <- rbind(data, others3)
  #delete countries subsumed in others
  others3 <- others3[- which(others3$country %in% del), ]
}

#exclude countries that have less than x patent applications in 2013 to obtain a set of countries with a notable innovation output (inventor data is used here)
dispdata <- others3(ann_counts, 2013, 20, 'inv')
#drop the countries subsumed in others and the international category (because single countries shall be the observation instances)
#and restrict the time period to 1980 - 2016
dispdata <- dispdata[!(dispdata$country %in% c("others", "INT")) & dispdata$year %in% 1980:2016, ]

#add dispersion columns to dispdata.
load("./datasource/TPF/disp_inv.RData")
load("./datasource/TPF/disp_app.RData")
dispdata$invd <- mapply(function(ctry, yr) {disp_inv[disp_inv$country == ctry & disp_inv$year == yr, "dispersion"]},
                        ctry = dispdata$country, yr = dispdata$year)
dispdata$appd <- mapply(function(ctry, yr) {disp_app[disp_app$country == ctry & disp_app$year == yr, "dispersion"]},
                        ctry = dispdata$country, yr = dispdata$year)
  


##ggplot
library(ggplot2)
library(ggrepel)
attach(dispdata)
#histogram of patent counts is strongly right-skewed
qplot(count_inv[year == 2013], geom = "histogram", binwidth = 500, col = I("black"))
#the logarithmic transformation of counts solves the problem to some extent
qplot(log(count_inv[year == 2013]), geom = "histogram", binwidth = 1, col = I("black"))


ggplot(data = dispdata[year %in% seq(1985, 2015, 6), ], aes(x = log(count_inv), y = invd)) +
  geom_point() +
  geom_smooth(method = 'lm',formula = y~x) +
  facet_wrap(~ year, ncol = 2)

#2013 case in detail
ggplot(data = dispdata[year == 2013, ], aes(x = log(count_inv), y = invd, label = country)) +
  geom_point() +
  geom_smooth(method = 'lm',formula = y~x) +
  geom_text_repel()

#simple regression result
model <- lm(invd[year == 2013] ~ log(count_inv[year == 2013]))
summary(model)
#some checks
dispres <- resid(model)
qqnorm(resid(model))
qqline(resid(model))

names(dispres) <- country[year == 2013]

#the above seems to work. now calculate the all the residuals for the years 2003 to 2013, and average them to find countries' tendency to internationality, while controlling
#for the influence of knowledge pool size
#meanres <- data.frame(country = NA, value = NA)
allres <- sapply(2003:2013, function (x) {
  model <- lm(invd[year == x] ~ log(count_inv[year == x]))
  res <- resid(model)
  names(res) <- country[year == x]
  return(res)
})

meanres <- apply(allres, 1, mean)
meanres[order(meanres)]




#applicant data? --> don't use
ggplot(data = dispdata[year %in% seq(1985, 2015, 6), ], aes(x = log(count_app), y = appd)) +
  geom_point() +
  geom_smooth(method = 'lm',formula = y~x) +
  facet_wrap(~ year, ncol = 2)

detach(dispdata)
