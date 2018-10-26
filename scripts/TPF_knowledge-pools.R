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

ann_counts_big <- others3(ann_counts, 2013, 200, 'app')


