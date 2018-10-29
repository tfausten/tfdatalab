##This set computes two new data.frames (disp_app and disp_inv) which contain yearly (average) country dispersion
##values for each (applicant or inventor) country in the database. The data.frames are then saved to the /datasource
##directory for further analysis, e.g. in a post on internationality in innovation networks. The concept of country 
##dispersion comes from the scientific article ...

#calc.disp(countries) calculates the share of country entries in a single country vector and returns the squares 
#of each share (for unique entries) function to calculate country dispersion for a single character vector
calc.disp <- function (countries) {
  uni <- unique(countries)
  sharevec <- vector(mode = "numeric", length = length(uni)) 
  
  #sharevec contains the share of each unique country in the country vector passed to the function
  for (i in 1:length(sharevec)) {             #for-loop actually computes faster than lapply here
    sharevec[i] <- sum(countries == uni[i])/length(countries)
  }
  return(1 - sum(sharevec^2))
}

#load applicant and inventor country data
load("./datasource/TPF/allcountries_app.RData")
load("./datasource/TPF/allcountries_inv.RData")

#initialize dataframe to hold family_id and corresponding country dispersion index (seperately for applicant and 
#inventor countries)
dispersion <- data.frame(family_id = 1:length(allcountries_app), D_app = NA, D_inv = NA)

#use the function to calculate dispersion for all entries (and save to file for later use)
dispersion$D_app <- sapply(allcountries_app, calc.disp)
dispersion$D_inv <- sapply(allcountries_inv, calc.disp)
save(dispersion, file = "./datasource/TPF/dispersion.Rdata")

#allcountries were only useful for calculating country dispersion
rm(allcountries_app, allcountries_inv)

#how many dispersion entries are !=0 and not NaN (NaN values result from division by 0 if there is no country entry)
length(which(dispersion$D_inv != 0 & is.nan(dispersion$D_inv) == FALSE))
#how many are NaN?
length(which(is.nan(dispersion$D_inv)))

#load tpf_families for information on the year of data entries
load("./datasource/TPF/tpf_families.RData")

#all inventor and applicant countries are respected. Create country list variables to simplify code below
countries_app <- levels(tpf_families$country_app)
countries_inv <- levels(tpf_families$country_inv)

#data.frame for yearly country dispersion data. Applicant countries
disp_app <- data.frame(year = rep(1975:2017, length.out = (2017 - 1974) * length(countries_app)), 
                       country = rep(countries_app, each = (2017 - 1974)), dispersion = NA, count = NA, 
                       stringsAsFactors = FALSE)
#Inventor countries
disp_inv <- data.frame(year = rep(1975:2017, length.out = (2017 - 1974) * length(countries_inv)), 
                       country = rep(countries_inv, each = (2017 - 1974)), dispersion = NA, count = NA, 
                       stringsAsFactors = FALSE)

#for loop to fill the data.frame. Create workingvec of type character to avoid repeated conversion to character 
#vector in the loop through the grep function
load("./datasource/TPF/unicountries_inv.Rdata")
load("./datasource/TPF/unicountries_app.Rdata")
unicountries_inv <- as.character(unicountries_inv)
unicountries_app <- as.character(unicountries_app)

whichyears <- sapply(1975:2016, function(x) {which(tpf_families$first_app_year == x)})
names(whichyears) <- 1975:2016
whichcountries_app <- lapply(countries_app, function(x) {grep(x, unicountries_app, fixed = TRUE)})
names(whichcountries_app) <- countries_app
whichcountries_inv <- lapply(countries_inv, function(x) {grep(x, unicountries_inv, fixed = TRUE)})
names(whichcountries_inv) <- countries_inv

for (y in countries_app) {
  
  for (i in 1975:2017) {
    
    #applicant dispersion
    isect <- intersect('[['(whichyears, as.character(i)), '[['(whichcountries_app, y))  
    disp_app$dispersion[disp_app$year == i & disp_app$country == y] <- mean(dispersion$D_app[isect])
    disp_app$count[disp_app$year == i & disp_app$country == y] <- length(dispersion$D_app[isect])
  }
  cat(y, " ")      #monitor progress
}

for (y in countries_inv) {
  
  for (i in 1975:2017) {
    
    #inventor dispersion
    isect <- intersect('[['(whichyears, as.character(i)), '[['(whichcountries_inv, y))  
    disp_inv$dispersion[disp_inv$year == i & disp_inv$country == y] <- mean(dispersion$D_inv[isect])
    disp_inv$count[disp_inv$year == i & disp_inv$country == y] <- length(dispersion$D_inv[isect])
  }
  cat(y, " ")      #monitor progress
}

#save annual dispersion
save(disp_app, file = "./datasource/TPF/disp_app.Rdata")
save(disp_inv, file = "./datasource/TPF/disp_inv.Rdata")


rm(unicountries_app, unicountries_inv, whichcountries_app, whichcountries_inv, whichyears, y, i,
   countries_app, countries_inv, isect)

