#Calculate the country dispersion index for entries in the allcountries matrix (which contains country data about 
#applicants. the same could be done with inventor-country data)

#function to calculate the share of country entries in a single country vector and return the squares of each share 
#(for unique entries) function to calculate country dispersion for a single character vector

calc.disp <- function (x) {
  uni <- unique(x)
  sharevec <- vector(mode = "numeric", length = length(uni))
  
  for (i in 1:length(sharevec)) {             #for-loop actually computes faster than lapply here
    sharevec[i] <- sum(x == uni[i])/length(x)
  }
  return(1 - sum(sharevec^2))
}

#load applicant and inventor country data
load("./datasource/TPF/allcountries_app.RData")
load("./datasource/TPF/allcountries_inv.RData")

str(allcountries_app)

#initialize dataframe to hold family_id and corresponding country dispersion index

dispersion <- data.frame(family_id = 1:length(allcountries_app), D_app = NA, D_inv = NA)

#use the function to calculate dispersion for all entries (and save to file for later use)
dispersion$D_app <- sapply(allcountries_app, calc.disp)
dispersion$D_inv <- sapply(allcountries_inv, calc.disp)
save(dispersion, file = "./datasource/TPF/dispersion.Rdata")

#allcountries not needed anymore
rm(allcountries_app, allcountries_inv)

#how many dispersion entries are !=0 and not NaN (NaN values result from division by 0 if there is no country entry)
length(which(dispersion$D_inv != 0 & is.nan(dispersion$D_inv) == FALSE))
#how many are NaN?
length(which(is.nan(dispersion$D_inv)))

##############
#investigate averages for example countries
#investigate dispersion index of key countries over time

#vector of countries to investigate. and load families dataset for year data
countrygroup <- c("JP", "US", "DE", "CN", "KR", "FR", "CH", "GB")
load("./datasource/TPF/tpf_families.RData")
#data.frame for yearly country dispersion data
disp_data <- data.frame(year = rep(1975:2016, length.out = (2016 - 1974) * length(countrygroup)), 
                        country = rep(countrygroup, each = (2016 - 1974)), inv_disp = NA, app_disp = NA, 
                        inv_count = NA, app_count = NA, stringsAsFactors = FALSE)

#for loop to fill the data.frame. Create workingvec of type character to avoid repeated conversion to character 
#vector in the loop through the grep function
load("./datasource/TPF/unicountries_inv.Rdata")
load("./datasource/TPF/unicountries_app.Rdata")
unicountries_inv <- as.character(unicountries_inv)
unicountries_app <- as.character(unicountries_app)


whichyears <- sapply(1975:2016, function(x) {which(tpf_families$first_app_year == x)})
names(whichyears) <- 1975:2016
whichcountries_inv <- lapply(countrygroup, function(x) {grep(x, unicountries_inv, fixed = TRUE)})
names(whichcountries_inv) <- countrygroup
whichcountries_app <- lapply(countrygroup, function(x) {grep(x, unicountries_app, fixed = TRUE)})
names(whichcountries_app) <- countrygroup

for (y in countrygroup) {
  
  for (i in 1975:2016) {

    #inventor dispersion
    isect <- intersect('[['(whichyears, as.character(i)), '[['(whichcountries_inv, y))  
    disp_data$inv_disp[disp_data$year == i & disp_data$country == y] <- mean(dispersion$D_inv[isect])
    disp_data$inv_count[disp_data$year == i & disp_data$country == y] <- length(dispersion$D_inv[isect])
    
    #applicant dispersion
    isect <- intersect('[['(whichyears, as.character(i)), '[['(whichcountries_app, y)) 
    disp_data$app_disp[disp_data$year == i & disp_data$country == y] <- mean(dispersion$D_app[isect])
    disp_data$app_count[disp_data$year == i & disp_data$country == y] <- length(dispersion$D_app[isect])
    
    cat(i, y, " ")
  }
}

##graph of country dispersion
library(ggplot2)
library(directlabels)
library(Cairo)
                        
graph1 <- ggplot(data = disp_data[disp_data$year < 2015 & disp_data$year > 1988, ], aes(year, inv_disp, color = country))+
  stat_smooth(size = 0.8, se = FALSE, span = 0.3) +
  ylab("country dispersion") +
  labs(title = "Country dispersion in selected countries (inventor data)", 
       caption = "CH - Switzerland, CN - China, DE - Germany, FR - France, GB - Great Britain, JP - Japan, KR - Korea, US - United States") +
  theme_light() +
  theme(plot.caption = element_text(size = 7.5))

png(file = "./graphs/Country dispersion in selected countries (inventor data).png",
    type = "cairo", height = 1600, width = 2500, res = 320)
direct.label(graph1, "last.qp")
dev.off()

#patent counts800
graph_inv_count <- ggplot(data = disp_data[disp_data$year < 2015 & disp_data$year > 1980, ], aes(year, inv_count, color = country))+
  stat_smooth(size = 1, se = FALSE, span = 0.3) +
  #geom_line(size = 1) +
  ylab("Patent count") +
  labs(title = "Patent counts in selected countries (inventor data)", 
       caption = "CH - Switzerland, CN - China, DE - Germany, FR - France, GB - Great Britain, JP - Japan, KR - Korea, US - United States") +
  theme(plot.caption = element_text(size = 7.5))

direct.label(graph_inv_count, last.polygons)
