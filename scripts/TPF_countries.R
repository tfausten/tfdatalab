##This script creates and saves matrices containing single- and multi-entry (for international 
##cases) vectors of the countries involved in filing patents, using both applicant and inventor data

load("./datasource/TPF/201803_TPF_Applicants.RData")
str(tpf_applicants)

#make a subset of tpf_applicants that only includes rows in which Country != "" & is not NA. In hope to speed up the loop
tpf_app.hascountry <- tpf_applicants[tpf_applicants$Country != "" & is.na(tpf_applicants$Country) == FALSE, c(1, 4)]
str(tpf_app.hascountry)
head(tpf_app.hascountry)

##initialize the list of country vectors (multi-entry)
allcountries_app <- as.list(matrix(data = list(), nrow = max(tpf_applicants$Family_id), ncol = 1))

### nested loop to create country vectors
start.time <- Sys.time() #measure computation time
for (i in levels(tpf_app.hascountry$Country)) {     #cycle through all country factor levels
  countrypos <- tpf_app.hascountry[tpf_app.hascountry$Country == i, "Family_id"] #find family ids associated with the country
  
  for (y in countrypos) { #cycle through family ids found
    allcountries_app[[y]] <- c(allcountries_app[[y]], i) #add respective country to the vector associated to family id
  }
  
  cat(i, " ")  #for monitoring progress
  
}
print(time.taken <- Sys.time() - start.time)

head(allcountries_app)
save(allcountries_app, file = "./datasource/TPF/allcountries_app.RData")

########create similar list that contains only unique country entries per list item
unicountries_app <- as.list(matrix(data = list(), nrow = max(tpf_applicants$Family_id), ncol = 1))

start.time <- Sys.time()
unicountries_app <- lapply(allcountries_app, unique)
print(time.taken <- Sys.time() - start.time)

head(unicountries_app)

save(unicountries_app, file = "./datasource/TPF/unicountries_app.RData")
rm(unicountries_app, allcountries_app, tpf_app.hascountry, tpf_applicants)

##########do the same for the inventors dataset
load("./datasource/TPF/201803_TPF_Inventors.RData")
str(tpf_inventors)

#make a subset of tpf_applicants that only includes rows in which Country != "" & is not NA. In hope to speed up the loop
tpf_inv.hascountry <- tpf_inventors[tpf_inventors$Country != "" & is.na(tpf_inventors$Country) == FALSE, c(1, 4)]
str(tpf_inv.hascountry)
head(tpf_inv.hascountry)

##initialize the list of country vectors (mutli-entry)
allcountries_inv <- as.list(matrix(data = list(), nrow = max(tpf_inventors$Family_id), ncol = 1))

### nested loop to create country vectors
start.time <- Sys.time() #measure computation time
for (i in levels(tpf_inv.hascountry$Country)) {     #cycle through all country factor levels
  countrypos <- tpf_inv.hascountry[tpf_inv.hascountry$Country == i, "Family_id"] #find family ids associated with the country
  
  for (y in countrypos) { #cycle through family ids found
    allcountries_inv[[y]] <- c(allcountries_inv[[y]], i) #add respective country to the vector associated to family id
  }
  
  cat(i, " ")  #for monitoring progress
  
}
print(time.taken <- Sys.time() - start.time)

head(allcountries_inv)

save(allcountries_inv, file = "./datasource/TPF/allcountries_inv.RData")

########same procedure for unique countries
unicountries_inv <- as.list(matrix(data = list(), nrow = max(tpf_inventors$Family_id), ncol = 1))

start.time <- Sys.time()
unicountries_inv <- lapply(allcountries_inv, unique)
print(time.taken <- Sys.time() - start.time)

head(unicountries_inv)

save(unicountries_inv, file = "./datasource/TPF/unicountries_inv.RData")
rm(list = ls())

