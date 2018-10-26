#create a single-entry family-id dataset with relevant values for further analysis

load("./datasource/TPF/201803_TPF_Core.RData")

tpf_families <- subset(tpf_core, select = c(Family_id, USPTO_app_first, EPO_app_first, JPO_app_first,
                                            PCT_app_first))
rm(tpf_core)

#tranform application dates into date format. Convert to character first to make as.Date work
tpf_families[, 2:5] <- lapply(tpf_families[, 2:5], as.character)
tpf_families[, 2:5] <- lapply(tpf_families[, 2:5], as.Date, format = "%Y%m%d")
str(tpf_families)

####find out which application was first and fill this info into new column first_app

#replace NA values in dates with 3000-01-01 to enable comparisons (didn't work with NA values)
for (i in 2:5) {
  isna <- which(is.na(tpf_families[, i]))
  tpf_families[isna, i] <- "3000-01-01"
}

#initialize new columns for "first" data
tpf_families$first_app <- NA
tpf_families$first_app_year <- as.numeric(NA)

#calculate values for which patent office the patent was registered at first (first_app) and what was the year of the first
#application (first_app_year)
attach(tpf_families)
#US first
first <- which((USPTO_app_first <= EPO_app_first) & (USPTO_app_first <= JPO_app_first) & (USPTO_app_first <= PCT_app_first))
tpf_families$first_app[first] <- "USPTO"
tpf_families$first_app_year[first] <- as.numeric(format(tpf_families$USPTO_app_first[first], "%Y"))
#EPO first
first <- which((EPO_app_first <= USPTO_app_first) & (EPO_app_first <= JPO_app_first) & (EPO_app_first <= PCT_app_first))
tpf_families$first_app[first] <- "EPO"
tpf_families$first_app_year[first] <- as.numeric(format(tpf_families$EPO_app_first[first], "%Y"))
#JPO first
first <- which((JPO_app_first <= EPO_app_first) & (JPO_app_first <= USPTO_app_first) & (JPO_app_first <= PCT_app_first))
tpf_families$first_app[first] <- "JPO"
tpf_families$first_app_year[first] <- as.numeric(format(tpf_families$JPO_app_first[first], "%Y"))
#PCT
first <- which((PCT_app_first <= EPO_app_first) & (PCT_app_first <= JPO_app_first) & (PCT_app_first <= USPTO_app_first))
tpf_families$first_app[first] <- "PCT"
tpf_families$first_app_year[first] <- as.numeric(format(tpf_families$PCT_app_first[first], "%Y"))
detach(tpf_families)


#return 3000-01-01 code to NA values
for (i in 2:5) {
  isna <- which(tpf_families[, i] == "3000-01-01")
  tpf_families[isna, i] <- NA
}
rm(first, isna)

tpf_families$first_app <- as.factor(tpf_families$first_app)
table(tpf_families$first_app)
####

####add country column that shows originating country if applicants/inventors come from a single country and "INT" if applicants are international
load("./datasource/TPF/unicountries_app.RData")

tpf_families$country_app <- as.character(NA)
whichint_app <- which(lapply(unicountries_app, length) > 1) #the whichint vectors contain the indices of patens with inventors/applicants from more than one country
tpf_families$country_app[whichint_app] <- "INT"
whichcountry_app <- which(lapply(unicountries_app, length) == 1)
tpf_families$country_app[whichcountry_app] <- unicountries_app[whichcountry_app]
rm(whichint_app, whichcountry_app)

load("./datasource/TPF/unicountries_inv.RData")
tpf_families$country_inv <- as.character(NA)
whichint_inv <- which(lapply(unicountries_inv, length) > 1) 
tpf_families$country_inv[whichint_inv] <- "INT"
whichcountry_inv <- which(lapply(unicountries_inv, length) == 1)
tpf_families$country_inv[whichcountry_inv] <- unicountries_inv[whichcountry_inv] 
rm(whichint_inv, whichcountry_inv)

tpf_families$country_app <- as.factor(unlist(tpf_families$country_app))
tpf_families$country_inv <- as.factor(unlist(tpf_families$country_inv))
table(tpf_families$country_app)

#save the dataset

save(tpf_families, file = "./datasource/TPF/tpf_families.RData") 
