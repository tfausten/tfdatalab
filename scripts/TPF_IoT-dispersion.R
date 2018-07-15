##
load("./datasource/TPF/201803_TPF_IPC.RData")
head(tpf_ipc, 30)

# IoT IPC-codes according to Ardito et al. 2017 and European Communities Trade Mark Association (2016)
#G05B019/418, G06F015/16, G08C017/02, H04B007/26, H04L012/28, H04L029/06, H04L029/08, H04W004/00, H04W072/04, H04W084/18

#IPC selector function. to return the Family_ids corresponding to a list of IPC-codes. One occurrence of an IPC-code is enough to be included in the
#output vector of Family_ids
ipc.select <- function (data, ipc) {
  ids <- vector(mode = "numeric")
  for (i in ipc) {
    if (substr(i , 9, 10) == "00") i <- substr(i, 1, 8)     #remove last two figures if they are "00", to allow for inclusion of whole subclasses
    ids <- c(ids, data[grep(i, data$IPC, fixed = TRUE), "Family_id"])
  }
  return(unique(ids))
}

#test function
ipc.select(tpf_ipc, ipc = c("C09B005/00", "C09B067/22"))

#make vector that holds IoT-relevant ids (according to Ardito et al. 2017)
iot_ids <- ipc.select(tpf_ipc, ipc = c("G05B019/418", "G06F015/16", "G08C017/02", "H04B007/26", "H04L012/28", "H04L029/06", 
                                       "H04L029/08", "H04W004/00", "H04W072/04", "H04W084/18"))


rm(tpf_ipc)     #ipc data not needed anymore

#load data for dispersion analysis
load("./datasource/TPF/dispersion.RData")        #for previously calculated family-specific dispersion data
head(dispersion)
load("./datasource/TPF/tpf_families.RData")      #for year data
load("./datasource/TPF/allcountries_app.RData")  #for country data on inventors
load("./datasource/TPF/allcountries_inv.RData")  #for country data on applicants

#data.frame for yearly country dispersion data (IoT-specific)
countrygroup <- c("JP", "US", "DE", "CN", "FR", "GB", "KR")     #modify this list to include different countries
disp_data_iot <- data.frame(year = rep(1975:2016, length.out = 42 * length(countrygroup)), 
                        country = rep(countrygroup, each = 42), inv_disp = NA, app_disp = NA, 
                        inv_count = NA, app_count = NA, stringsAsFactors = FALSE)

#for loop to fill the data.frame. Create workingvec of type character to avoid repeated conversion to character vector in the loop through the
#grep function used in the loop below
allcountries_inv <- as.character(allcountries_inv)
allcountries_app <- as.character(allcountries_app)

for (i in 1975:2016) {      #years to be included in the data.frame
  
  years_iot <- intersect(which(tpf_families$first_app_year == i), iot_ids)  #the selection vector takes care of year and IPC selection
  
  for (y in countrygroup) {
    #inventor dispersioin
    ctry <- grep(y, allcountries_inv, fixed = TRUE)       #ctry contains family_ids that have an inventor from country y
    sub <- dispersion$D_inv[intersect(ctry, years_iot)]   #sub is a subset of dispersion data, containing only data relevant to years_iot and ctry
    disp_data_iot$inv_disp[(i-1974) + 42 * (which(countrygroup == y) - 1)] <- mean(sub) #assign yearly country-speficic mean to data.frame. formula finds the correct position
    disp_data_iot$inv_count[(i-1974) + 42 * (which(countrygroup == y) - 1)] <- length(sub) #assign patent count in similar fashion
    
    #applicant dispersion
    ctry <- grep(y, allcountries_app, fixed = TRUE)
    sub <- dispersion$D_app[intersect(ctry, years_iot)]
    disp_data_iot$app_disp[(i-1974) + 42 * (which(countrygroup == y) - 1)] <- mean(sub)
    disp_data_iot$app_count[(i-1974) + 42 * (which(countrygroup == y) - 1)] <- length(sub)
    
    print(c(i, y))
  }
}
#cleanup
rm(i, y, ctry, years_iot, sub, countrygroup)

##graph of country dispersion
library(ggplot2)
library(directlabels)
library(Cairo)

##inventor graphs
graph_inv_disp <- ggplot(data = disp_data_iot[disp_data_iot$year < 2015 & disp_data_iot$year > 1993, ], aes(year, inv_disp, color = country))+
  stat_smooth(size = 1, se = FALSE, span = 0.3) +
  #geom_line(size = 1) +
  ylab("country dispersion") +
  labs(title = "Country dispersion in selected countries (inventor data, IoT-categories)", 
       caption = "CH - Switzerland, CN - China, DE - Germany, FR - France, GB - Great Britain, JP - Japan, KR - Korea, US - United States") +
  theme(plot.caption = element_text(size = 7.5))

direct.label(graph_inv_disp, last.polygons)

#number of patents
graph_inv_count <- ggplot(data = disp_data_iot[disp_data_iot$year < 2014 & disp_data_iot$year > 1993, ], aes(year, inv_count, color = country))+
  #stat_smooth(size = 1, se = FALSE, span = 0.3) +
  geom_line(size = 1) +
  ylab("Patent count") +
  labs(title = "Patent counts in selected countries (inventor data, IoT-categories)", 
       caption = "CH - Switzerland, CN - China, DE - Germany, FR - France, GB - Great Britain, JP - Japan, KR - Korea, US - United States") +
  theme(plot.caption = element_text(size = 7.5))

direct.label(graph_inv_count, last.polygons)

##applicant graphs
graph_app_disp <- ggplot(data = disp_data_iot[disp_data_iot$year < 2013 & disp_data_iot$year > 1994, ], aes(year, app_disp, color = country))+
  stat_smooth(size = 1, se = FALSE, span = 0.3) +
  #geom_line(size = 1) +
  ylab("country dispersion") +
  labs(title = "Country dispersion in selected countries (applicant data, IoT-categories)", 
       caption = "CH - Switzerland, CN - China, DE - Germany, FR - France, GB - Great Britain, JP - Japan, KR - Korea, US - United States") +
  theme(plot.caption = element_text(size = 7.5))

direct.label(graph_app_disp, last.polygons)

#number of patents
graph_app_count <- ggplot(data = disp_data_iot[disp_data_iot$year < 2015 & disp_data_iot$year > 1993, ], aes(year, app_count, color = country))+
  #stat_smooth(size = 1, se = FALSE, span = 0.3) +
  geom_line(size = 1) +
  ylab("Patent count") +
  labs(title = "Patent counts in selected countries (applicant data)", 
       caption = "CH - Switzerland, CN - China, DE - Germany, FR - France, GB - Great Britain, JP - Japan, KR - Korea, US - United States") +
  theme(plot.caption = element_text(size = 7.5))

direct.label(graph_app_count, last.polygons)
