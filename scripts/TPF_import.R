##This script converts csv files from the tpf database to RData files to make them directly usable for following scripts
#import and save core dataset
tpf_core <- read.delim("./datasource/TPF/201803_TPF_Core.txt", sep = '|', stringsAsFactors = FALSE)
save(tpf_core, file = "./datasource/TPF/201803_TPF_Core.RData")
rm(tpf_core)

#import and save ipc categories
tpf_ipc <- read.delim("./datasource/TPF/201803_TPF_IPC.txt", sep = '|', stringsAsFactors = FALSE)
tpf_ipc$Appln_auth <- as.factor(tpf_ipc$Appln_auth)
str(tpf_ipc)
save(tpf_ipc, file = "./datasource/TPF/201803_TPF_IPC.RData")
rm(tpf_ipc)

#import and save applicants file
tpf_applicants <- read.delim("./datasource/TPF/201803_TPF_Applicants.txt", sep = '|', stringsAsFactors = FALSE)
tpf_applicants$Country <- as.factor(tpf_applicants$Country)
str(tpf_applicants)
save(tpf_applicants, file = "./datasource/TPF/201803_TPF_Applicants.RData")
rm(tpf_applicants)

#import and save inventor file
tpf_inventors <- read.delim("./datasource/TPF/201803_TPF_Inventors.txt", sep = '|', stringsAsFactors = FALSE)
tpf_inventors$Country <- as.factor(tpf_inventors$Country)
str(tpf_inventors)
save(tpf_inventors, file = "./datasource/TPF/201803_TPF_Inventors.RData")
rm(tpf_inventors)