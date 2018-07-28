##description
load("./datasource/TPF/201803_TPF_IPC.RData")

#make vector of IoT-related IPC categories (What is the source?)
iot <- c("G05B019/418", "G06F015/16", "G08C017/02", "H04B007/26", "H04L012/28", "H04L029/06", "H04L029/08", "H04W004/00",
         "H04W072/04", "H04W084/18")

#how many entries of the IoT-categories in the database?
sapply(iot, function(x) {sum(tpf_ipc$IPC == x)})