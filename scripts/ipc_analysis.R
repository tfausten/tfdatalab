##description
load("./datasource/TPF/201803_TPF_IPC.RData")

####find the count of IoT related IPC categories

#G05B019/418: Total factory control, i.e., centrally controlling a plurality of machines, e.g. direct or distributed numerical control (DNC), flexible manufacturing systems (FMS),
#integrated manufacturing systems (IMS), computer integrated manufacturing (CIM)
want <- which(tpf_ipc$IPC == "G05B019/418")
length(want)
tpf_ipc[want, ]
#G06F015/16: Combinations of two or more digital computers each having at least an arithmetic unit, a programme unit and a register, e.g. for a simultaneous processing of
#several programmes
want <- which(tpf_ipc$IPC == "G06F015/16")
length(want)
tpf_ipc[want, ]
#G08C017/02
want <- which(tpf_ipc$IPC == "G08C017/02")
length(want)
tpf_ipc[want, ]
#H04B007/26
want <- which(tpf_ipc$IPC == "H04B007/26")
length(want)
tpf_ipc[want, ]
#H04L012/28
want <- which(tpf_ipc$IPC == "H04L012/28")
length(want)
tpf_ipc[want, ]
#H04L029/06
want <- which(tpf_ipc$IPC == "H04L029/06")
length(want)
tpf_ipc[want, ]
#H04L029/08
want <- which(tpf_ipc$IPC == "H04L029/08")
length(want)
tpf_ipc[want, ]
#H04W004/00
want <- which(tpf_ipc$IPC == "H04W004/00")
length(want)
tpf_ipc[want, ]
#H04W072/04
want <- which(tpf_ipc$IPC == "H04W072/04")
length(want)
tpf_ipc[want, ]
#H04W084/18
want <- which(tpf_ipc$IPC == "H04W084/18")
length(want)
tpf_ipc[want, ]
