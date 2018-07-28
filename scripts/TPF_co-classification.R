##This script uses IPC classifications to make a co-classification matrix of tpf
load("./datasource/TPF/201803_TPF_IPC.RData")
str(tpf_ipc)
head(tpf_ipc)
length(unique(tpf_ipc$IPC))

#make small subsets of tpf_ipc for experimentation
tpf_ipc23 <- tpf_ipc[1:23, ]
tpf_ipc1000 <- tpf_ipc[1:1000, ]
tpf_ipc10k <- tpf_ipc[1:10000, ]
#convert IPC values to factor
tpf_ipc23$IPC <- as.factor(tpf_ipc23$IPC)
tpf_ipc1000$IPC <- as.factor(tpf_ipc1000$IPC)
tpf_ipc10k$IPC <- as.factor(tpf_ipc10k$IPC)
str(tpf_ipc1000)


######
#function that returns a co-classification matrix from input of tpf_ipc dataset. IPC variable is assumed to be of type factor

coclass <- function (data) {
  ipc_classes <- levels(data$IPC)
  comatrix <- matrix(data = as.numeric(0), nrow = length(ipc_classes), ncol = length(ipc_classes))
  rownames(comatrix) <- ipc_classes
  colnames(comatrix) <- ipc_classes
  
  for (i in 1:max(data$Family_id)) {
    ipc_vec <- data[data$Family_id == i, "IPC"]

    comatrix[ipc_vec, ipc_vec] <- comatrix[ipc_vec, ipc_vec] + 1
  }

  return(comatrix)
}

start_time <- Sys.time()
test <- coclass(tpf_ipc10k)
test <- coclass(tpf_ipc23)
print(Sys.time() - start_time)


str(test)
max(test)
isSymmetric(test)
