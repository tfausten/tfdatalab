##description
load("./datasource/TPF/201803_TPF_IPC.RData")

str(tpf_ipc)
head(tpf_ipc, 30)

#make small subset to tpf_ipc for experimentation
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
  comatrix <- matrix(data = as.numeric(0), nrow = length(levels(data$IPC)), ncol = length(levels(data$IPC)))
  rownames(comatrix) <- levels(data$IPC)
  colnames(comatrix) <- levels(data$IPC)
  
  for (i in 1:max(data$Family_id)) {
    datasub <- which(data$Family_id == i)
    print(i)
    
    for (x in data[datasub, "IPC"]) {
      for (y in data[datasub, "IPC"]) {
        if (x == y) next #avoid 
        comatrix[x, y] <- comatrix[x, y] + 1
      }
    }
  }
  
  return(comatrix)
}

#alternative approach test
coclass2 <- function (data) {
  comatrix <- matrix(data = as.numeric(0), nrow = length(levels(data$IPC)), ncol = length(levels(data$IPC)))
  rownames(comatrix) <- levels(data$IPC)
  colnames(comatrix) <- levels(data$IPC)
  
  for (i in 1:max(data$Family_id)) {
    print(i)
    ipc_vec <- data[data$Family_id == i, "IPC"]
    
    #for (y in ipc_vec) {
     # comatrix[ipc_vec, y] <- comatrix[ipc_vec, y] + 1
    #}
    comatrix[ipc_vec, ipc_vec] <- comatrix[ipc_vec, ipc_vec] + 1
  }
  return(comatrix)
}

test <- coclass2(tpf_ipc10k)
str(test)
max(test)
isSymmetric(test)
