pollutantmean<-function(directory, pollutant, id=1:332){
  filelist <- list.files(path=directory, full.names = TRUE)
  pollutantdata <- data.frame()
  for (i in id) {
    pollutantdata = rbind(pollutantdata, read.csv(filelist[i]))
  }
  mean(pollutantdata[,pollutant], na.rm =TRUE)

}

complete <- function(directory,id=1:332){
  filelist <- list.files(path=directory, full.names = TRUE)
  nobs <- data.frame(Id=integer(0), nobs=integer(0))
  for (i in id) {
    pollutantdata <- read.csv(filelist[i])
    n <- nrow(pollutantdata[complete.cases(pollutantdata),])
    nobs <- rbind(nobs,data.frame(id=i, nobs=n))
  }
 nobs
}


corr <- function(directory, threshold=0){
  filelist <- list.files(path=directory, full.names = TRUE)
  res <- numeric(0)
  k <-1
  for (i in 1:length(filelist)){
    pollutantdata <- read.csv(filelist[i])
    if(sum(complete.cases(pollutantdata))> threshold){
      res[k] = cor(pollutantdata["nitrate"], pollutantdata["sulfate"], use="complete.obs")
      k<- k+1
      }
  }
  res
}