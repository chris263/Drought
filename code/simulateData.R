
simulateData <- function(varieties, blocks,treatments,locations,days){
  #creating pheno data for each variety
  size1 <- length(blocks)*length(treatments)
  meanV <- 80
  data_wattered <- c()
  for(i in 1:length(locations)){
    meanV <- meanV+13
    for (j in 1:length(varieties)){
      dataW <- rnorm(size1, meanV, 5)
      meanV = meanV+3
      data_wattered<-append(data_wattered,dataW)
    }
  }
  length(data_wattered)

  #preparing columns for varieties
  vColumn = c()
  vColumn = sort(rep(varieties,length(blocks)*length(treatments)*length(days)*length(locations)))
  length(vColumn)

  #Column for blocks
  bCol1 <- rep(1,length(days))
  bCol2 <- rep(2,length(days))
  bCol3 <- rep(3,length(days))
  bCol12 <- append(bCol1, bCol2)
  bCol123 <- append(bCol12, bCol3)
  bColumn = rep(bCol123,length(varieties)*length(treatments)*length(locations))
  length(bColumn)

  #Column for location
  for (i in 1:length(varieties)){
    lCol1 = rep(locations[1],length(blocks)*length(days)*length(treatments))
    lCol2 = rep(locations[2],length(blocks)*length(days)*length(treatments))
    lColumn = append(lCol1,lCol2)
    length(lColumn)
  }


  #column treatments
  tCol1 = rep(treatments[1],length(days)*length(blocks))
  tCol2 = rep(treatments[2],length(days)*length(blocks))
  tCol12 = append(tCol1,tCol2)
  tColumn = rep(tCol12,length(varieties)*length(locations))
  length(tColumn)

  #Column with days
  dColumn = rep(days,length(varieties)*length(blocks)*length(treatments)*length(locations))
  length(dColumn)

  dataf<<-data.frame(variety=as.factor(vColumn),
                     location=as.factor(lColumn),
                     blocks=as.factor(bColumn),
                     treatments=as.factor(tColumn),
                     days=as.factor(dColumn),
                     daysDouble=as.double(dColumn),
                     pheno=as.double(data_wattered))
  dim(dataf)
  return(dataf)

}

