CrossCorrInput <- function(AssetList,StarDate,EndDate,WindowLength,LagLength){
  variables <- list(AssetList,StarDate,EndDate,WindowLength,LagLength)
  return(variables)
}

##############################

PrepareDatabase <- function(df,StartDate,EndDate,SelectColumns){
  x <- as.Date(StartDate)
  y <- as.Date(EndDate)
  RawData <- df[df$Date >= x & df$Date <= y,] %>%
    dplyr::as_tibble() %>%
    dplyr::select(SelectColumns)
  
  RawData<- subset(RawData,(as.numeric(strftime(as.Date(RawData$Date, "%Y-%m-%d"), "%u")) %in% c(1, 2, 3, 4, 5)))
  return(RawData)}

##############################

TSDiff <- function(df){
  colnumbers <- ncol(df)
  rownumbers <- nrow(df)
  
  for (asset3 in 2:rownumbers-1){
    for (asset4 in 2:colnumbers){
      df[[asset3,asset4]]<-df[[asset3+1,asset4]]-df[[asset3,asset4]]
    }
  }
  
  #a differenciazas miatt az utolso sort torolhetjuk
  df<- df[-rownumbers,]
  return(df)
}

##############################

CrossCorrAnalysis <- function(RawData,lengthlag,lengthwindow){
  colnumbers <- ncol(RawData)
  rownumbers <- nrow(RawData)
  colnames_tray <- c()
  runwindow=0
  k=1
  
  eredmeny <- data.frame(1,nrow = rownumbers-lengthwindow-lengthlag,ncol = ((colnumbers-1)*(colnumbers-2))+1)
  newelem <- "Date"
  colnames_tray <- c(colnames_tray,newelem)
  
  for (asset1 in 2:colnumbers) {
    for (asset2 in 2:colnumbers){
      if (asset1!=asset2){
        
        k=k+1
        
        #elnevezzük az oszlopokat
        
        newelem <-paste0("(",colnames(RawData)[asset1],",",colnames(RawData)[asset2],")")
        colnames_tray <- c(colnames_tray,newelem)
        
        #majd feltöltjük a korreláció ereményeivel
        
        for (runwindow in 1:(rownumbers-lengthwindow-lengthlag)){
          
          eredmeny[runwindow,1]=RawData[runwindow,1]
          eredmeny[runwindow,k]=cor(RawData[runwindow:(lengthwindow+runwindow),asset1],RawData[(runwindow+lengthlag):(lengthwindow+runwindow+lengthlag),asset2])
        }
      } 
    }
  }
  #elneveztük a ciklusban kapott nevekre az oszlopokat
  
  colnames(eredmeny)=colnames_tray    
  return(eredmeny)
}

##############################
library(scales)
gg <- function(dates, timeseries){
  g<-ggplot(Results,aes(dates,timeseries)) +
    geom_line() + scale_x_date(labels = date_format("%m-%Y")) +  ggtitle("Keresztkorreláció")
  labs(
    y="korreláció",
    x="Idő",
    caption = "Source: wti")
  plot(g)
}

##############################

#mivel maceras lenne a kesz dinamikus korrelacio tablankat visszaalakitani korrelacios matrix formaba, 
#ez egyelore nem jo



#csupa 1-es oszlopok beszurasa
withOnes <- data.frame(Results[1])
for(i in 1:23){
  if (((i-1)*23+2)!=((i-1)*23+2+i-1)) withOnes <- cbind(withOnes, Results[((i-1)*23+2):((i-1)*23+2+(i-2))])
  withOnes <- cbind(withOnes, rep(1, 105))
  if (((i-1)*23+2+(i-1))!=((i-2)*23+24)) withOnes <- cbind(withOnes, Results[((i-1)*23+2+(i-1)):((i-1)*23+24)])
}
withOnes <- cbind(withOnes, Results[((24-1)*23+2):((24-1)*23+24)])
withOnes <- cbind(withOnes, rep(1, 105))






corMatrix <- matrix(slice(Results, 1)[-1],nrow=23)


corMat <- cor(wti$CL1, lag(wti$CL2,30), use="na.or.complete")


Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
                      sampleSize = nrow(corMat))

