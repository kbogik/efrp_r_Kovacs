#Mielőtt az itteni sorok futtatásra kerülnek, a Beadandó_functions fileból le kell futtatni az ottani függvényeket, hiszen azok kerülnek itt felhasználásra
#A vizsgálat input paramétereit a 16.Sorban adhatjuk meg a CrossCorrInput argumentumaiként

library(readxl)
library(rstudioapi)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)

#A WTI árfolyamokat tartalmazó excell betöltése
wti<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/WTI2.xlsx", sep=""))
wti$Date = as.Date(wti$Date, format="%m/%d %M:%S")

#A vizsgálandó oszlopok (CL2,CL2...stb megadása. A Date legyen mindig megadva), kezdeti illetv végső dátuma a vizsgálandó időszaknak
#valamint az ablak és lag méretek megadáa
AnalysisInputs <- CrossCorrInput(c("Date","CL1", "CL2","CL3","CL4","CL5","CL6","CL7","CL8","CL9","CL10","CL11","CL12","CL13","CL14","CL15","CL16","CL17","CL18","CL19","CL20","CL21","CL22","CL23","CL24"),"2011-04-29","2012-05-04",130,30)

#A megadott feltételekre a WTI alap adatbázis leszűkítése
RawData <- PrepareDatabase(wti,AnalysisInputs[[2]],AnalysisInputs[[3]],AnalysisInputs[[1]])

#Az idősor egyszeri differenciázása, hogy stacioner legyen. (az utolsó sor törlésre kerül a RawData-ból)
usedata <- TSDiff(RawData)

#A dinamikus keresztkorreláció kiszámítása majd eltárolása egy data.frame-ba, melynek az oszlopyai (Date, (CL1,CL2), (CL1,CL3)...)
Results<-CrossCorrAnalysis(usedata,AnalysisInputs[[5]],AnalysisInputs[[4]])

#Kiszámoljuk a minimumot, a maximumot, és az átlagot
MinMaxAvg(Results)

#A Results data.frame dátum oszlopa numerikus alakból dátum formátumba írása
Results <- Results %>%
  as_tibble() %>%
  mutate(Date=as.Date(Date,origin="1970-01-01"))


#ha random kivalasztotok egy pl.:grafikon$`(CL1,CL2)`
gg(Results$Date,Results$`(CL1,CL2)`)


##############################



#csupa 1-es oszlopok beszurasa
withOnes <- data.frame(Results[1])
for(i in 1:23){
  if (((i-1)*23+2)!=((i-1)*23+2+i-1)) withOnes <- cbind(withOnes, Results[((i-1)*23+2):((i-1)*23+2+(i-2))])
  withOnes <- cbind(withOnes, rep(1, 105))
  if (((i-1)*23+2+(i-1))!=((i-2)*23+24)) withOnes <- cbind(withOnes, Results[((i-1)*23+2+(i-1)):((i-1)*23+24)])
}
withOnes <- cbind(withOnes, Results[((24-1)*23+2):((24-1)*23+24)])
withOnes <- cbind(withOnes, rep(1, 105))


statCorMat <- matrix(withOnes[1,][-1], nrow=24, ncol=24)

statCorMat <- (data.frame(statCorMat))

statCorMat2 <- data.matrix(statCorMat)

for (i in 2:24){
  for (j in 1:(i-1)){
    statCorMat2[i,j] <- statCorMat2[j,i]
  }
}


corrplot(data.matrix(statCorMat2))

#elmentjuk a korrelációs mátrixot, hogy az rmarkdownba használhassuk
save(statCorMat2,file="statCorMat.Rdata")
