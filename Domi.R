library("readxl")
wti<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/WTI2.xlsx", sep=""))

#wti <- read_excel("WTI2.xlsx", col_types = c("date", "numeric", "numeric", "numeric")

View(wti)
typeof(wti$Date)
wti$date <- as.Date(wti$Date)

ccf(wti$CL1,wti$CL8,main="Cross correlation CL1 and CL8")

#paraméterek bekérésére:
windowsize <- readline(prompt="Windowsize:");
numberoflegs <- readline(prompt="Number of legs:");
commodity1 <- readline(prompt="First commodity:");
commodity2 <- readline(prompt="Second commodity:"); #itt lehet inkább vmi listát kéne egyben bekérni
startdate <- readline(prompt="Start date:");
enddate <- readline(prompt="End date:");

#sima korrelacio, ez nem is kell
correl=cor(wti[sapply( wti, is.numeric)])

# window_length=2000, erre kellene hibauzenet
#nem tudom az elejerol inditani az ablakot :(, azert van -10, 6 listaelembol áll a ccf eredmenye (erre 201-tol indulo listat ad, vagyis azelso 201 elem 0)

ij_ccf<-list()
cl_length<-nrow(wti)
cl_number<-ncol(wti)
window_length=2000
i=2
j=window_length
k=3
cc_lag=2

#ha még csak erre futtatjátok, akkor működik
cl_ccf<-list()
j=2551
while(j <= cl_length){
  cl_ccf[[j]]<-(ccf(wti$CL1[j-(1):j], wti$CL2[j-(1):j]))
  j <- j + 1
}

#adott ablakhoz tartozó acf-ek minden lag-re
#rakhatunk még ciklust, vagy mátrixba is tölthetjük, de kiszedhető korrekten
result_acf[[j]]<-cl_ccf[[j]]$acf
result_lag[[j]]<-cl_ccf[[j]]$lag

#a ciklust am elrontotam sorry

while (j<=(cl_length)){
  while(i <= cl_number){
    while(k <= cl_number){
      test_ccf[[j]]<-(ccf(wti[lag:(j-lag),i], lag:(j-lag),k],cc_lag))
      k<-i+1
    }
    j <- j + 1
  i <- i + 1
  k<-i+1
}
}


##
library(ggplot2)
data("wti", package = "ggplot2")

gg <- ggplot(wti, 
plot(gg)
