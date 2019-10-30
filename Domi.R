library(readxl)
wti <- read_excel("WTI2.xlsx", col_types = c("date", "numeric", "numeric", "numeric")

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



