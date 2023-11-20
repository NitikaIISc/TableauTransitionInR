setwd("C:/UCLAAnderson/10KfilingsTables/Data/UndergradsQualtricsQA")
data<-read.csv("OFG_WEF_June 26_ 2023.csv", check.names = F)

write.csv(t(data[c(1:2),]),"Data1Dictionary.csv")
