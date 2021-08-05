library('Synth')
library(readxl)
#install.packages("writexl")
library(writexl)
library(ggplot2)
####### importamos los datos ######
mydata = read.csv("C:/Users/jonat/Documents/Tesis/r/Final1.csv")

########### pasamos a factor lo que es necesario #########
str(mydata)
mydata$factBorough<- as.factor(mydata$Borough)
mydata$factBorough
str(mydata)
mydata$factBorough<-as.numeric(mydata$factBorough)
str(mydata)
dict <- data.frame(mydata$Borough,mydata$factBorough)
######################
### trabajamos con las covariables #############
names(mydata)[68:72]
unique(mydata$factBorough[mydata$Inun == 0]) 

names(mydata)[62:67] 



treated <- unique(mydata$factBorough[mydata$Inun == 1])
control <- unique(mydata$factBorough[mydata$Inun == 0]) 
treated
control
gapgroup<-c()
#### creamos el dataset para sc #############
for (i in treated){
  dataprep.out<-
    dataprep(
      foo = mydata, #DATASET 
      predictors = c(names(mydata)[14:59],"birth.United.Kingdom____.percent","birth.Non.United.Kingdom____.percent","nationality.British____.percent","nationality.Non.British____.percent","percent_attainers",names(mydata)[68:72]), #covariables 
      predictors.op = "mean", #metodo a ser usado en los predictores 
      dependent = "lnyMean2", 
      unit.variable = "factBorough", #
      time.variable = "Year",
      treatment.identifier = i,
      controls.identifier = c(unique(mydata$factBorough[mydata$Inun == 0]) ),
      time.predictors.prior = c(2001:2008),
      special.predictors = list(
        list("lnyMean2", 2007, "mean"),
        list("lnyMean2", 2005, "mean"),
        list("lnyMean2", 2001, "mean")
      ),
      time.optimize.ssr = c(2001:2009),
      unit.names.variable = "Borough",
      time.plot = 2001:2020
    )
  #################### sintetico ############
  synth.out <- synth(dataprep.out)
  
  round(synth.out$solution.w,2)
  synth.out$solution.v
  gaps<- dataprep.out$Y1plot-(
    dataprep.out$Y0plot%*%synth.out$solution.w
  ) ; gaps
  synth.tables <- synth.tab(
    dataprep.res = dataprep.out,
    synth.res = synth.out)
  synth.tables
  
  pvar <- "C:/Users/jonat/Documents/Tesis/r/pesos {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(pvar)
  write_xlsx(synth.tables$tab.w,namepesos) #pesos
  
  vvar <- "C:/Users/jonat/Documents/Tesis/r/vpesos {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(vvar)
  write_xlsx(data.frame(synth.tables$tab.v),namepesos) #pesos
  
  losswvar <- "C:/Users/jonat/Documents/Tesis/r/lossw {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(losswvar)
  write_xlsx(data.frame(synth.tables$tab.loss),namepesos) #pesos
  
  comp <- "C:/Users/jonat/Documents/Tesis/r/comp {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(comp)
  write_xlsx(data.frame(synth.tables$tab.pred),namepesos) #pesos
  ######## graficos ##33
  
  graphpath<- "treatvssint {unique(mydata$Borough[mydata$factBorough == i])}.jpg"
  graphpath<-glue(graphpath)
  jpeg(graphpath, width = 1000, height = 800 )
  path.plot(dataprep.res = dataprep.out,synth.res = synth.out,Ylab = c("Treated Real vs Sintetic Price"),Ylim = c(12,14.5))
  dev.off()
  
  graphpath<- "gap {unique(mydata$Borough[mydata$factBorough == i])}.jpg"
  graphpath<-glue(graphpath)
  jpeg(graphpath, width = 1000, height = 800)
  gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)
  dev.off()
  
  gap <- data.frame(gaps)
  gapname<- "{unique(mydata$Borough[mydata$factBorough == i])} gap"
  gapname<-glue(gapname)
  names(gap) <- gapname
  gaprute <- "C:/Users/jonat/Documents/Tesis/r/gap {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  gaprute<-glue(gaprute)
  gapgroup<-c(gapgroup,gap)
  write_xlsx(gap,gaprute) #pesos
  print(i)
}
gapgroup
gapg <- data.frame(gapgroup)
write_xlsx(gapg,"C:/Users/jonat/Documents/Tesis/r/gaps list log semi.xlsx")

gapg$Year <- c(2001:2020)
gapg<- t(gapg)
names(gapg)

#################################3 PLACEBO LABURO ############################
gappgroup <- c()
for (i in control){
  L <- unique(mydata$factBorough[mydata$Inun == 0])
  L<- L[!(L %in% i)] #sacamos de los placebos aquel placebo usado para observar. 
  
  
  dataprep.out<-
    dataprep(
      foo = mydata, #DATASET 
      predictors = c(names(mydata)[14:59],"birth.United.Kingdom____.percent","birth.Non.United.Kingdom____.percent","nationality.British____.percent","nationality.Non.British____.percent","percent_attainers",names(mydata)[68:72]), #covariables 
      predictors.op = "mean", #metodo a ser usado en los predictores 
      dependent = "lnyMean2", 
      unit.variable = "factBorough", #
      time.variable = "Year",
      treatment.identifier = i,
      controls.identifier = c(L),
      time.predictors.prior = c(2001:2008),
      special.predictors = list(
        list("lnyMean2", 2007, "mean"),
        list("lnyMean2", 2005, "mean"),
        list("lnyMean2", 2001, "mean")
      ),
      time.optimize.ssr = c(2001:2009),
      unit.names.variable = "Borough",
      time.plot = 2001:2020
    )
  #CONTROL SINTETICO 
  synth.out <- synth(dataprep.out)
  
  #ALGUNAS TABLAS 
  round(synth.out$solution.w,2)
  synth.out$solution.v
  gaps<- dataprep.out$Y1plot-(
    dataprep.out$Y0plot%*%synth.out$solution.w
  ) ; gaps
  synth.tables <- synth.tab(
    dataprep.res = dataprep.out,
    synth.res = synth.out)
  synth.tables
  
  pvar <- "C:/Users/jonat/Documents/Tesis/r/pesos placebo {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(pvar)
  write_xlsx(synth.tables$tab.w,namepesos) #pesos
  
  vvar <- "C:/Users/jonat/Documents/Tesis/r/vpesos placebo {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(vvar)
  write_xlsx(data.frame(synth.tables$tab.v),namepesos) #pesos
  
  losswvar <- "C:/Users/jonat/Documents/Tesis/r/lossw placebo {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(losswvar)
  write_xlsx(data.frame(synth.tables$tab.loss),namepesos) #pesos
  
  comp <- "C:/Users/jonat/Documents/Tesis/r/comp  placebo {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  namepesos<-glue(comp)
  write_xlsx(data.frame(synth.tables$tab.pred),namepesos) #pesos
  
  ######## graficos ##33
  
  graphpath<- "treatvssint placebo {unique(mydata$Borough[mydata$factBorough == i])}.jpg"
  graphpath<-glue(graphpath)
  jpeg(graphpath, width = 1000, height = 800 )
  path.plot(dataprep.res = dataprep.out,synth.res = synth.out,Ylab = c("Treated Real vs Sintetic Price"),Ylim = c(12,14.5))
  dev.off()
  
  graphpath<- "gap placebo {unique(mydata$Borough[mydata$factBorough == i])}.jpg"
  graphpath<-glue(graphpath)
  jpeg(graphpath, width = 1000, height = 800)
  gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)
  dev.off()
  
  ################### GAP CALC ############
  gapp <- data.frame(gaps)
  gapname<- "{unique(mydata$Borough[mydata$factBorough == i])} gap placebo"
  gapname<-glue(gapname)
  names(gapp) <- gapname
  gaprute <- "C:/Users/jonat/Documents/Tesis/r/gap placebo {unique(mydata$Borough[mydata$factBorough == i])}.xlsx"
  gaprute<-glue(gaprute)
  gappgroup<-c(gappgroup,gapp)
  write_xlsx(gapp,gaprute) #pesos
  print(i)
  
}
gappgroup
gappg <- data.frame(gappgroup)
write_xlsx(gappg,"C:/Users/jonat/Documents/Tesis/r/gaps placebo list log semi.xlsx")

gappg$Year <- c(2001:2020)
gappg<- t(gappg)


gappg
gapg
