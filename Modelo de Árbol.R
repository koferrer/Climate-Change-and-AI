#Árbol
data<-read.csv("D:/GitHub/Climate-Change-and-AI/Data/Model/ModelData.csv")
summary(data)

#Limpiar variables del NA
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.03])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#let's select a response of interest
#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
ids<-c("iso_code3","country")
response<-"a_social_development"
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids, response, bad.vars)))
predictors
summary(data[,response])

#Se divide en dos el árbol. Diferenciando por nivel de emisiones.
threshold<-as.numeric(quantile(data[,response],0.70,na.rm=TRUE))
data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))
summary(data$response.binary)

#Remover NAs
dataclean<-subset(data,is.na(response.binary)==FALSE)
summary(dataclean)

# Solo para casos completos
data.model<-dataclean[,c("response.binary",predictors)]
data.model<-data.model[complete.cases(data.model),]
summary(data.model)
dim(data.model)

#Construcción del modelo
model<-as.formula(paste0("response.binary","~",paste(predictors,collapse="+")))
model

#NUEVO INTENTO
predictors.tree <-c("a_transport", "a_environment","a_urban", "a_health", "EN.ATM.CO2E.PC","a_energy","NY.GNP.PCAP.CD","SI.DST.FRST.20","DT.ODA.ODAT.PC.ZS")
model.tree<-as.formula(paste0("response.binary","~",paste(predictors,collapse="+")))
head(model.tree)
fullmodel<-glm(model.tree, data=data.model, family=binomial)
summary(fullmodel)

#Construcción del árbol final
library (MASS)
library(tree)
set.seed (55555)
train <- sample (1: nrow(data.model), 23)
train
arbolreto <-tree(model.tree,data,subset =train)
summary (arbolreto)
plot(arbolreto)
text(arbolreto,pretty =0)


#prune tree
cv.arbolreto <- cv.tree(arbolreto)
cv.arbolreto

#which is the best tree?

prune.arbolreto <- prune.misclass(arbolreto,best =2)
plot(prune.arbolreto)
text(prune.arbolreto,pretty =0)

