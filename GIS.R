library("ggplot2")
gis <- read.csv2("GIS13445.csv",header = TRUE,sep = ",")
K <- gis[1:779,]
KR <- gis[780:1148,]
KT <- gis[1149:1312,] 
###############Temperature###############
KTemperature <- K[K$Parameter == 'Temperature',]
KTemperatureSample=data.frame(cbind(KTemperature$First.Sample,KTemperature$Second..Sample,KTemperature$Third.Sample,KTemperature$Fourth.Sample,KTemperature$Fifth.Sample))
names(KTemperatureSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTemperatureSample$First <- as.numeric(KTemperatureSample$First)
KTemperatureSample$Second <- as.numeric(KTemperatureSample$Second)
KTemperatureSample$Third <- as.numeric(KTemperatureSample$Third)
KTemperatureSample$Fourth <- as.numeric(KTemperatureSample$Fourth)
KTemperatureSample$Fifth <- as.numeric(KTemperatureSample$Fifth)
MeanKTemperature<-c()
for (i in 1:5) {
  MeanKTemperature[i]=mean(KTemperatureSample[,i])
}
require(gridExtra)
KTemperatureggplot <- ggplot(data=data.frame(MeanKTemperature)
                             ,aes(x=c(1:5),y=MeanKTemperature))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Temperature (C)')+
  ggtitle("K")+
  ylim(10,30)
#geom_hline(yintercept=KTemperature$Drinking.Standard)
KRTemperature <- KR[KR$Parameter == 'Temperature',]
KRTemperatureSample=data.frame(cbind(KRTemperature$First.Sample,KRTemperature$Second..Sample,KRTemperature$Third.Sample,KRTemperature$Fourth.Sample,KRTemperature$Fifth.Sample))
names(KRTemperatureSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRTemperatureSample$First <- as.numeric(KRTemperatureSample$First)
KRTemperatureSample$Second <- as.numeric(KRTemperatureSample$Second)
KRTemperatureSample$Third <- as.numeric(KRTemperatureSample$Third)
KRTemperatureSample$Fourth <- as.numeric(KRTemperatureSample$Fourth)
KRTemperatureSample$Fifth <- as.numeric(KRTemperatureSample$Fifth)
MeanKRTemperature<-c()
for (i in 1:5) {
  MeanKRTemperature[i]=mean(KRTemperatureSample[,i])
}
KRTemperatureggplot <- ggplot(data=data.frame(MeanKRTemperature)
                              ,aes(x=c(1:5),y=MeanKRTemperature))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Temperature (C)')+
  ggtitle("KR")+
  ylim(10,30)
#geom_hline(yintercept=KRTemperature$DrinKRing.Standard)
KTTemperature <- KT[KT$Parameter == 'Temperature',]
KTTemperatureSample=data.frame(cbind(KTTemperature$First.Sample,KTTemperature$Second..Sample,KTTemperature$Third.Sample,KTTemperature$Fourth.Sample,KTTemperature$Fifth.Sample))
names(KTTemperatureSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTTemperatureSample$First <- as.numeric(KTTemperatureSample$First)
KTTemperatureSample$Second <- as.numeric(KTTemperatureSample$Second)
KTTemperatureSample$Third <- as.numeric(KTTemperatureSample$Third)
KTTemperatureSample$Fourth <- as.numeric(KTTemperatureSample$Fourth)
KTTemperatureSample$Fifth <- as.numeric(KTTemperatureSample$Fifth)
MeanKTTemperature<-c()
for (i in 1:5) {
  MeanKTTemperature[i]=mean(KTTemperatureSample[,i])
}
KTTemperatureggplot <- ggplot(data=data.frame(MeanKTTemperature)
                              ,aes(x=c(1:5),y=MeanKTTemperature))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Temperature (C)')+
  ggtitle("KT")+
  ylim(10,30)
#geom_hline(yintercept=KTTemperature$DrinKTing.Standard)
grid.arrange(KTemperatureggplot,KRTemperatureggplot,KTTemperatureggplot,nrow=1)
###############Opacity###############
KOpacity <- K[K$Parameter == 'Opacity',]
KOpacitySample=data.frame(cbind(KOpacity$First.Sample,KOpacity$Second..Sample,KOpacity$Third.Sample,KOpacity$Fourth.Sample,KOpacity$Fifth.Sample))
names(KOpacitySample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KOpacitySample$First <- as.numeric(KOpacitySample$First)
KOpacitySample$Second <- as.numeric(KOpacitySample$Second)
KOpacitySample$Third <- as.numeric(KOpacitySample$Third)
KOpacitySample$Fourth <- as.numeric(KOpacitySample$Fourth)
KOpacitySample$Fifth <- as.numeric(KOpacitySample$Fifth)
MeanKOpacity<-c()
for (i in 1:5) {
  MeanKOpacity[i]=mean(KOpacitySample[,i])
}
require(gridExtra)
KOpacityggplot <- ggplot(data=data.frame(MeanKOpacity)
                         ,aes(x=c(1:5),y=MeanKOpacity))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Opacity (NTU)')+
  ggtitle("K")+
  ylim(0,70)+
  geom_hline(yintercept=50,color="ORANGE")+
  geom_hline(yintercept=1,color="PURPLE")+
  geom_hline(yintercept=5,color="PURPLE")
KROpacity <- KR[KR$Parameter == 'Opacity',]
KROpacitySample=data.frame(cbind(KROpacity$First.Sample,KROpacity$Second..Sample,KROpacity$Third.Sample,KROpacity$Fourth.Sample,KROpacity$Fifth.Sample))
names(KROpacitySample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KROpacitySample$First <- as.numeric(KROpacitySample$First)
KROpacitySample$Second <- as.numeric(KROpacitySample$Second)
KROpacitySample$Third <- as.numeric(KROpacitySample$Third)
KROpacitySample$Fourth <- as.numeric(KROpacitySample$Fourth)
KROpacitySample$Fifth <- as.numeric(KROpacitySample$Fifth)
MeanKROpacity<-c()
for (i in 1:5) {
  MeanKROpacity[i]=mean(KROpacitySample[,i])
}
KROpacityggplot <- ggplot(data=data.frame(MeanKROpacity)
                          ,aes(x=c(1:5),y=MeanKROpacity))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Opacity (NTU)')+
  ggtitle("KR")+
  ylim(0,70)+
  geom_hline(yintercept=50,color="ORANGE")+
  geom_hline(yintercept=1,color="PURPLE")+
  geom_hline(yintercept=5,color="PURPLE")
KTOpacity <- KT[KT$Parameter == 'Opacity',]
KTOpacitySample=data.frame(cbind(KTOpacity$First.Sample,KTOpacity$Second..Sample,KTOpacity$Third.Sample,KTOpacity$Fourth.Sample,KTOpacity$Fifth.Sample))
names(KTOpacitySample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTOpacitySample$First <- as.numeric(KTOpacitySample$First)
KTOpacitySample$Second <- as.numeric(KTOpacitySample$Second)
KTOpacitySample$Third <- as.numeric(KTOpacitySample$Third)
KTOpacitySample$Fourth <- as.numeric(KTOpacitySample$Fourth)
KTOpacitySample$Fifth <- as.numeric(KTOpacitySample$Fifth)
MeanKTOpacity<-c()
for (i in 1:5) {
  MeanKTOpacity[i]=mean(KTOpacitySample[,i])
}
KTOpacityggplot <- ggplot(data=data.frame(MeanKTOpacity)
                          ,aes(x=c(1:5),y=MeanKTOpacity))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Opacity (NTU)')+
  ggtitle("KT")+
  ylim(0,70)+
  geom_hline(yintercept=50,color="ORANGE")+
  geom_hline(yintercept=1,color="PURPLE")+
  geom_hline(yintercept=5,color="PURPLE")

grid.arrange(KOpacityggplot,KROpacityggplot,KTOpacityggplot,nrow=1)
###############OxygenSolution###############
KOxygenSolution <- K[K$Parameter == 'Oxygen Solution',]
KOxygenSolutionSample=data.frame(cbind(KOxygenSolution$First.Sample,KOxygenSolution$Second..Sample,KOxygenSolution$Third.Sample,KOxygenSolution$Fourth.Sample,KOxygenSolution$Fifth.Sample))
names(KOxygenSolutionSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KOxygenSolutionSample$First <- as.numeric(KOxygenSolutionSample$First)
KOxygenSolutionSample$Second <- as.numeric(KOxygenSolutionSample$Second)
KOxygenSolutionSample$Third <- as.numeric(KOxygenSolutionSample$Third)
KOxygenSolutionSample$Fourth <- as.numeric(KOxygenSolutionSample$Fourth)
KOxygenSolutionSample$Fifth <- as.numeric(KOxygenSolutionSample$Fifth)
MeanKOxygenSolution<-c()
for (i in 1:5) {
  MeanKOxygenSolution[i]=mean(KOxygenSolutionSample[,i])
}
require(gridExtra)
KOxygenSolutionggplot <- ggplot(data=data.frame(MeanKOxygenSolution)
                                ,aes(x=c(1:5),y=MeanKOxygenSolution))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of OxygenSolution (PPM)')+
  ggtitle("K")+
  ylim(0,10)+
  geom_hline(yintercept=2,color="ORANGE")
  KROxygenSolution <- KR[KR$Parameter == 'Oxygen Solution',]
KROxygenSolutionSample=data.frame(cbind(KROxygenSolution$First.Sample,KROxygenSolution$Second..Sample,KROxygenSolution$Third.Sample,KROxygenSolution$Fourth.Sample,KROxygenSolution$Fifth.Sample))
names(KROxygenSolutionSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KROxygenSolutionSample$First <- as.numeric(KROxygenSolutionSample$First)
KROxygenSolutionSample$Second <- as.numeric(KROxygenSolutionSample$Second)
KROxygenSolutionSample$Third <- as.numeric(KROxygenSolutionSample$Third)
KROxygenSolutionSample$Fourth <- as.numeric(KROxygenSolutionSample$Fourth)
KROxygenSolutionSample$Fifth <- as.numeric(KROxygenSolutionSample$Fifth)
MeanKROxygenSolution<-c()
for (i in 1:5) {
  MeanKROxygenSolution[i]=mean(KROxygenSolutionSample[,i])
}
KROxygenSolutionggplot <- ggplot(data=data.frame(MeanKROxygenSolution)
                                 ,aes(x=c(1:5),y=MeanKROxygenSolution))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of OxygenSolution (PPM)')+
  ggtitle("KR")+
  ylim(0,10)+
  geom_hline(yintercept=2,color="ORANGE")
  KTOxygenSolution <- KT[KT$Parameter == 'Oxygen Solution',]
KTOxygenSolutionSample=data.frame(cbind(KTOxygenSolution$First.Sample,KTOxygenSolution$Second..Sample,KTOxygenSolution$Third.Sample,KTOxygenSolution$Fourth.Sample,KTOxygenSolution$Fifth.Sample))
names(KTOxygenSolutionSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTOxygenSolutionSample$First <- as.numeric(KTOxygenSolutionSample$First)
KTOxygenSolutionSample$Second <- as.numeric(KTOxygenSolutionSample$Second)
KTOxygenSolutionSample$Third <- as.numeric(KTOxygenSolutionSample$Third)
KTOxygenSolutionSample$Fourth <- as.numeric(KTOxygenSolutionSample$Fourth)
KTOxygenSolutionSample$Fifth <- as.numeric(KTOxygenSolutionSample$Fifth)
MeanKTOxygenSolution<-c()
for (i in 1:5) {
  MeanKTOxygenSolution[i]=mean(KTOxygenSolutionSample[,i])
}
KTOxygenSolutionggplot <- ggplot(data=data.frame(MeanKTOxygenSolution)
                                 ,aes(x=c(1:5),y=MeanKTOxygenSolution))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of OxygenSolution (PPM)')+
  ggtitle("KT")+
  ylim(0,10)+
  geom_hline(yintercept=2,color="ORANGE")
grid.arrange(KOxygenSolutionggplot,KROxygenSolutionggplot,KTOxygenSolutionggplot,nrow=1)
###############TotalSolubleSolids###############
KTotalSolubleSolids <- K[K$Parameter == 'Total Soluble Solids',]
KTotalSolubleSolidsSample=data.frame(cbind(KTotalSolubleSolids$First.Sample,KTotalSolubleSolids$Second..Sample,KTotalSolubleSolids$Third.Sample,KTotalSolubleSolids$Fourth.Sample,KTotalSolubleSolids$Fifth.Sample))
names(KTotalSolubleSolidsSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTotalSolubleSolidsSample$First <- as.numeric(KTotalSolubleSolidsSample$First)
KTotalSolubleSolidsSample$Second <- as.numeric(KTotalSolubleSolidsSample$Second)
KTotalSolubleSolidsSample$Third <- as.numeric(KTotalSolubleSolidsSample$Third)
KTotalSolubleSolidsSample$Fourth <- as.numeric(KTotalSolubleSolidsSample$Fourth)
KTotalSolubleSolidsSample$Fifth <- as.numeric(KTotalSolubleSolidsSample$Fifth)
MeanKTotalSolubleSolids<-c()
for (i in 1:5) {
  MeanKTotalSolubleSolids[i]=mean(KTotalSolubleSolidsSample[,i])
}
require(gridExtra)
KTotalSolubleSolidsggplot <- ggplot(data=data.frame(MeanKTotalSolubleSolids)
                                    ,aes(x=c(1:5),y=MeanKTotalSolubleSolids))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of TotalSolubleSolids (PPM)')+
  ggtitle("K")+
  ylim(0,2000)+
  geom_hline(yintercept=1000,color="PURPLE")+
  geom_hline(yintercept=1500,color="PURPLE")
KRTotalSolubleSolids <- KR[KR$Parameter == 'Total Soluble Solids',]
KRTotalSolubleSolidsSample=data.frame(cbind(KRTotalSolubleSolids$First.Sample,KRTotalSolubleSolids$Second..Sample,KRTotalSolubleSolids$Third.Sample,KRTotalSolubleSolids$Fourth.Sample,KRTotalSolubleSolids$Fifth.Sample))
names(KRTotalSolubleSolidsSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRTotalSolubleSolidsSample$First <- as.numeric(KRTotalSolubleSolidsSample$First)
KRTotalSolubleSolidsSample$Second <- as.numeric(KRTotalSolubleSolidsSample$Second)
KRTotalSolubleSolidsSample$Third <- as.numeric(KRTotalSolubleSolidsSample$Third)
KRTotalSolubleSolidsSample$Fourth <- as.numeric(KRTotalSolubleSolidsSample$Fourth)
KRTotalSolubleSolidsSample$Fifth <- as.numeric(KRTotalSolubleSolidsSample$Fifth)
MeanKRTotalSolubleSolids<-c()
for (i in 1:5) {
  MeanKRTotalSolubleSolids[i]=mean(KRTotalSolubleSolidsSample[,i])
}
KRTotalSolubleSolidsggplot <- ggplot(data=data.frame(MeanKRTotalSolubleSolids)
                                     ,aes(x=c(1:5),y=MeanKRTotalSolubleSolids))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of TotalSolubleSolids (PPM)')+
  ggtitle("KR")+
  ylim(0,2000)+
  geom_hline(yintercept=1000,color="PURPLE")+
  geom_hline(yintercept=1500,color="PURPLE")
KTTotalSolubleSolids <- KT[KT$Parameter == 'Total Soluble Solids',]
KTTotalSolubleSolidsSample=data.frame(cbind(KTTotalSolubleSolids$First.Sample,KTTotalSolubleSolids$Second..Sample,KTTotalSolubleSolids$Third.Sample,KTTotalSolubleSolids$Fourth.Sample,KTTotalSolubleSolids$Fifth.Sample))
names(KTTotalSolubleSolidsSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTTotalSolubleSolidsSample$First <- as.numeric(KTTotalSolubleSolidsSample$First)
KTTotalSolubleSolidsSample$Second <- as.numeric(KTTotalSolubleSolidsSample$Second)
KTTotalSolubleSolidsSample$Third <- as.numeric(KTTotalSolubleSolidsSample$Third)
KTTotalSolubleSolidsSample$Fourth <- as.numeric(KTTotalSolubleSolidsSample$Fourth)
KTTotalSolubleSolidsSample$Fifth <- as.numeric(KTTotalSolubleSolidsSample$Fifth)
MeanKTTotalSolubleSolids<-c()
for (i in 1:5) {
  MeanKTTotalSolubleSolids[i]=mean(KTTotalSolubleSolidsSample[,i])
}
KTTotalSolubleSolidsggplot <- ggplot(data=data.frame(MeanKTTotalSolubleSolids)
                                     ,aes(x=c(1:5),y=MeanKTTotalSolubleSolids))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of TotalSolubleSolids (PPM)')+
  ggtitle("KT")+
  ylim(0,2000)+
  geom_hline(yintercept=1000,color="PURPLE")+
  geom_hline(yintercept=1500,color="PURPLE")
grid.arrange(KTotalSolubleSolidsggplot,KRTotalSolubleSolidsggplot,KTTotalSolubleSolidsggplot,nrow=1)
###############pH###############
KpH <- K[K$Parameter == 'pH',]
KpHSample=data.frame(cbind(KpH$First.Sample,KpH$Second..Sample,KpH$Third.Sample,KpH$Fourth.Sample,KpH$Fifth.Sample))
names(KpHSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KpHSample$First <- as.numeric(KpHSample$First)
KpHSample$Second <- as.numeric(KpHSample$Second)
KpHSample$Third <- as.numeric(KpHSample$Third)
KpHSample$Fourth <- as.numeric(KpHSample$Fourth)
KpHSample$Fifth <- as.numeric(KpHSample$Fifth)
MeanKpH<-c()
for (i in 1:5) {
  MeanKpH[i]=mean(KpHSample[,i])
}
require(gridExtra)
KpHggplot <- ggplot(data=data.frame(MeanKpH)
                    ,aes(x=c(1:5),y=MeanKpH))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of pH (-)')+
  ggtitle("K")+
  ylim(0,10)+
  geom_hline(yintercept=6.5,color="PURPLE")+
  geom_hline(yintercept=8.5,color="PURPLE")+
  geom_hline(yintercept=9,color="PURPLE")+
  geom_hline(yintercept=5.4,color="ORANGE")+
  geom_hline(yintercept=6.8,color="ORANGE")
KRpH <- KR[KR$Parameter == 'pH',]
KRpHSample=data.frame(cbind(KRpH$First.Sample,KRpH$Second..Sample,KRpH$Third.Sample,KRpH$Fourth.Sample,KRpH$Fifth.Sample))
names(KRpHSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRpHSample$First <- as.numeric(KRpHSample$First)
KRpHSample$Second <- as.numeric(KRpHSample$Second)
KRpHSample$Third <- as.numeric(KRpHSample$Third)
KRpHSample$Fourth <- as.numeric(KRpHSample$Fourth)
KRpHSample$Fifth <- as.numeric(KRpHSample$Fifth)
MeanKRpH<-c()
for (i in 1:5) {
  MeanKRpH[i]=mean(KRpHSample[,i])
}
KRpHggplot <- ggplot(data=data.frame(MeanKRpH)
                     ,aes(x=c(1:5),y=MeanKRpH))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of pH (-)')+
  ggtitle("KR")+
  ylim(0,10)+
  geom_hline(yintercept=6.5,color="PURPLE")+
  geom_hline(yintercept=8.5,color="PURPLE")+
  geom_hline(yintercept=9,color="PURPLE")+
  geom_hline(yintercept=5.4,color="ORANGE")+
  geom_hline(yintercept=6.8,color="ORANGE")
KTpH <- KT[KT$Parameter == 'pH',]
KTpHSample=data.frame(cbind(KTpH$First.Sample,KTpH$Second..Sample,KTpH$Third.Sample,KTpH$Fourth.Sample,KTpH$Fifth.Sample))
names(KTpHSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTpHSample$First <- as.numeric(KTpHSample$First)
KTpHSample$Second <- as.numeric(KTpHSample$Second)
KTpHSample$Third <- as.numeric(KTpHSample$Third)
KTpHSample$Fourth <- as.numeric(KTpHSample$Fourth)
KTpHSample$Fifth <- as.numeric(KTpHSample$Fifth)
MeanKTpH<-c()
for (i in 1:5) {
  MeanKTpH[i]=mean(KTpHSample[,i])
}
KTpHggplot <- ggplot(data=data.frame(MeanKTpH)
                     ,aes(x=c(1:5),y=MeanKTpH))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of pH (-)')+
  ggtitle("KT")+
  ylim(0,10)+
  geom_hline(yintercept=6.5,color="PURPLE")+
  geom_hline(yintercept=8.5,color="PURPLE")+
  geom_hline(yintercept=9,color="PURPLE")+
  geom_hline(yintercept=5.4,color="ORANGE")+
  geom_hline(yintercept=6.8,color="ORANGE")
grid.arrange(KpHggplot,KRpHggplot,KTpHggplot,nrow=1)
###############ElectricalConductivity###############
KElectricalConductivity <- K[K$Parameter == 'Electrical Conductivity',]
KElectricalConductivitySample=data.frame(cbind(KElectricalConductivity$First.Sample,KElectricalConductivity$Second..Sample,KElectricalConductivity$Third.Sample,KElectricalConductivity$Fourth.Sample,KElectricalConductivity$Fifth.Sample))
names(KElectricalConductivitySample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KElectricalConductivitySample$First <- as.numeric(KElectricalConductivitySample$First)
KElectricalConductivitySample$Second <- as.numeric(KElectricalConductivitySample$Second)
KElectricalConductivitySample$Third <- as.numeric(KElectricalConductivitySample$Third)
KElectricalConductivitySample$Fourth <- as.numeric(KElectricalConductivitySample$Fourth)
KElectricalConductivitySample$Fifth <- as.numeric(KElectricalConductivitySample$Fifth)
MeanKElectricalConductivity<-c()
for (i in 1:5) {
  MeanKElectricalConductivity[i]=mean(KElectricalConductivitySample[,i])
}
require(gridExtra)
KElectricalConductivityggplot <- ggplot(data=data.frame(MeanKElectricalConductivity)
                                        ,aes(x=c(1:5),y=MeanKElectricalConductivity))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Electrical Conductivity (mS.cm-1)')+
  ggtitle("K")+
  ylim(0,3.5)+
  geom_hline(yintercept=3,color="ORANGE")+
  geom_hline(yintercept=1.5,color="PURPLE")
KRElectricalConductivity <- KR[KR$Parameter == 'Electrical Conductivity',]
KRElectricalConductivitySample=data.frame(cbind(KRElectricalConductivity$First.Sample,KRElectricalConductivity$Second..Sample,KRElectricalConductivity$Third.Sample,KRElectricalConductivity$Fourth.Sample,KRElectricalConductivity$Fifth.Sample))
names(KRElectricalConductivitySample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRElectricalConductivitySample$First <- as.numeric(KRElectricalConductivitySample$First)
KRElectricalConductivitySample$Second <- as.numeric(KRElectricalConductivitySample$Second)
KRElectricalConductivitySample$Third <- as.numeric(KRElectricalConductivitySample$Third)
KRElectricalConductivitySample$Fourth <- as.numeric(KRElectricalConductivitySample$Fourth)
KRElectricalConductivitySample$Fifth <- as.numeric(KRElectricalConductivitySample$Fifth)
MeanKRElectricalConductivity<-c()
for (i in 1:5) {
  MeanKRElectricalConductivity[i]=mean(KRElectricalConductivitySample[,i])
}
KRElectricalConductivityggplot <- ggplot(data=data.frame(MeanKRElectricalConductivity)
                                         ,aes(x=c(1:5),y=MeanKRElectricalConductivity))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Electrical Conductivity (mS.cm-1)')+
  ggtitle("KR")+
  ylim(0,3.5)+
  geom_hline(yintercept=3,color="ORANGE")+
  geom_hline(yintercept=1.5,color="PURPLE")
KTElectricalConductivity <- KT[KT$Parameter == 'Electrical Conductivity',]
KTElectricalConductivitySample=data.frame(cbind(KTElectricalConductivity$First.Sample,KTElectricalConductivity$Second..Sample,KTElectricalConductivity$Third.Sample,KTElectricalConductivity$Fourth.Sample,KTElectricalConductivity$Fifth.Sample))
names(KTElectricalConductivitySample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTElectricalConductivitySample$First <- as.numeric(KTElectricalConductivitySample$First)
KTElectricalConductivitySample$Second <- as.numeric(KTElectricalConductivitySample$Second)
KTElectricalConductivitySample$Third <- as.numeric(KTElectricalConductivitySample$Third)
KTElectricalConductivitySample$Fourth <- as.numeric(KTElectricalConductivitySample$Fourth)
KTElectricalConductivitySample$Fifth <- as.numeric(KTElectricalConductivitySample$Fifth)
MeanKTElectricalConductivity<-c()
for (i in 1:5) {
  MeanKTElectricalConductivity[i]=mean(KTElectricalConductivitySample[,i])
}
KTElectricalConductivityggplot <- ggplot(data=data.frame(MeanKTElectricalConductivity)
                                         ,aes(x=c(1:5),y=MeanKTElectricalConductivity))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of ElectricalConductivity (mS.cm-1)')+
  ggtitle("KT")+
  ylim(0,3.5)+
  geom_hline(yintercept=3,color="ORANGE")+
  geom_hline(yintercept=1.5,color="PURPLE")
grid.arrange(KElectricalConductivityggplot,KRElectricalConductivityggplot,KTElectricalConductivityggplot,nrow=1)
###############TOC###############
KTOC <- K[K$Parameter == 'TOC',]
KTOCSample=data.frame(cbind(KTOC$First.Sample,KTOC$Second..Sample,KTOC$Third.Sample,KTOC$Fourth.Sample,KTOC$Fifth.Sample))
names(KTOCSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTOCSample$First <- as.numeric(KTOCSample$First)
KTOCSample$Second <- as.numeric(KTOCSample$Second)
KTOCSample$Third <- as.numeric(KTOCSample$Third)
KTOCSample$Fourth <- as.numeric(KTOCSample$Fourth)
KTOCSample$Fifth <- as.numeric(KTOCSample$Fifth)
MeanKTOC<-c()
for (i in 1:5) {
  MeanKTOC[i]=mean(KTOCSample[,i])
}
require(gridExtra)
KTOCggplot <- ggplot(data=data.frame(MeanKTOC)
                     ,aes(x=c(1:5),y=MeanKTOC))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of TOC (PPM)')+
  ggtitle("K")+
  ylim(0,54)+
  geom_hline(yintercept=2,color="ORANGE")+
  geom_hline(yintercept=4,color="PURPLE")
KRTOC <- KR[KR$Parameter == 'TOC',]
KRTOCSample=data.frame(cbind(KRTOC$First.Sample,KRTOC$Second..Sample,KRTOC$Third.Sample,KRTOC$Fourth.Sample,KRTOC$Fifth.Sample))
names(KRTOCSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRTOCSample$First <- as.numeric(KRTOCSample$First)
KRTOCSample$Second <- as.numeric(KRTOCSample$Second)
KRTOCSample$Third <- as.numeric(KRTOCSample$Third)
KRTOCSample$Fourth <- as.numeric(KRTOCSample$Fourth)
KRTOCSample$Fifth <- as.numeric(KRTOCSample$Fifth)
MeanKRTOC<-c()
for (i in 1:5) {
  MeanKRTOC[i]=mean(KRTOCSample[,i])
}
KRTOCggplot <- ggplot(data=data.frame(MeanKRTOC)
                      ,aes(x=c(1:5),y=MeanKRTOC))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of TOC (PPM)')+
  ggtitle("KR")+
  ylim(0,54)+
  geom_hline(yintercept=2,color="ORANGE")+
  geom_hline(yintercept=4,color="PURPLE")
KTTOC <- KT[KT$Parameter == 'TOC',]
KTTOCSample=data.frame(cbind(KTTOC$First.Sample,KTTOC$Second..Sample,KTTOC$Third.Sample,KTTOC$Fourth.Sample,KTTOC$Fifth.Sample))
names(KTTOCSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTTOCSample$First <- as.numeric(KTTOCSample$First)
KTTOCSample$Second <- as.numeric(KTTOCSample$Second)
KTTOCSample$Third <- as.numeric(KTTOCSample$Third)
KTTOCSample$Fourth <- as.numeric(KTTOCSample$Fourth)
KTTOCSample$Fifth <- as.numeric(KTTOCSample$Fifth)
MeanKTTOC<-c()
for (i in 1:5) {
  MeanKTTOC[i]=mean(KTTOCSample[,i])
}
KTTOCggplot <- ggplot(data=data.frame(MeanKTTOC)
                      ,aes(x=c(1:5),y=MeanKTTOC))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of TOC (PPM)')+
  ggtitle("KT")+
  ylim(0,54)+
  geom_hline(yintercept=2,color="ORANGE")+
  geom_hline(yintercept=4,color="PURPLE")
grid.arrange(KTOCggplot,KRTOCggplot,KTTOCggplot,nrow=1)
###############Ag###############
KAg <- K[K$Parameter == 'Ag',]
KAgSample=data.frame(cbind(KAg$First.Sample,KAg$Second..Sample,KAg$Third.Sample,KAg$Fourth.Sample,KAg$Fifth.Sample))
names(KAgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KAgSample$First <- as.numeric(KAgSample$First)
KAgSample$Second <- as.numeric(KAgSample$Second)
KAgSample$Third <- as.numeric(KAgSample$Third)
KAgSample$Fourth <- as.numeric(KAgSample$Fourth)
KAgSample$Fifth <- as.numeric(KAgSample$Fifth)
MeanKAg<-c()
for (i in 1:5) {
  MeanKAg[i]=mean(KAgSample[,i])
}
require(gridExtra)
KAgggplot <- ggplot(data=data.frame(MeanKAg)
                    ,aes(x=c(1:5),y=MeanKAg))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Ag (PPM)')+
  ggtitle("K")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")
KRAg <- KR[KR$Parameter == 'Ag',]
KRAgSample=data.frame(cbind(KRAg$First.Sample,KRAg$Second..Sample,KRAg$Third.Sample,KRAg$Fourth.Sample,KRAg$Fifth.Sample))
names(KRAgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRAgSample$First <- as.numeric(KRAgSample$First)
KRAgSample$Second <- as.numeric(KRAgSample$Second)
KRAgSample$Third <- as.numeric(KRAgSample$Third)
KRAgSample$Fourth <- as.numeric(KRAgSample$Fourth)
KRAgSample$Fifth <- as.numeric(KRAgSample$Fifth)
MeanKRAg<-c()
for (i in 1:5) {
  MeanKRAg[i]=mean(KRAgSample[,i])
}
KRAgggplot <- ggplot(data=data.frame(MeanKRAg)
                     ,aes(x=c(1:5),y=MeanKRAg))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Ag (PPM)')+
  ggtitle("KR")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")
KTAg <- KT[KT$Parameter == 'Ag',]
KTAgSample=data.frame(cbind(KTAg$First.Sample,KTAg$Second..Sample,KTAg$Third.Sample,KTAg$Fourth.Sample,KTAg$Fifth.Sample))
names(KTAgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTAgSample$First <- as.numeric(KTAgSample$First)
KTAgSample$Second <- as.numeric(KTAgSample$Second)
KTAgSample$Third <- as.numeric(KTAgSample$Third)
KTAgSample$Fourth <- as.numeric(KTAgSample$Fourth)
KTAgSample$Fifth <- as.numeric(KTAgSample$Fifth)
MeanKTAg<-c()
for (i in 1:5) {
  MeanKTAg[i]=mean(KTAgSample[,i])
}
KTAgggplot <- ggplot(data=data.frame(MeanKTAg)
                     ,aes(x=c(1:5),y=MeanKTAg))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Ag (PPM)')+
  ggtitle("KT")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")
grid.arrange(KAgggplot,KRAgggplot,KTAgggplot,nrow=1)
###############Al###############
KAl <- K[K$Parameter == 'Al',]
KAlSample=data.frame(cbind(KAl$First.Sample,KAl$Second..Sample,KAl$Third.Sample,KAl$Fourth.Sample,KAl$Fifth.Sample))
names(KAlSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KAlSample$First <- as.numeric(KAlSample$First)
KAlSample$Second <- as.numeric(KAlSample$Second)
KAlSample$Third <- as.numeric(KAlSample$Third)
KAlSample$Fourth <- as.numeric(KAlSample$Fourth)
KAlSample$Fifth <- as.numeric(KAlSample$Fifth)
MeanKAl<-c()
for (i in 1:5) {
  MeanKAl[i]=mean(KAlSample[,i])
}
require(gridExtra)
KAlggplot <- ggplot(data=data.frame(MeanKAl)
                    ,aes(x=c(1:5),y=MeanKAl))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Al (PPM)')+
  ggtitle("K")+
  ylim(0,5.1)+
  geom_hline(yintercept=5,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")+
  geom_hline(yintercept=0.2,color="PURPLE")
KRAl <- KR[KR$Parameter == 'Al',]
KRAlSample=data.frame(cbind(KRAl$First.Sample,KRAl$Second..Sample,KRAl$Third.Sample,KRAl$Fourth.Sample,KRAl$Fifth.Sample))
names(KRAlSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRAlSample$First <- as.numeric(KRAlSample$First)
KRAlSample$Second <- as.numeric(KRAlSample$Second)
KRAlSample$Third <- as.numeric(KRAlSample$Third)
KRAlSample$Fourth <- as.numeric(KRAlSample$Fourth)
KRAlSample$Fifth <- as.numeric(KRAlSample$Fifth)
MeanKRAl<-c()
for (i in 1:5) {
  MeanKRAl[i]=mean(KRAlSample[,i])
}
KRAlggplot <- ggplot(data=data.frame(MeanKRAl)
                     ,aes(x=c(1:5),y=MeanKRAl))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Al (PPM)')+
  ggtitle("KR")+
  ylim(0,5.1)+
  geom_hline(yintercept=5,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")+
  geom_hline(yintercept=0.2,color="PURPLE")
KTAl <- KT[KT$Parameter == 'Al',]
KTAlSample=data.frame(cbind(KTAl$First.Sample,KTAl$Second..Sample,KTAl$Third.Sample,KTAl$Fourth.Sample,KTAl$Fifth.Sample))
names(KTAlSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTAlSample$First <- as.numeric(KTAlSample$First)
KTAlSample$Second <- as.numeric(KTAlSample$Second)
KTAlSample$Third <- as.numeric(KTAlSample$Third)
KTAlSample$Fourth <- as.numeric(KTAlSample$Fourth)
KTAlSample$Fifth <- as.numeric(KTAlSample$Fifth)
MeanKTAl<-c()
for (i in 1:5) {
  MeanKTAl[i]=mean(KTAlSample[,i])
}
KTAlggplot <- ggplot(data=data.frame(MeanKTAl)
                     ,aes(x=c(1:5),y=MeanKTAl))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Al (PPM)')+
  ggtitle("KT")+
  ylim(0,5.1)+
  geom_hline(yintercept=5,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")+
  geom_hline(yintercept=0.2,color="PURPLE")
grid.arrange(KAlggplot,KRAlggplot,KTAlggplot,nrow=1)
###############As###############
KAs <- K[K$Parameter == 'As',]
KAsSample=data.frame(cbind(KAs$First.Sample,KAs$Second..Sample,KAs$Third.Sample,KAs$Fourth.Sample,KAs$Fifth.Sample))
names(KAsSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KAsSample$First <- as.numeric(KAsSample$First)
KAsSample$Second <- as.numeric(KAsSample$Second)
KAsSample$Third <- as.numeric(KAsSample$Third)
KAsSample$Fourth <- as.numeric(KAsSample$Fourth)
KAsSample$Fifth <- as.numeric(KAsSample$Fifth)
MeanKAs<-c()
for (i in 1:5) {
  MeanKAs[i]=mean(KAsSample[,i])
}
require(gridExtra)
KAsggplot <- ggplot(data=data.frame(MeanKAs)
                    ,aes(x=c(1:5),y=MeanKAs))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of As (PPM)')+
  ggtitle("K")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.01,color="PURPLE")
KRAs <- KR[KR$Parameter == 'As',]
KRAsSample=data.frame(cbind(KRAs$First.Sample,KRAs$Second..Sample,KRAs$Third.Sample,KRAs$Fourth.Sample,KRAs$Fifth.Sample))
names(KRAsSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRAsSample$First <- as.numeric(KRAsSample$First)
KRAsSample$Second <- as.numeric(KRAsSample$Second)
KRAsSample$Third <- as.numeric(KRAsSample$Third)
KRAsSample$Fourth <- as.numeric(KRAsSample$Fourth)
KRAsSample$Fifth <- as.numeric(KRAsSample$Fifth)
MeanKRAs<-c()
for (i in 1:5) {
  MeanKRAs[i]=mean(KRAsSample[,i])
}
KRAsggplot <- ggplot(data=data.frame(MeanKRAs)
                     ,aes(x=c(1:5),y=MeanKRAs))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of As (PPM)')+
  ggtitle("KR")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.01,color="PURPLE")
KTAs <- KT[KT$Parameter == 'As',]
KTAsSample=data.frame(cbind(KTAs$First.Sample,KTAs$Second..Sample,KTAs$Third.Sample,KTAs$Fourth.Sample,KTAs$Fifth.Sample))
names(KTAsSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTAsSample$First <- as.numeric(KTAsSample$First)
KTAsSample$Second <- as.numeric(KTAsSample$Second)
KTAsSample$Third <- as.numeric(KTAsSample$Third)
KTAsSample$Fourth <- as.numeric(KTAsSample$Fourth)
KTAsSample$Fifth <- as.numeric(KTAsSample$Fifth)
MeanKTAs<-c()
for (i in 1:5) {
  MeanKTAs[i]=mean(KTAsSample[,i])
}
KTAsggplot <- ggplot(data=data.frame(MeanKTAs)
                     ,aes(x=c(1:5),y=MeanKTAs))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of As (PPM)')+
  ggtitle("KT")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.01,color="PURPLE")
grid.arrange(KAsggplot,KRAsggplot,KTAsggplot,nrow=1)
###############B###############
KBb <- K[K$Parameter == 'B',]
KBbSample=data.frame(cbind(KBb$First.Sample,KBb$Second..Sample,KBb$Third.Sample,KBb$Fourth.Sample,KBb$Fifth.Sample))
names(KBbSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KBbSample$First <- as.numeric(KBbSample$First)
KBbSample$Second <- as.numeric(KBbSample$Second)
KBbSample$Third <- as.numeric(KBbSample$Third)
KBbSample$Fourth <- as.numeric(KBbSample$Fourth)
KBbSample$Fifth <- as.numeric(KBbSample$Fifth)
MeanKBb<-c()
for (i in 1:5) {
  MeanKBb[i]=mean(KBbSample[,i])
}
require(gridExtra)
KBbggplot <- ggplot(data=data.frame(MeanKBb)
                    ,aes(x=c(1:5),y=MeanKBb))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of B (PPM)')+
  ggtitle("K")+
  ylim(0,12)+
  geom_hline(yintercept=3,color="ORANGE")+
  geom_hline(yintercept=0.5,color="PURPLE")
KRBb <- KR[KR$Parameter == 'B',]
KRBbSample=data.frame(cbind(KRBb$First.Sample,KRBb$Second..Sample,KRBb$Third.Sample,KRBb$Fourth.Sample,KRBb$Fifth.Sample))
names(KRBbSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRBbSample$First <- as.numeric(KRBbSample$First)
KRBbSample$Second <- as.numeric(KRBbSample$Second)
KRBbSample$Third <- as.numeric(KRBbSample$Third)
KRBbSample$Fourth <- as.numeric(KRBbSample$Fourth)
KRBbSample$Fifth <- as.numeric(KRBbSample$Fifth)
MeanKRBb<-c()
for (i in 1:5) {
  MeanKRBb[i]=mean(KRBbSample[,i])
}
KRBbggplot <- ggplot(data=data.frame(MeanKRBb)
                     ,aes(x=c(1:5),y=MeanKRBb))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of B (PPM)')+
  ggtitle("KR")+
  ylim(0,12)+
  geom_hline(yintercept=3,color="ORANGE")+
  geom_hline(yintercept=0.5,color="PURPLE")
KTBb <- KT[KT$Parameter == 'B',]
KTBbSample=data.frame(cbind(KTBb$First.Sample,KTBb$Second..Sample,KTBb$Third.Sample,KTBb$Fourth.Sample,KTBb$Fifth.Sample))
names(KTBbSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTBbSample$First <- as.numeric(KTBbSample$First)
KTBbSample$Second <- as.numeric(KTBbSample$Second)
KTBbSample$Third <- as.numeric(KTBbSample$Third)
KTBbSample$Fourth <- as.numeric(KTBbSample$Fourth)
KTBbSample$Fifth <- as.numeric(KTBbSample$Fifth)
MeanKTBb<-c()
for (i in 1:5) {
  MeanKTBb[i]=mean(KTBbSample[,i])
}
KTBbggplot <- ggplot(data=data.frame(MeanKTBb)
                     ,aes(x=c(1:5),y=MeanKTBb))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of B (PPM)')+
  ggtitle("KT")+
  ylim(0,12)+
  geom_hline(yintercept=3,color="ORANGE")+
  geom_hline(yintercept=0.5,color="PURPLE")
grid.arrange(KBbggplot,KRBbggplot,KTBbggplot,nrow=1)
###############Ba###############
KBa <- K[K$Parameter == 'Ba',]
KBaSample=data.frame(cbind(KBa$First.Sample,KBa$Second..Sample,KBa$Third.Sample,KBa$Fourth.Sample,KBa$Fifth.Sample))
names(KBaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KBaSample$First <- as.numeric(KBaSample$First)
KBaSample$Second <- as.numeric(KBaSample$Second)
KBaSample$Third <- as.numeric(KBaSample$Third)
KBaSample$Fourth <- as.numeric(KBaSample$Fourth)
KBaSample$Fifth <- as.numeric(KBaSample$Fifth)
MeanKBa<-c()
for (i in 1:5) {
  MeanKBa[i]=mean(KBaSample[,i])
}
require(gridExtra)
KBaggplot <- ggplot(data=data.frame(MeanKBa)
                    ,aes(x=c(1:5),y=MeanKBa))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Ba (PPM)')+
  ggtitle("K")+
  ylim(0,1.5)+
  geom_hline(yintercept=1,color="ORANGE")+
  geom_hline(yintercept=0.7,color="PURPLE")
KRBa <- KR[KR$Parameter == 'Ba',]
KRBaSample=data.frame(cbind(KRBa$First.Sample,KRBa$Second..Sample,KRBa$Third.Sample,KRBa$Fourth.Sample,KRBa$Fifth.Sample))
names(KRBaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRBaSample$First <- as.numeric(KRBaSample$First)
KRBaSample$Second <- as.numeric(KRBaSample$Second)
KRBaSample$Third <- as.numeric(KRBaSample$Third)
KRBaSample$Fourth <- as.numeric(KRBaSample$Fourth)
KRBaSample$Fifth <- as.numeric(KRBaSample$Fifth)
MeanKRBa<-c()
for (i in 1:5) {
  MeanKRBa[i]=mean(KRBaSample[,i])
}
KRBaggplot <- ggplot(data=data.frame(MeanKRBa)
                     ,aes(x=c(1:5),y=MeanKRBa))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Ba (PPM)')+
  ggtitle("KR")+
  ylim(0,1.5)+
  geom_hline(yintercept=1,color="ORANGE")+
  geom_hline(yintercept=0.7,color="PURPLE")
KTBa <- KT[KT$Parameter == 'Ba',]
KTBaSample=data.frame(cbind(KTBa$First.Sample,KTBa$Second..Sample,KTBa$Third.Sample,KTBa$Fourth.Sample,KTBa$Fifth.Sample))
names(KTBaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTBaSample$First <- as.numeric(KTBaSample$First)
KTBaSample$Second <- as.numeric(KTBaSample$Second)
KTBaSample$Third <- as.numeric(KTBaSample$Third)
KTBaSample$Fourth <- as.numeric(KTBaSample$Fourth)
KTBaSample$Fifth <- as.numeric(KTBaSample$Fifth)
MeanKTBa<-c()
for (i in 1:5) {
  MeanKTBa[i]=mean(KTBaSample[,i])
}
KTBaggplot <- ggplot(data=data.frame(MeanKTBa)
                     ,aes(x=c(1:5),y=MeanKTBa))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Ba (PPM)')+
  ggtitle("KT")+
  ylim(0,1.5)+
  geom_hline(yintercept=1,color="ORANGE")+
  geom_hline(yintercept=0.7,color="PURPLE")
grid.arrange(KBaggplot,KRBaggplot,KTBaggplot,nrow=1)
###############Bi###############
KBi <- K[K$Parameter == 'Bi',]
KBiSample=data.frame(cbind(KBi$First.Sample,KBi$Second..Sample,KBi$Third.Sample,KBi$Fourth.Sample,KBi$Fifth.Sample))
names(KBiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KBiSample$First <- as.numeric(KBiSample$First)
KBiSample$Second <- as.numeric(KBiSample$Second)
KBiSample$Third <- as.numeric(KBiSample$Third)
KBiSample$Fourth <- as.numeric(KBiSample$Fourth)
KBiSample$Fifth <- as.numeric(KBiSample$Fifth)
MeanKBi<-c()
for (i in 1:5) {
  MeanKBi[i]=mean(KBiSample[,i])
}
require(gridExtra)
KBiggplot <- ggplot(data=data.frame(MeanKBi)
                    ,aes(x=c(1:5),y=MeanKBi))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Bi (PPM)')+
  ggtitle("K")+
  ylim(0,0.6)+
  geom_hline(yintercept=0.5,color="ORANGE")
KRBi <- KR[KR$Parameter == 'Bi',]
KRBiSample=data.frame(cbind(KRBi$First.Sample,KRBi$Second..Sample,KRBi$Third.Sample,KRBi$Fourth.Sample,KRBi$Fifth.Sample))
names(KRBiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRBiSample$First <- as.numeric(KRBiSample$First)
KRBiSample$Second <- as.numeric(KRBiSample$Second)
KRBiSample$Third <- as.numeric(KRBiSample$Third)
KRBiSample$Fourth <- as.numeric(KRBiSample$Fourth)
KRBiSample$Fifth <- as.numeric(KRBiSample$Fifth)
MeanKRBi<-c()
for (i in 1:5) {
  MeanKRBi[i]=mean(KRBiSample[,i])
}
KRBiggplot <- ggplot(data=data.frame(MeanKRBi)
                     ,aes(x=c(1:5),y=MeanKRBi))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Bi (PPM)')+
  ggtitle("KR")+
  ylim(0,0.6)+
  geom_hline(yintercept=0.5,color="ORANGE")
KTBi <- KT[KT$Parameter == 'Bi',]
KTBiSample=data.frame(cbind(KTBi$First.Sample,KTBi$Second..Sample,KTBi$Third.Sample,KTBi$Fourth.Sample,KTBi$Fifth.Sample))
names(KTBiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTBiSample$First <- as.numeric(KTBiSample$First)
KTBiSample$Second <- as.numeric(KTBiSample$Second)
KTBiSample$Third <- as.numeric(KTBiSample$Third)
KTBiSample$Fourth <- as.numeric(KTBiSample$Fourth)
KTBiSample$Fifth <- as.numeric(KTBiSample$Fifth)
MeanKTBi<-c()
for (i in 1:5) {
  MeanKTBi[i]=mean(KTBiSample[,i])
}
KTBiggplot <- ggplot(data=data.frame(MeanKTBi)
                     ,aes(x=c(1:5),y=MeanKTBi))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Bi (PPM)')+
  ggtitle("KT")+
  ylim(0,0.6)+
  geom_hline(yintercept=0.5,color="ORANGE")
grid.arrange(KBiggplot,KRBiggplot,KTBiggplot,nrow=1)
###############Ca###############
KCa <- K[K$Parameter == 'Ca',]
KCaSample=data.frame(cbind(KCa$First.Sample,KCa$Second..Sample,KCa$Third.Sample,KCa$Fourth.Sample,KCa$Fifth.Sample))
names(KCaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KCaSample$First <- as.numeric(KCaSample$First)
KCaSample$Second <- as.numeric(KCaSample$Second)
KCaSample$Third <- as.numeric(KCaSample$Third)
KCaSample$Fourth <- as.numeric(KCaSample$Fourth)
KCaSample$Fifth <- as.numeric(KCaSample$Fifth)
MeanKCa<-c()
for (i in 1:5) {
  MeanKCa[i]=mean(KCaSample[,i])
}
require(gridExtra)
KCaggplot <- ggplot(data=data.frame(MeanKCa)
                    ,aes(x=c(1:5),y=MeanKCa))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Ca (PPM)')+
  ggtitle("K")+
  ylim(0,400)+
  geom_hline(yintercept=300,color="PURPLE")
KRCa <- KR[KR$Parameter == 'Ca',]
KRCaSample=data.frame(cbind(KRCa$First.Sample,KRCa$Second..Sample,KRCa$Third.Sample,KRCa$Fourth.Sample,KRCa$Fifth.Sample))
names(KRCaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRCaSample$First <- as.numeric(KRCaSample$First)
KRCaSample$Second <- as.numeric(KRCaSample$Second)
KRCaSample$Third <- as.numeric(KRCaSample$Third)
KRCaSample$Fourth <- as.numeric(KRCaSample$Fourth)
KRCaSample$Fifth <- as.numeric(KRCaSample$Fifth)
MeanKRCa<-c()
for (i in 1:5) {
  MeanKRCa[i]=mean(KRCaSample[,i])
}
KRCaggplot <- ggplot(data=data.frame(MeanKRCa)
                     ,aes(x=c(1:5),y=MeanKRCa))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Ca (PPM)')+
  ggtitle("KR")+
  ylim(0,400)+
  geom_hline(yintercept=300,color="PURPLE")
KTCa <- KT[KT$Parameter == 'Ca',]
KTCaSample=data.frame(cbind(KTCa$First.Sample,KTCa$Second..Sample,KTCa$Third.Sample,KTCa$Fourth.Sample,KTCa$Fifth.Sample))
names(KTCaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTCaSample$First <- as.numeric(KTCaSample$First)
KTCaSample$Second <- as.numeric(KTCaSample$Second)
KTCaSample$Third <- as.numeric(KTCaSample$Third)
KTCaSample$Fourth <- as.numeric(KTCaSample$Fourth)
KTCaSample$Fifth <- as.numeric(KTCaSample$Fifth)
MeanKTCa<-c()
for (i in 1:5) {
  MeanKTCa[i]=mean(KTCaSample[,i])
}
KTCaggplot <- ggplot(data=data.frame(MeanKTCa)
                     ,aes(x=c(1:5),y=MeanKTCa))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Ca (PPM)')+
  ggtitle("KT")+
  ylim(0,400)+
  geom_hline(yintercept=300,color="PURPLE")
grid.arrange(KCaggplot,KRCaggplot,KTCaggplot,nrow=1)
###############Cd###############
KCd <- K[K$Parameter == 'Cd',]
KCdSample=data.frame(cbind(KCd$First.Sample,KCd$Second..Sample,KCd$Third.Sample,KCd$Fourth.Sample,KCd$Fifth.Sample))
names(KCdSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KCdSample$First <- as.numeric(KCdSample$First)
KCdSample$Second <- as.numeric(KCdSample$Second)
KCdSample$Third <- as.numeric(KCdSample$Third)
KCdSample$Fourth <- as.numeric(KCdSample$Fourth)
KCdSample$Fifth <- as.numeric(KCdSample$Fifth)
MeanKCd<-c()
for (i in 1:5) {
  MeanKCd[i]=mean(KCdSample[,i])
}
require(gridExtra)
KCdggplot <- ggplot(data=data.frame(MeanKCd)
                    ,aes(x=c(1:5),y=MeanKCd))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Cd (PPM)')+
  ggtitle("K")+
  ylim(0,5)+
  geom_hline(yintercept=0.01,color="ORANGE")+
  geom_hline(yintercept=3,color="PURPLE")
KRCd <- KR[KR$Parameter == 'Cd',]
KRCdSample=data.frame(cbind(KRCd$First.Sample,KRCd$Second..Sample,KRCd$Third.Sample,KRCd$Fourth.Sample,KRCd$Fifth.Sample))
names(KRCdSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRCdSample$First <- as.numeric(KRCdSample$First)
KRCdSample$Second <- as.numeric(KRCdSample$Second)
KRCdSample$Third <- as.numeric(KRCdSample$Third)
KRCdSample$Fourth <- as.numeric(KRCdSample$Fourth)
KRCdSample$Fifth <- as.numeric(KRCdSample$Fifth)
MeanKRCd<-c()
for (i in 1:5) {
  MeanKRCd[i]=mean(KRCdSample[,i])
}
KRCdggplot <- ggplot(data=data.frame(MeanKRCd)
                     ,aes(x=c(1:5),y=MeanKRCd))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Cd (PPM)')+
  ggtitle("KR")+
  ylim(0,5)+
  geom_hline(yintercept=0.01,color="ORANGE")+
  geom_hline(yintercept=3,color="PURPLE")
KTCd <- KT[KT$Parameter == 'Cd',]
KTCdSample=data.frame(cbind(KTCd$First.Sample,KTCd$Second..Sample,KTCd$Third.Sample,KTCd$Fourth.Sample,KTCd$Fifth.Sample))
names(KTCdSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTCdSample$First <- as.numeric(KTCdSample$First)
KTCdSample$Second <- as.numeric(KTCdSample$Second)
KTCdSample$Third <- as.numeric(KTCdSample$Third)
KTCdSample$Fourth <- as.numeric(KTCdSample$Fourth)
KTCdSample$Fifth <- as.numeric(KTCdSample$Fifth)
MeanKTCd<-c()
for (i in 1:5) {
  MeanKTCd[i]=mean(KTCdSample[,i])
}
KTCdggplot <- ggplot(data=data.frame(MeanKTCd)
                     ,aes(x=c(1:5),y=MeanKTCd))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Cd (PPM)')+
  ggtitle("KT")+
  ylim(0,5)+
  geom_hline(yintercept=0.01,color="ORANGE")+
  geom_hline(yintercept=3,color="PURPLE")
grid.arrange(KCdggplot,KRCdggplot,KTCdggplot,nrow=1)
###############Co###############
KCo <- K[K$Parameter == 'Co',]
KCoSample=data.frame(cbind(KCo$First.Sample,KCo$Second..Sample,KCo$Third.Sample,KCo$Fourth.Sample,KCo$Fifth.Sample))
names(KCoSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KCoSample$First <- as.numeric(KCoSample$First)
KCoSample$Second <- as.numeric(KCoSample$Second)
KCoSample$Third <- as.numeric(KCoSample$Third)
KCoSample$Fourth <- as.numeric(KCoSample$Fourth)
KCoSample$Fifth <- as.numeric(KCoSample$Fifth)
MeanKCo<-c()
for (i in 1:5) {
  MeanKCo[i]=mean(KCoSample[,i])
}
require(gridExtra)
KCoggplot <- ggplot(data=data.frame(MeanKCo)
                    ,aes(x=c(1:5),y=MeanKCo))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Co (PPM)')+
  ggtitle("K")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.05,color="ORANGE")
KRCo <- KR[KR$Parameter == 'Co',]
KRCoSample=data.frame(cbind(KRCo$First.Sample,KRCo$Second..Sample,KRCo$Third.Sample,KRCo$Fourth.Sample,KRCo$Fifth.Sample))
names(KRCoSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRCoSample$First <- as.numeric(KRCoSample$First)
KRCoSample$Second <- as.numeric(KRCoSample$Second)
KRCoSample$Third <- as.numeric(KRCoSample$Third)
KRCoSample$Fourth <- as.numeric(KRCoSample$Fourth)
KRCoSample$Fifth <- as.numeric(KRCoSample$Fifth)
MeanKRCo<-c()
for (i in 1:5) {
  MeanKRCo[i]=mean(KRCoSample[,i])
}
KRCoggplot <- ggplot(data=data.frame(MeanKRCo)
                     ,aes(x=c(1:5),y=MeanKRCo))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Co (PPM)')+
  ggtitle("KR")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.05,color="ORANGE")
KTCo <- KT[KT$Parameter == 'Co',]
KTCoSample=data.frame(cbind(KTCo$First.Sample,KTCo$Second..Sample,KTCo$Third.Sample,KTCo$Fourth.Sample,KTCo$Fifth.Sample))
names(KTCoSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTCoSample$First <- as.numeric(KTCoSample$First)
KTCoSample$Second <- as.numeric(KTCoSample$Second)
KTCoSample$Third <- as.numeric(KTCoSample$Third)
KTCoSample$Fourth <- as.numeric(KTCoSample$Fourth)
KTCoSample$Fifth <- as.numeric(KTCoSample$Fifth)
MeanKTCo<-c()
for (i in 1:5) {
  MeanKTCo[i]=mean(KTCoSample[,i])
}
KTCoggplot <- ggplot(data=data.frame(MeanKTCo)
                     ,aes(x=c(1:5),y=MeanKTCo))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Co (PPM)')+
  ggtitle("KT")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.05,color="ORANGE")
grid.arrange(KCoggplot,KRCoggplot,KTCoggplot,nrow=1)
###############Cr###############
KCr <- K[K$Parameter == 'Cr',]
KCrSample=data.frame(cbind(KCr$First.Sample,KCr$Second..Sample,KCr$Third.Sample,KCr$Fourth.Sample,KCr$Fifth.Sample))
names(KCrSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KCrSample$First <- as.numeric(KCrSample$First)
KCrSample$Second <- as.numeric(KCrSample$Second)
KCrSample$Third <- as.numeric(KCrSample$Third)
KCrSample$Fourth <- as.numeric(KCrSample$Fourth)
KCrSample$Fifth <- as.numeric(KCrSample$Fifth)
MeanKCr<-c()
for (i in 1:5) {
  MeanKCr[i]=mean(KCrSample[,i])
}
require(gridExtra)
KCrggplot <- ggplot(data=data.frame(MeanKCr)
                    ,aes(x=c(1:5),y=MeanKCr))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Cr (PPM)')+
  ggtitle("K")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.05,color="PURPLE")
KRCr <- KR[KR$Parameter == 'Cr',]
KRCrSample=data.frame(cbind(KRCr$First.Sample,KRCr$Second..Sample,KRCr$Third.Sample,KRCr$Fourth.Sample,KRCr$Fifth.Sample))
names(KRCrSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRCrSample$First <- as.numeric(KRCrSample$First)
KRCrSample$Second <- as.numeric(KRCrSample$Second)
KRCrSample$Third <- as.numeric(KRCrSample$Third)
KRCrSample$Fourth <- as.numeric(KRCrSample$Fourth)
KRCrSample$Fifth <- as.numeric(KRCrSample$Fifth)
MeanKRCr<-c()
for (i in 1:5) {
  MeanKRCr[i]=mean(KRCrSample[,i])
}
KRCrggplot <- ggplot(data=data.frame(MeanKRCr)
                     ,aes(x=c(1:5),y=MeanKRCr))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Cr (PPM)')+
  ggtitle("KR")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.05,color="PURPLE")
KTCr <- KT[KT$Parameter == 'Cr',]
KTCrSample=data.frame(cbind(KTCr$First.Sample,KTCr$Second..Sample,KTCr$Third.Sample,KTCr$Fourth.Sample,KTCr$Fifth.Sample))
names(KTCrSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTCrSample$First <- as.numeric(KTCrSample$First)
KTCrSample$Second <- as.numeric(KTCrSample$Second)
KTCrSample$Third <- as.numeric(KTCrSample$Third)
KTCrSample$Fourth <- as.numeric(KTCrSample$Fourth)
KTCrSample$Fifth <- as.numeric(KTCrSample$Fifth)
MeanKTCr<-c()
for (i in 1:5) {
  MeanKTCr[i]=mean(KTCrSample[,i])
}
KTCrggplot <- ggplot(data=data.frame(MeanKTCr)
                     ,aes(x=c(1:5),y=MeanKTCr))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Cr (PPM)')+
  ggtitle("KT")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.05,color="PURPLE")
grid.arrange(KCrggplot,KRCrggplot,KTCrggplot,nrow=1)
###############Cu###############
KCu <- K[K$Parameter == 'Cu',]
KCuSample=data.frame(cbind(KCu$First.Sample,KCu$Second..Sample,KCu$Third.Sample,KCu$Fourth.Sample,KCu$Fifth.Sample))
names(KCuSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KCuSample$First <- as.numeric(KCuSample$First)
KCuSample$Second <- as.numeric(KCuSample$Second)
KCuSample$Third <- as.numeric(KCuSample$Third)
KCuSample$Fourth <- as.numeric(KCuSample$Fourth)
KCuSample$Fifth <- as.numeric(KCuSample$Fifth)
MeanKCu<-c()
for (i in 1:5) {
  MeanKCu[i]=mean(KCuSample[,i])
}
require(gridExtra)
KCuggplot <- ggplot(data=data.frame(MeanKCu)
                    ,aes(x=c(1:5),y=MeanKCu))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Cu (PPM)')+
  ggtitle("K")+
  ylim(0,2.2)+
  geom_hline(yintercept=0.2,color="ORANGE")+
  geom_hline(yintercept=1,color="PURPLE")+
  geom_hline(yintercept=2,color="PURPLE")
KRCu <- KR[KR$Parameter == 'Cu',]
KRCuSample=data.frame(cbind(KRCu$First.Sample,KRCu$Second..Sample,KRCu$Third.Sample,KRCu$Fourth.Sample,KRCu$Fifth.Sample))
names(KRCuSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRCuSample$First <- as.numeric(KRCuSample$First)
KRCuSample$Second <- as.numeric(KRCuSample$Second)
KRCuSample$Third <- as.numeric(KRCuSample$Third)
KRCuSample$Fourth <- as.numeric(KRCuSample$Fourth)
KRCuSample$Fifth <- as.numeric(KRCuSample$Fifth)
MeanKRCu<-c()
for (i in 1:5) {
  MeanKRCu[i]=mean(KRCuSample[,i])
}
KRCuggplot <- ggplot(data=data.frame(MeanKRCu)
                     ,aes(x=c(1:5),y=MeanKRCu))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Cu (PPM)')+
  ggtitle("KR")+
  ylim(0,2.2)+
  geom_hline(yintercept=0.2,color="ORANGE")+
  geom_hline(yintercept=1,color="PURPLE")+
  geom_hline(yintercept=2,color="PURPLE")
KTCu <- KT[KT$Parameter == 'Cu',]
KTCuSample=data.frame(cbind(KTCu$First.Sample,KTCu$Second..Sample,KTCu$Third.Sample,KTCu$Fourth.Sample,KTCu$Fifth.Sample))
names(KTCuSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTCuSample$First <- as.numeric(KTCuSample$First)
KTCuSample$Second <- as.numeric(KTCuSample$Second)
KTCuSample$Third <- as.numeric(KTCuSample$Third)
KTCuSample$Fourth <- as.numeric(KTCuSample$Fourth)
KTCuSample$Fifth <- as.numeric(KTCuSample$Fifth)
MeanKTCu<-c()
for (i in 1:5) {
  MeanKTCu[i]=mean(KTCuSample[,i])
}
KTCuggplot <- ggplot(data=data.frame(MeanKTCu)
                     ,aes(x=c(1:5),y=MeanKTCu))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Cu (PPM)')+
  ggtitle("KT")+
  ylim(0,2.2)+
  geom_hline(yintercept=0.2,color="ORANGE")+
  geom_hline(yintercept=1,color="PURPLE")+
  geom_hline(yintercept=2,color="PURPLE")
grid.arrange(KCuggplot,KRCuggplot,KTCuggplot,nrow=1)
###############Fe###############
KFe <- K[K$Parameter == 'Fe',]
KFeSample=data.frame(cbind(KFe$First.Sample,KFe$Second..Sample,KFe$Third.Sample,KFe$Fourth.Sample,KFe$Fifth.Sample))
names(KFeSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KFeSample$First <- as.numeric(KFeSample$First)
KFeSample$Second <- as.numeric(KFeSample$Second)
KFeSample$Third <- as.numeric(KFeSample$Third)
KFeSample$Fourth <- as.numeric(KFeSample$Fourth)
KFeSample$Fifth <- as.numeric(KFeSample$Fifth)
MeanKFe<-c()
for (i in 1:5) {
  MeanKFe[i]=mean(KFeSample[,i])
}
require(gridExtra)
KFeggplot <- ggplot(data=data.frame(MeanKFe)
                    ,aes(x=c(1:5),y=MeanKFe))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Fe (PPM)')+
  ggtitle("K")+
  ylim(0,5.1)+
  geom_hline(yintercept=5,color="ORANGE")+
  geom_hline(yintercept=0.3,color="PURPLE")
KRFe <- KR[KR$Parameter == 'Fe',]
KRFeSample=data.frame(cbind(KRFe$First.Sample,KRFe$Second..Sample,KRFe$Third.Sample,KRFe$Fourth.Sample,KRFe$Fifth.Sample))
names(KRFeSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRFeSample$First <- as.numeric(KRFeSample$First)
KRFeSample$Second <- as.numeric(KRFeSample$Second)
KRFeSample$Third <- as.numeric(KRFeSample$Third)
KRFeSample$Fourth <- as.numeric(KRFeSample$Fourth)
KRFeSample$Fifth <- as.numeric(KRFeSample$Fifth)
MeanKRFe<-c()
for (i in 1:5) {
  MeanKRFe[i]=mean(KRFeSample[,i])
}
KRFeggplot <- ggplot(data=data.frame(MeanKRFe)
                     ,aes(x=c(1:5),y=MeanKRFe))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Fe (PPM)')+
  ggtitle("KR")+
  ylim(0,5.1)+
  geom_hline(yintercept=5,color="ORANGE")+
  geom_hline(yintercept=0.3,color="PURPLE")
KTFe <- KT[KT$Parameter == 'Fe',]
KTFeSample=data.frame(cbind(KTFe$First.Sample,KTFe$Second..Sample,KTFe$Third.Sample,KTFe$Fourth.Sample,KTFe$Fifth.Sample))
names(KTFeSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTFeSample$First <- as.numeric(KTFeSample$First)
KTFeSample$Second <- as.numeric(KTFeSample$Second)
KTFeSample$Third <- as.numeric(KTFeSample$Third)
KTFeSample$Fourth <- as.numeric(KTFeSample$Fourth)
KTFeSample$Fifth <- as.numeric(KTFeSample$Fifth)
MeanKTFe<-c()
for (i in 1:5) {
  MeanKTFe[i]=mean(KTFeSample[,i])
}
KTFeggplot <- ggplot(data=data.frame(MeanKTFe)
                     ,aes(x=c(1:5),y=MeanKTFe))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Fe (PPM)')+
  ggtitle("KT")+
  ylim(0,5.1)+
  geom_hline(yintercept=5,color="ORANGE")+
  geom_hline(yintercept=0.3,color="PURPLE")
grid.arrange(KFeggplot,KRFeggplot,KTFeggplot,nrow=1)
###############Hg###############
KHg <- K[K$Parameter == 'Hg',]
KHgSample=data.frame(cbind(KHg$First.Sample,KHg$Second..Sample,KHg$Third.Sample,KHg$Fourth.Sample,KHg$Fifth.Sample))
names(KHgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KHgSample$First <- as.numeric(KHgSample$First)
KHgSample$Second <- as.numeric(KHgSample$Second)
KHgSample$Third <- as.numeric(KHgSample$Third)
KHgSample$Fourth <- as.numeric(KHgSample$Fourth)
KHgSample$Fifth <- as.numeric(KHgSample$Fifth)
MeanKHg<-c()
for (i in 1:5) {
  MeanKHg[i]=mean(KHgSample[,i])
}
require(gridExtra)
KHgggplot <- ggplot(data=data.frame(MeanKHg)
                    ,aes(x=c(1:5),y=MeanKHg))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Hg (PPM)')+
  ggtitle("K")+
  ylim(0,12)+
  geom_hline(yintercept=6,color="PURPLE")
KRHg <- KR[KR$Parameter == 'Hg',]
KRHgSample=data.frame(cbind(KRHg$First.Sample,KRHg$Second..Sample,KRHg$Third.Sample,KRHg$Fourth.Sample,KRHg$Fifth.Sample))
names(KRHgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRHgSample$First <- as.numeric(KRHgSample$First)
KRHgSample$Second <- as.numeric(KRHgSample$Second)
KRHgSample$Third <- as.numeric(KRHgSample$Third)
KRHgSample$Fourth <- as.numeric(KRHgSample$Fourth)
KRHgSample$Fifth <- as.numeric(KRHgSample$Fifth)
MeanKRHg<-c()
for (i in 1:5) {
  MeanKRHg[i]=mean(KRHgSample[,i])
}
KRHgggplot <- ggplot(data=data.frame(MeanKRHg)
                     ,aes(x=c(1:5),y=MeanKRHg))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Hg (PPM)')+
  ggtitle("KR")+
  ylim(0,12)+
  geom_hline(yintercept=6,color="PURPLE")
KTHg <- KT[KT$Parameter == 'Hg',]
KTHgSample=data.frame(cbind(KTHg$First.Sample,KTHg$Second..Sample,KTHg$Third.Sample,KTHg$Fourth.Sample,KTHg$Fifth.Sample))
names(KTHgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTHgSample$First <- as.numeric(KTHgSample$First)
KTHgSample$Second <- as.numeric(KTHgSample$Second)
KTHgSample$Third <- as.numeric(KTHgSample$Third)
KTHgSample$Fourth <- as.numeric(KTHgSample$Fourth)
KTHgSample$Fifth <- as.numeric(KTHgSample$Fifth)
MeanKTHg<-c()
for (i in 1:5) {
  MeanKTHg[i]=mean(KTHgSample[,i])
}
KTHgggplot <- ggplot(data=data.frame(MeanKTHg)
                     ,aes(x=c(1:5),y=MeanKTHg))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Hg (PPM)')+
  ggtitle("KT")+
  ylim(0,12)+
  geom_hline(yintercept=6,color="PURPLE")
grid.arrange(KHgggplot,KRHgggplot,KTHgggplot,nrow=1)
###############K###############
KKk <- K[K$Parameter == 'K',]
KKkSample=data.frame(cbind(KKk$First.Sample,KKk$Second..Sample,KKk$Third.Sample,KKk$Fourth.Sample,KKk$Fifth.Sample))
names(KKkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KKkSample$First <- as.numeric(KKkSample$First)
KKkSample$Second <- as.numeric(KKkSample$Second)
KKkSample$Third <- as.numeric(KKkSample$Third)
KKkSample$Fourth <- as.numeric(KKkSample$Fourth)
KKkSample$Fifth <- as.numeric(KKkSample$Fifth)
MeanKKk<-c()
for (i in 1:5) {
  MeanKKk[i]=mean(KKkSample[,i])
}
require(gridExtra)
KKkggplot <- ggplot(data=data.frame(MeanKKk)
                    ,aes(x=c(1:5),y=MeanKKk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of K (PPM)')+
  ggtitle("K")+
  ylim(0,20)+
  geom_hline(yintercept=12,color="PURPLE")
KRKk <- KR[KR$Parameter == 'K',]
KRKkSample=data.frame(cbind(KRKk$First.Sample,KRKk$Second..Sample,KRKk$Third.Sample,KRKk$Fourth.Sample,KRKk$Fifth.Sample))
names(KRKkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRKkSample$First <- as.numeric(KRKkSample$First)
KRKkSample$Second <- as.numeric(KRKkSample$Second)
KRKkSample$Third <- as.numeric(KRKkSample$Third)
KRKkSample$Fourth <- as.numeric(KRKkSample$Fourth)
KRKkSample$Fifth <- as.numeric(KRKkSample$Fifth)
MeanKRKk<-c()
for (i in 1:5) {
  MeanKRKk[i]=mean(KRKkSample[,i])
}
KRKkggplot <- ggplot(data=data.frame(MeanKRKk)
                     ,aes(x=c(1:5),y=MeanKRKk))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of K (PPM)')+
  ggtitle("KR")+
  ylim(0,20)+
  geom_hline(yintercept=12,color="PURPLE")
KTKk <- KT[KT$Parameter == 'K',]
KTKkSample=data.frame(cbind(KTKk$First.Sample,KTKk$Second..Sample,KTKk$Third.Sample,KTKk$Fourth.Sample,KTKk$Fifth.Sample))
names(KTKkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTKkSample$First <- as.numeric(KTKkSample$First)
KTKkSample$Second <- as.numeric(KTKkSample$Second)
KTKkSample$Third <- as.numeric(KTKkSample$Third)
KTKkSample$Fourth <- as.numeric(KTKkSample$Fourth)
KTKkSample$Fifth <- as.numeric(KTKkSample$Fifth)
MeanKTKk<-c()
for (i in 1:5) {
  MeanKTKk[i]=mean(KTKkSample[,i])
}
KTKkggplot <- ggplot(data=data.frame(MeanKTKk)
                     ,aes(x=c(1:5),y=MeanKTKk))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of K (PPM)')+
  ggtitle("KT")+
  ylim(0,20)+
  geom_hline(yintercept=12,color="PURPLE")
grid.arrange(KKkggplot,KRKkggplot,KTKkggplot,nrow=1)
###############Li###############
Lik <- K[K$Parameter == 'Li',]
LikSample=data.frame(cbind(Lik$First.Sample,Lik$Second..Sample,Lik$Third.Sample,Lik$Fourth.Sample,Lik$Fifth.Sample))
names(LikSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
LikSample$First <- as.numeric(LikSample$First)
LikSample$Second <- as.numeric(LikSample$Second)
LikSample$Third <- as.numeric(LikSample$Third)
LikSample$Fourth <- as.numeric(LikSample$Fourth)
LikSample$Fifth <- as.numeric(LikSample$Fifth)
MeanLik<-c()
for (i in 1:5) {
  MeanLik[i]=mean(LikSample[,i])
}
require(gridExtra)
Likggplot <- ggplot(data=data.frame(MeanLik)
                    ,aes(x=c(1:5),y=MeanLik))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Li (PPM)')+
  ggtitle("K")+
  ylim(0,3)+
  geom_hline(yintercept=2.5,color="ORANGE")
KRLi <- KR[KR$Parameter == 'Li',]
KRLiSample=data.frame(cbind(KRLi$First.Sample,KRLi$Second..Sample,KRLi$Third.Sample,KRLi$Fourth.Sample,KRLi$Fifth.Sample))
names(KRLiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRLiSample$First <- as.numeric(KRLiSample$First)
KRLiSample$Second <- as.numeric(KRLiSample$Second)
KRLiSample$Third <- as.numeric(KRLiSample$Third)
KRLiSample$Fourth <- as.numeric(KRLiSample$Fourth)
KRLiSample$Fifth <- as.numeric(KRLiSample$Fifth)
MeanKRLi<-c()
for (i in 1:5) {
  MeanKRLi[i]=mean(KRLiSample[,i])
}
KRLiggplot <- ggplot(data=data.frame(MeanKRLi)
                     ,aes(x=c(1:5),y=MeanKRLi))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Li (PPM)')+
  ggtitle("KR")+
  ylim(0,3)+
  geom_hline(yintercept=2.5,color="ORANGE")
KTLi <- KT[KT$Parameter == 'Li',]
KTLiSample=data.frame(cbind(KTLi$First.Sample,KTLi$Second..Sample,KTLi$Third.Sample,KTLi$Fourth.Sample,KTLi$Fifth.Sample))
names(KTLiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTLiSample$First <- as.numeric(KTLiSample$First)
KTLiSample$Second <- as.numeric(KTLiSample$Second)
KTLiSample$Third <- as.numeric(KTLiSample$Third)
KTLiSample$Fourth <- as.numeric(KTLiSample$Fourth)
KTLiSample$Fifth <- as.numeric(KTLiSample$Fifth)
MeanKTLi<-c()
for (i in 1:5) {
  MeanKTLi[i]=mean(KTLiSample[,i])
}
KTLiggplot <- ggplot(data=data.frame(MeanKTLi)
                     ,aes(x=c(1:5),y=MeanKTLi))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Li (PPM)')+
  ggtitle("KT")+
  ylim(0,3)+
  geom_hline(yintercept=2.5,color="ORANGE")
grid.arrange(Likggplot,KRLiggplot,KTLiggplot,nrow=1)
###############Mg###############
Mgk <- K[K$Parameter == 'Mg',]
MgkSample=data.frame(cbind(Mgk$First.Sample,Mgk$Second..Sample,Mgk$Third.Sample,Mgk$Fourth.Sample,Mgk$Fifth.Sample))
names(MgkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
MgkSample$First <- as.numeric(MgkSample$First)
MgkSample$Second <- as.numeric(MgkSample$Second)
MgkSample$Third <- as.numeric(MgkSample$Third)
MgkSample$Fourth <- as.numeric(MgkSample$Fourth)
MgkSample$Fifth <- as.numeric(MgkSample$Fifth)
MeanMgk<-c()
for (i in 1:5) {
  MeanMgk[i]=mean(MgkSample[,i])
}
require(gridExtra)
Mgkggplot <- ggplot(data=data.frame(MeanMgk)
                    ,aes(x=c(1:5),y=MeanMgk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Mg (PPM)')+
  ggtitle("K")+
  ylim(0,120)+
  geom_hline(yintercept=100,color="ORANGE")+
  geom_hline(yintercept=30,color="PURPLE")
KRMg <- KR[KR$Parameter == 'Mg',]
KRMgSample=data.frame(cbind(KRMg$First.Sample,KRMg$Second..Sample,KRMg$Third.Sample,KRMg$Fourth.Sample,KRMg$Fifth.Sample))
names(KRMgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRMgSample$First <- as.numeric(KRMgSample$First)
KRMgSample$Second <- as.numeric(KRMgSample$Second)
KRMgSample$Third <- as.numeric(KRMgSample$Third)
KRMgSample$Fourth <- as.numeric(KRMgSample$Fourth)
KRMgSample$Fifth <- as.numeric(KRMgSample$Fifth)
MeanKRMg<-c()
for (i in 1:5) {
  MeanKRMg[i]=mean(KRMgSample[,i])
}
KRMgggplot <- ggplot(data=data.frame(MeanKRMg)
                     ,aes(x=c(1:5),y=MeanKRMg))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Mg (PPM)')+
  ggtitle("KR")+
  ylim(0,120)+
  geom_hline(yintercept=100,color="ORANGE")+
  geom_hline(yintercept=30,color="PURPLE")
KTMg <- KT[KT$Parameter == 'Mg',]
KTMgSample=data.frame(cbind(KTMg$First.Sample,KTMg$Second..Sample,KTMg$Third.Sample,KTMg$Fourth.Sample,KTMg$Fifth.Sample))
names(KTMgSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTMgSample$First <- as.numeric(KTMgSample$First)
KTMgSample$Second <- as.numeric(KTMgSample$Second)
KTMgSample$Third <- as.numeric(KTMgSample$Third)
KTMgSample$Fourth <- as.numeric(KTMgSample$Fourth)
KTMgSample$Fifth <- as.numeric(KTMgSample$Fifth)
MeanKTMg<-c()
for (i in 1:5) {
  MeanKTMg[i]=mean(KTMgSample[,i])
}
KTMgggplot <- ggplot(data=data.frame(MeanKTMg)
                     ,aes(x=c(1:5),y=MeanKTMg))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Mg (PPM)')+
  ggtitle("KT")+
  ylim(0,120)+
  geom_hline(yintercept=100,color="ORANGE")+
  geom_hline(yintercept=30,color="PURPLE")
grid.arrange(Mgkggplot,KRMgggplot,KTMgggplot,nrow=1)
###############Mn###############
Mnk <- K[K$Parameter == 'Mn',]
MnkSample=data.frame(cbind(Mnk$First.Sample,Mnk$Second..Sample,Mnk$Third.Sample,Mnk$Fourth.Sample,Mnk$Fifth.Sample))
names(MnkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
MnkSample$First <- as.numeric(MnkSample$First)
MnkSample$Second <- as.numeric(MnkSample$Second)
MnkSample$Third <- as.numeric(MnkSample$Third)
MnkSample$Fourth <- as.numeric(MnkSample$Fourth)
MnkSample$Fifth <- as.numeric(MnkSample$Fifth)
MeanMnk<-c()
for (i in 1:5) {
  MeanMnk[i]=mean(MnkSample[,i])
}
require(gridExtra)
Mnkggplot <- ggplot(data=data.frame(MeanMnk)
                    ,aes(x=c(1:5),y=MeanMnk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Mn (PPM)')+
  ggtitle("K")+
  ylim(0,1)+
  geom_hline(yintercept=0.2,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")+
  geom_hline(yintercept=0.4,color="PURPLE")
KRMn <- KR[KR$Parameter == 'Mn',]
KRMnSample=data.frame(cbind(KRMn$First.Sample,KRMn$Second..Sample,KRMn$Third.Sample,KRMn$Fourth.Sample,KRMn$Fifth.Sample))
names(KRMnSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRMnSample$First <- as.numeric(KRMnSample$First)
KRMnSample$Second <- as.numeric(KRMnSample$Second)
KRMnSample$Third <- as.numeric(KRMnSample$Third)
KRMnSample$Fourth <- as.numeric(KRMnSample$Fourth)
KRMnSample$Fifth <- as.numeric(KRMnSample$Fifth)
MeanKRMn<-c()
for (i in 1:5) {
  MeanKRMn[i]=mean(KRMnSample[,i])
}
KRMnggplot <- ggplot(data=data.frame(MeanKRMn)
                     ,aes(x=c(1:5),y=MeanKRMn))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Mn (PPM)')+
  ggtitle("KR")+
  ylim(0,1)+
  geom_hline(yintercept=0.2,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")+
  geom_hline(yintercept=0.4,color="PURPLE")
KTMn <- KT[KT$Parameter == 'Mn',]
KTMnSample=data.frame(cbind(KTMn$First.Sample,KTMn$Second..Sample,KTMn$Third.Sample,KTMn$Fourth.Sample,KTMn$Fifth.Sample))
names(KTMnSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTMnSample$First <- as.numeric(KTMnSample$First)
KTMnSample$Second <- as.numeric(KTMnSample$Second)
KTMnSample$Third <- as.numeric(KTMnSample$Third)
KTMnSample$Fourth <- as.numeric(KTMnSample$Fourth)
KTMnSample$Fifth <- as.numeric(KTMnSample$Fifth)
MeanKTMn<-c()
for (i in 1:5) {
  MeanKTMn[i]=mean(KTMnSample[,i])
}
KTMnggplot <- ggplot(data=data.frame(MeanKTMn)
                     ,aes(x=c(1:5),y=MeanKTMn))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Mn (PPM)')+
  ggtitle("KT")+
  ylim(0,1)+
  geom_hline(yintercept=0.2,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")+
  geom_hline(yintercept=0.4,color="PURPLE")
grid.arrange(Mnkggplot,KRMnggplot,KTMnggplot,nrow=1)
###############Mo###############
Mok <- K[K$Parameter == 'Mo',]
MokSample=data.frame(cbind(Mok$First.Sample,Mok$Second..Sample,Mok$Third.Sample,Mok$Fourth.Sample,Mok$Fifth.Sample))
names(MokSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
MokSample$First <- as.numeric(MokSample$First)
MokSample$Second <- as.numeric(MokSample$Second)
MokSample$Third <- as.numeric(MokSample$Third)
MokSample$Fourth <- as.numeric(MokSample$Fourth)
MokSample$Fifth <- as.numeric(MokSample$Fifth)
MeanMok<-c()
for (i in 1:5) {
  MeanMok[i]=mean(MokSample[,i])
}
require(gridExtra)
Mokggplot <- ggplot(data=data.frame(MeanMok)
                    ,aes(x=c(1:5),y=MeanMok))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Mo (PPM)')+
  ggtitle("K")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.01,color="ORANGE")+
  geom_hline(yintercept=0.07,color="PURPLE")
KRMo <- KR[KR$Parameter == 'Mo',]
KRMoSample=data.frame(cbind(KRMo$First.Sample,KRMo$Second..Sample,KRMo$Third.Sample,KRMo$Fourth.Sample,KRMo$Fifth.Sample))
names(KRMoSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRMoSample$First <- as.numeric(KRMoSample$First)
KRMoSample$Second <- as.numeric(KRMoSample$Second)
KRMoSample$Third <- as.numeric(KRMoSample$Third)
KRMoSample$Fourth <- as.numeric(KRMoSample$Fourth)
KRMoSample$Fifth <- as.numeric(KRMoSample$Fifth)
MeanKRMo<-c()
for (i in 1:5) {
  MeanKRMo[i]=mean(KRMoSample[,i])
}
KRMoggplot <- ggplot(data=data.frame(MeanKRMo)
                     ,aes(x=c(1:5),y=MeanKRMo))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Mo (PPM)')+
  ggtitle("KR")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.01,color="ORANGE")+
  geom_hline(yintercept=0.07,color="PURPLE")
KTMo <- KT[KT$Parameter == 'Mo',]
KTMoSample=data.frame(cbind(KTMo$First.Sample,KTMo$Second..Sample,KTMo$Third.Sample,KTMo$Fourth.Sample,KTMo$Fifth.Sample))
names(KTMoSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTMoSample$First <- as.numeric(KTMoSample$First)
KTMoSample$Second <- as.numeric(KTMoSample$Second)
KTMoSample$Third <- as.numeric(KTMoSample$Third)
KTMoSample$Fourth <- as.numeric(KTMoSample$Fourth)
KTMoSample$Fifth <- as.numeric(KTMoSample$Fifth)
MeanKTMo<-c()
for (i in 1:5) {
  MeanKTMo[i]=mean(KTMoSample[,i])
}
KTMoggplot <- ggplot(data=data.frame(MeanKTMo)
                     ,aes(x=c(1:5),y=MeanKTMo))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Mo (PPM)')+
  ggtitle("KT")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.01,color="ORANGE")+
  geom_hline(yintercept=0.07,color="PURPLE")
grid.arrange(Mokggplot,KRMoggplot,KTMoggplot,nrow=1)
###############Na###############
Nak <- K[K$Parameter == 'Na',]
NakSample=data.frame(cbind(Nak$First.Sample,Nak$Second..Sample,Nak$Third.Sample,Nak$Fourth.Sample,Nak$Fifth.Sample))
names(NakSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
NakSample$First <- as.numeric(NakSample$First)
NakSample$Second <- as.numeric(NakSample$Second)
NakSample$Third <- as.numeric(NakSample$Third)
NakSample$Fourth <- as.numeric(NakSample$Fourth)
NakSample$Fifth <- as.numeric(NakSample$Fifth)
MeanNak<-c()
for (i in 1:5) {
  MeanNak[i]=mean(NakSample[,i])
}
require(gridExtra)
Nakggplot <- ggplot(data=data.frame(MeanNak)
                    ,aes(x=c(1:5),y=MeanNak))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Na (PPM)')+
  ggtitle("K")+
  ylim(0,220)+
  geom_hline(yintercept=200,color="PURPLE")
KRNa <- KR[KR$Parameter == 'Na',]
KRNaSample=data.frame(cbind(KRNa$First.Sample,KRNa$Second..Sample,KRNa$Third.Sample,KRNa$Fourth.Sample,KRNa$Fifth.Sample))
names(KRNaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRNaSample$First <- as.numeric(KRNaSample$First)
KRNaSample$Second <- as.numeric(KRNaSample$Second)
KRNaSample$Third <- as.numeric(KRNaSample$Third)
KRNaSample$Fourth <- as.numeric(KRNaSample$Fourth)
KRNaSample$Fifth <- as.numeric(KRNaSample$Fifth)
MeanKRNa<-c()
for (i in 1:5) {
  MeanKRNa[i]=mean(KRNaSample[,i])
}
KRNaggplot <- ggplot(data=data.frame(MeanKRNa)
                     ,aes(x=c(1:5),y=MeanKRNa))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Na (PPM)')+
  ggtitle("KR")+
  ylim(0,220)+
  geom_hline(yintercept=200,color="PURPLE")
KTNa <- KT[KT$Parameter == 'Na',]
KTNaSample=data.frame(cbind(KTNa$First.Sample,KTNa$Second..Sample,KTNa$Third.Sample,KTNa$Fourth.Sample,KTNa$Fifth.Sample))
names(KTNaSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTNaSample$First <- as.numeric(KTNaSample$First)
KTNaSample$Second <- as.numeric(KTNaSample$Second)
KTNaSample$Third <- as.numeric(KTNaSample$Third)
KTNaSample$Fourth <- as.numeric(KTNaSample$Fourth)
KTNaSample$Fifth <- as.numeric(KTNaSample$Fifth)
MeanKTNa<-c()
for (i in 1:5) {
  MeanKTNa[i]=mean(KTNaSample[,i])
}
KTNaggplot <- ggplot(data=data.frame(MeanKTNa)
                     ,aes(x=c(1:5),y=MeanKTNa))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Na (PPM)')+
  ggtitle("KT")+
  ylim(0,220)+
  geom_hline(yintercept=200,color="PURPLE")
grid.arrange(Nakggplot,KRNaggplot,KTNaggplot,nrow=1)
###############Ni###############
Nik <- K[K$Parameter == 'Ni',]
NikSample=data.frame(cbind(Nik$First.Sample,Nik$Second..Sample,Nik$Third.Sample,Nik$Fourth.Sample,Nik$Fifth.Sample))
names(NikSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
NikSample$First <- as.numeric(NikSample$First)
NikSample$Second <- as.numeric(NikSample$Second)
NikSample$Third <- as.numeric(NikSample$Third)
NikSample$Fourth <- as.numeric(NikSample$Fourth)
NikSample$Fifth <- as.numeric(NikSample$Fifth)
MeanNik<-c()
for (i in 1:5) {
  MeanNik[i]=mean(NikSample[,i])
}
require(gridExtra)
Nikggplot <- ggplot(data=data.frame(MeanNik)
                    ,aes(x=c(1:5),y=MeanNik))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Ni (PPM)')+
  ggtitle("K")+
  ylim(0,0.21)+
  geom_hline(yintercept=0.07,color="PURPLE")+
  geom_hline(yintercept=0.2,color="ORANGE")
KRNi <- KR[KR$Parameter == 'Ni',]
KRNiSample=data.frame(cbind(KRNi$First.Sample,KRNi$Second..Sample,KRNi$Third.Sample,KRNi$Fourth.Sample,KRNi$Fifth.Sample))
names(KRNiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRNiSample$First <- as.numeric(KRNiSample$First)
KRNiSample$Second <- as.numeric(KRNiSample$Second)
KRNiSample$Third <- as.numeric(KRNiSample$Third)
KRNiSample$Fourth <- as.numeric(KRNiSample$Fourth)
KRNiSample$Fifth <- as.numeric(KRNiSample$Fifth)
MeanKRNi<-c()
for (i in 1:5) {
  MeanKRNi[i]=mean(KRNiSample[,i])
}
KRNiggplot <- ggplot(data=data.frame(MeanKRNi)
                     ,aes(x=c(1:5),y=MeanKRNi))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Ni (PPM)')+
  ggtitle("KR")+
  ylim(0,0.21)+
  geom_hline(yintercept=0.07,color="PURPLE")+
  geom_hline(yintercept=0.2,color="ORANGE")
KTNi <- KT[KT$Parameter == 'Ni',]
KTNiSample=data.frame(cbind(KTNi$First.Sample,KTNi$Second..Sample,KTNi$Third.Sample,KTNi$Fourth.Sample,KTNi$Fifth.Sample))
names(KTNiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTNiSample$First <- as.numeric(KTNiSample$First)
KTNiSample$Second <- as.numeric(KTNiSample$Second)
KTNiSample$Third <- as.numeric(KTNiSample$Third)
KTNiSample$Fourth <- as.numeric(KTNiSample$Fourth)
KTNiSample$Fifth <- as.numeric(KTNiSample$Fifth)
MeanKTNi<-c()
for (i in 1:5) {
  MeanKTNi[i]=mean(KTNiSample[,i])
}
KTNiggplot <- ggplot(data=data.frame(MeanKTNi)
                     ,aes(x=c(1:5),y=MeanKTNi))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Ni (PPM)')+
  ggtitle("KT")+
  ylim(0,0.21)+
  geom_hline(yintercept=0.07,color="PURPLE")+
  geom_hline(yintercept=0.2,color="ORANGE")
grid.arrange(Nikggplot,KRNiggplot,KTNiggplot,nrow=1)
###############Pb###############
Pbk <- K[K$Parameter == 'Pb',]
PbkSample=data.frame(cbind(Pbk$First.Sample,Pbk$Second..Sample,Pbk$Third.Sample,Pbk$Fourth.Sample,Pbk$Fifth.Sample))
names(PbkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
PbkSample$First <- as.numeric(PbkSample$First)
PbkSample$Second <- as.numeric(PbkSample$Second)
PbkSample$Third <- as.numeric(PbkSample$Third)
PbkSample$Fourth <- as.numeric(PbkSample$Fourth)
PbkSample$Fifth <- as.numeric(PbkSample$Fifth)
MeanPbk<-c()
for (i in 1:5) {
  MeanPbk[i]=mean(PbkSample[,i])
}
require(gridExtra)
Pbkggplot <- ggplot(data=data.frame(MeanPbk)
                    ,aes(x=c(1:5),y=MeanPbk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Pb (PPM)')+
  ggtitle("K")+
  ylim(0,1.36)+
  geom_hline(yintercept=0.01,color="PURPLE")+
  geom_hline(yintercept=1,color="ORANGE")
KRPb <- KR[KR$Parameter == 'Pb',]
KRPbSample=data.frame(cbind(KRPb$First.Sample,KRPb$Second..Sample,KRPb$Third.Sample,KRPb$Fourth.Sample,KRPb$Fifth.Sample))
names(KRPbSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRPbSample$First <- as.numeric(KRPbSample$First)
KRPbSample$Second <- as.numeric(KRPbSample$Second)
KRPbSample$Third <- as.numeric(KRPbSample$Third)
KRPbSample$Fourth <- as.numeric(KRPbSample$Fourth)
KRPbSample$Fifth <- as.numeric(KRPbSample$Fifth)
MeanKRPb<-c()
for (i in 1:5) {
  MeanKRPb[i]=mean(KRPbSample[,i])
}
KRPbggplot <- ggplot(data=data.frame(MeanKRPb)
                     ,aes(x=c(1:5),y=MeanKRPb))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Pb (PPM)')+
  ggtitle("KR")+
  ylim(0,1.36)+
  geom_hline(yintercept=0.01,color="PURPLE")+
  geom_hline(yintercept=1,color="ORANGE")
KTPb <- KT[KT$Parameter == 'Pb',]
KTPbSample=data.frame(cbind(KTPb$First.Sample,KTPb$Second..Sample,KTPb$Third.Sample,KTPb$Fourth.Sample,KTPb$Fifth.Sample))
names(KTPbSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTPbSample$First <- as.numeric(KTPbSample$First)
KTPbSample$Second <- as.numeric(KTPbSample$Second)
KTPbSample$Third <- as.numeric(KTPbSample$Third)
KTPbSample$Fourth <- as.numeric(KTPbSample$Fourth)
KTPbSample$Fifth <- as.numeric(KTPbSample$Fifth)
MeanKTPb<-c()
for (i in 1:5) {
  MeanKTPb[i]=mean(KTPbSample[,i])
}
KTPbggplot <- ggplot(data=data.frame(MeanKTPb)
                     ,aes(x=c(1:5),y=MeanKTPb))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Pb (PPM)')+
  ggtitle("KT")+
  ylim(0,1.36)+
  geom_hline(yintercept=0.01,color="PURPLE")+
  geom_hline(yintercept=1,color="ORANGE")
grid.arrange(Pbkggplot,KRPbggplot,KTPbggplot,nrow=1)
###############P###############
Ppk <- K[K$Parameter == 'P',]
PpkSample=data.frame(cbind(Ppk$First.Sample,Ppk$Second..Sample,Ppk$Third.Sample,Ppk$Fourth.Sample,Ppk$Fifth.Sample))
names(PpkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
PpkSample$First <- as.numeric(PpkSample$First)
PpkSample$Second <- as.numeric(PpkSample$Second)
PpkSample$Third <- as.numeric(PpkSample$Third)
PpkSample$Fourth <- as.numeric(PpkSample$Fourth)
PpkSample$Fifth <- as.numeric(PpkSample$Fifth)
MeanPpk<-c()
for (i in 1:5) {
  MeanPpk[i]=mean(PpkSample[,i])
}
require(gridExtra)
Ppkggplot <- ggplot(data=data.frame(MeanPpk)
                    ,aes(x=c(1:5),y=MeanPpk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of P (PPM)')+
  ggtitle("K")+
  ylim(0,30)
KRPp <- KR[KR$Parameter == 'P',]
KRPpSample=data.frame(cbind(KRPp$First.Sample,KRPp$Second..Sample,KRPp$Third.Sample,KRPp$Fourth.Sample,KRPp$Fifth.Sample))
names(KRPpSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRPpSample$First <- as.numeric(KRPpSample$First)
KRPpSample$Second <- as.numeric(KRPpSample$Second)
KRPpSample$Third <- as.numeric(KRPpSample$Third)
KRPpSample$Fourth <- as.numeric(KRPpSample$Fourth)
KRPpSample$Fifth <- as.numeric(KRPpSample$Fifth)
MeanKRPp<-c()
for (i in 1:5) {
  MeanKRPp[i]=mean(KRPpSample[,i])
}
KRPpggplot <- ggplot(data=data.frame(MeanKRPp)
                     ,aes(x=c(1:5),y=MeanKRPp))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of P (PPM)')+
  ggtitle("KR")+
  ylim(0,30)
KTPp <- KT[KT$Parameter == 'P',]
KTPpSample=data.frame(cbind(KTPp$First.Sample,KTPp$Second..Sample,KTPp$Third.Sample,KTPp$Fourth.Sample,KTPp$Fifth.Sample))
names(KTPpSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTPpSample$First <- as.numeric(KTPpSample$First)
KTPpSample$Second <- as.numeric(KTPpSample$Second)
KTPpSample$Third <- as.numeric(KTPpSample$Third)
KTPpSample$Fourth <- as.numeric(KTPpSample$Fourth)
KTPpSample$Fifth <- as.numeric(KTPpSample$Fifth)
MeanKTPp<-c()
for (i in 1:5) {
  MeanKTPp[i]=mean(KTPpSample[,i])
}
KTPpggplot <- ggplot(data=data.frame(MeanKTPp)
                     ,aes(x=c(1:5),y=MeanKTPp))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of P (PPM)')+
  ggtitle("KT")+
  ylim(0,30)
grid.arrange(Ppkggplot,KRPpggplot,KTPpggplot,nrow=1)
###############Sb###############
Sbk <- K[K$Parameter == 'Sb',]
SbkSample=data.frame(cbind(Sbk$First.Sample,Sbk$Second..Sample,Sbk$Third.Sample,Sbk$Fourth.Sample,Sbk$Fifth.Sample))
names(SbkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
SbkSample$First <- as.numeric(SbkSample$First)
SbkSample$Second <- as.numeric(SbkSample$Second)
SbkSample$Third <- as.numeric(SbkSample$Third)
SbkSample$Fourth <- as.numeric(SbkSample$Fourth)
SbkSample$Fifth <- as.numeric(SbkSample$Fifth)
MeanSbk<-c()
for (i in 1:5) {
  MeanSbk[i]=mean(SbkSample[,i])
}
require(gridExtra)
Sbkggplot <- ggplot(data=data.frame(MeanSbk)
                    ,aes(x=c(1:5),y=MeanSbk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Sb (PPM)')+
  ggtitle("K")+
  ylim(0,0.025)+
  geom_hline(yintercept=0.02,color="PURPLE")
KRSb <- KR[KR$Parameter == 'Sb',]
KRSbSample=data.frame(cbind(KRSb$First.Sample,KRSb$Second..Sample,KRSb$Third.Sample,KRSb$Fourth.Sample,KRSb$Fifth.Sample))
names(KRSbSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRSbSample$First <- as.numeric(KRSbSample$First)
KRSbSample$Second <- as.numeric(KRSbSample$Second)
KRSbSample$Third <- as.numeric(KRSbSample$Third)
KRSbSample$Fourth <- as.numeric(KRSbSample$Fourth)
KRSbSample$Fifth <- as.numeric(KRSbSample$Fifth)
MeanKRSb<-c()
for (i in 1:5) {
  MeanKRSb[i]=mean(KRSbSample[,i])
}
KRSbggplot <- ggplot(data=data.frame(MeanKRSb)
                     ,aes(x=c(1:5),y=MeanKRSb))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Sb (PPM)')+
  ggtitle("KR")+
  ylim(0,0.025)+
  geom_hline(yintercept=0.02,color="PURPLE")
KTSb <- KT[KT$Parameter == 'Sb',]
KTSbSample=data.frame(cbind(KTSb$First.Sample,KTSb$Second..Sample,KTSb$Third.Sample,KTSb$Fourth.Sample,KTSb$Fifth.Sample))
names(KTSbSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTSbSample$First <- as.numeric(KTSbSample$First)
KTSbSample$Second <- as.numeric(KTSbSample$Second)
KTSbSample$Third <- as.numeric(KTSbSample$Third)
KTSbSample$Fourth <- as.numeric(KTSbSample$Fourth)
KTSbSample$Fifth <- as.numeric(KTSbSample$Fifth)
MeanKTSb<-c()
for (i in 1:5) {
  MeanKTSb[i]=mean(KTSbSample[,i])
}
KTSbggplot <- ggplot(data=data.frame(MeanKTSb)
                     ,aes(x=c(1:5),y=MeanKTSb))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Sb (PPM)')+
  ggtitle("KT")+
  ylim(0,0.025)+
  geom_hline(yintercept=0.02,color="PURPLE")
grid.arrange(Sbkggplot,KRSbggplot,KTSbggplot,nrow=1)
###############Se###############
Sek <- K[K$Parameter == 'Se',]
SekSample=data.frame(cbind(Sek$First.Sample,Sek$Second..Sample,Sek$Third.Sample,Sek$Fourth.Sample,Sek$Fifth.Sample))
names(SekSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
SekSample$First <- as.numeric(SekSample$First)
SekSample$Second <- as.numeric(SekSample$Second)
SekSample$Third <- as.numeric(SekSample$Third)
SekSample$Fourth <- as.numeric(SekSample$Fourth)
SekSample$Fifth <- as.numeric(SekSample$Fifth)
MeanSek<-c()
for (i in 1:5) {
  MeanSek[i]=mean(SekSample[,i])
}
require(gridExtra)
Sekggplot <- ggplot(data=data.frame(MeanSek)
                    ,aes(x=c(1:5),y=MeanSek))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Se (PPM)')+
  ggtitle("K")+
  ylim(0,1.3)+
  geom_hline(yintercept=0.01,color="PURPLE")+
  geom_hline(yintercept=0.02,color="ORANGE")
KRSe <- KR[KR$Parameter == 'Se',]
KRSeSample=data.frame(cbind(KRSe$First.Sample,KRSe$Second..Sample,KRSe$Third.Sample,KRSe$Fourth.Sample,KRSe$Fifth.Sample))
names(KRSeSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRSeSample$First <- as.numeric(KRSeSample$First)
KRSeSample$Second <- as.numeric(KRSeSample$Second)
KRSeSample$Third <- as.numeric(KRSeSample$Third)
KRSeSample$Fourth <- as.numeric(KRSeSample$Fourth)
KRSeSample$Fifth <- as.numeric(KRSeSample$Fifth)
MeanKRSe<-c()
for (i in 1:5) {
  MeanKRSe[i]=mean(KRSeSample[,i])
}
KRSeggplot <- ggplot(data=data.frame(MeanKRSe)
                     ,aes(x=c(1:5),y=MeanKRSe))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Se (PPM)')+
  ggtitle("KR")+
  ylim(0,1.3)+
  geom_hline(yintercept=0.01,color="PURPLE")+
  geom_hline(yintercept=0.02,color="ORANGE")
KTSe <- KT[KT$Parameter == 'Se',]
KTSeSample=data.frame(cbind(KTSe$First.Sample,KTSe$Second..Sample,KTSe$Third.Sample,KTSe$Fourth.Sample,KTSe$Fifth.Sample))
names(KTSeSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTSeSample$First <- as.numeric(KTSeSample$First)
KTSeSample$Second <- as.numeric(KTSeSample$Second)
KTSeSample$Third <- as.numeric(KTSeSample$Third)
KTSeSample$Fourth <- as.numeric(KTSeSample$Fourth)
KTSeSample$Fifth <- as.numeric(KTSeSample$Fifth)
MeanKTSe<-c()
for (i in 1:5) {
  MeanKTSe[i]=mean(KTSeSample[,i])
}
KTSeggplot <- ggplot(data=data.frame(MeanKTSe)
                     ,aes(x=c(1:5),y=MeanKTSe))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Se (PPM)')+
  ggtitle("KT")+
  ylim(0,1.3)+
  geom_hline(yintercept=0.01,color="PURPLE")+
  geom_hline(yintercept=0.02,color="ORANGE")
grid.arrange(Sekggplot,KRSeggplot,KTSeggplot,nrow=1)
###############Si###############
Sik <- K[K$Parameter == 'Si',]
SikSample=data.frame(cbind(Sik$First.Sample,Sik$Second..Sample,Sik$Third.Sample,Sik$Fourth.Sample,Sik$Fifth.Sample))
names(SikSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
SikSample$First <- as.numeric(SikSample$First)
SikSample$Second <- as.numeric(SikSample$Second)
SikSample$Third <- as.numeric(SikSample$Third)
SikSample$Fourth <- as.numeric(SikSample$Fourth)
SikSample$Fifth <- as.numeric(SikSample$Fifth)
MeanSik<-c()
for (i in 1:5) {
  MeanSik[i]=mean(SikSample[,i])
}
require(gridExtra)
Sikggplot <- ggplot(data=data.frame(MeanSik)
                    ,aes(x=c(1:5),y=MeanSik))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Si (PPM)')+
  ggtitle("K")+
  ylim(0,10)
KRSi <- KR[KR$Parameter == 'Si',]
KRSiSample=data.frame(cbind(KRSi$First.Sample,KRSi$Second..Sample,KRSi$Third.Sample,KRSi$Fourth.Sample,KRSi$Fifth.Sample))
names(KRSiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRSiSample$First <- as.numeric(KRSiSample$First)
KRSiSample$Second <- as.numeric(KRSiSample$Second)
KRSiSample$Third <- as.numeric(KRSiSample$Third)
KRSiSample$Fourth <- as.numeric(KRSiSample$Fourth)
KRSiSample$Fifth <- as.numeric(KRSiSample$Fifth)
MeanKRSi<-c()
for (i in 1:5) {
  MeanKRSi[i]=mean(KRSiSample[,i])
}
KRSiggplot <- ggplot(data=data.frame(MeanKRSi)
                     ,aes(x=c(1:5),y=MeanKRSi))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Si (PPM)')+
  ggtitle("KR")+
  ylim(0,10)
KTSi <- KT[KT$Parameter == 'Si',]
KTSiSample=data.frame(cbind(KTSi$First.Sample,KTSi$Second..Sample,KTSi$Third.Sample,KTSi$Fourth.Sample,KTSi$Fifth.Sample))
names(KTSiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTSiSample$First <- as.numeric(KTSiSample$First)
KTSiSample$Second <- as.numeric(KTSiSample$Second)
KTSiSample$Third <- as.numeric(KTSiSample$Third)
KTSiSample$Fourth <- as.numeric(KTSiSample$Fourth)
KTSiSample$Fifth <- as.numeric(KTSiSample$Fifth)
MeanKTSi<-c()
for (i in 1:5) {
  MeanKTSi[i]=mean(KTSiSample[,i])
}
KTSiggplot <- ggplot(data=data.frame(MeanKTSi)
                     ,aes(x=c(1:5),y=MeanKTSi))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Si (PPM)')+
  ggtitle("KT")+
  ylim(0,10)
grid.arrange(Sikggplot,KRSiggplot,KTSiggplot,nrow=1)
###############Sn###############
Snk <- K[K$Parameter == 'Sn',]
SnkSample=data.frame(cbind(Snk$First.Sample,Snk$Second..Sample,Snk$Third.Sample,Snk$Fourth.Sample,Snk$Fifth.Sample))
names(SnkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
SnkSample$First <- as.numeric(SnkSample$First)
SnkSample$Second <- as.numeric(SnkSample$Second)
SnkSample$Third <- as.numeric(SnkSample$Third)
SnkSample$Fourth <- as.numeric(SnkSample$Fourth)
SnkSample$Fifth <- as.numeric(SnkSample$Fifth)
MeanSnk<-c()
for (i in 1:5) {
  MeanSnk[i]=mean(SnkSample[,i])
}
require(gridExtra)
Snkggplot <- ggplot(data=data.frame(MeanSnk)
                    ,aes(x=c(1:5),y=MeanSnk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Sn (PPM)')+
  ggtitle("K")+
  ylim(0,0.01)
KRSn <- KR[KR$Parameter == 'Sn',]
KRSnSample=data.frame(cbind(KRSn$First.Sample,KRSn$Second..Sample,KRSn$Third.Sample,KRSn$Fourth.Sample,KRSn$Fifth.Sample))
names(KRSnSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRSnSample$First <- as.numeric(KRSnSample$First)
KRSnSample$Second <- as.numeric(KRSnSample$Second)
KRSnSample$Third <- as.numeric(KRSnSample$Third)
KRSnSample$Fourth <- as.numeric(KRSnSample$Fourth)
KRSnSample$Fifth <- as.numeric(KRSnSample$Fifth)
MeanKRSn<-c()
for (i in 1:5) {
  MeanKRSn[i]=mean(KRSnSample[,i])
}
KRSnggplot <- ggplot(data=data.frame(MeanKRSn)
                     ,aes(x=c(1:5),y=MeanKRSn))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Sn (PPM)')+
  ggtitle("KR")+
  ylim(0,0.01)
KTSn <- KT[KT$Parameter == 'Sn',]
KTSnSample=data.frame(cbind(KTSn$First.Sample,KTSn$Second..Sample,KTSn$Third.Sample,KTSn$Fourth.Sample,KTSn$Fifth.Sample))
names(KTSnSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTSnSample$First <- as.numeric(KTSnSample$First)
KTSnSample$Second <- as.numeric(KTSnSample$Second)
KTSnSample$Third <- as.numeric(KTSnSample$Third)
KTSnSample$Fourth <- as.numeric(KTSnSample$Fourth)
KTSnSample$Fifth <- as.numeric(KTSnSample$Fifth)
MeanKTSn<-c()
for (i in 1:5) {
  MeanKTSn[i]=mean(KTSnSample[,i])
}
KTSnggplot <- ggplot(data=data.frame(MeanKTSn)
                     ,aes(x=c(1:5),y=MeanKTSn))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Sn (PPM)')+
  ggtitle("KT")+
  ylim(0,0.01)
grid.arrange(Snkggplot,KRSnggplot,KTSnggplot,nrow=1)
###############Sr###############
Srk <- K[K$Parameter == 'Sr',]
SrkSample=data.frame(cbind(Srk$First.Sample,Srk$Second..Sample,Srk$Third.Sample,Srk$Fourth.Sample,Srk$Fifth.Sample))
names(SrkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
SrkSample$First <- as.numeric(SrkSample$First)
SrkSample$Second <- as.numeric(SrkSample$Second)
SrkSample$Third <- as.numeric(SrkSample$Third)
SrkSample$Fourth <- as.numeric(SrkSample$Fourth)
SrkSample$Fifth <- as.numeric(SrkSample$Fifth)
MeanSrk<-c()
for (i in 1:5) {
  MeanSrk[i]=mean(SrkSample[,i])
}
require(gridExtra)
Srkggplot <- ggplot(data=data.frame(MeanSrk)
                    ,aes(x=c(1:5),y=MeanSrk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Sr (PPM)')+
  ggtitle("K")+
  ylim(0,7.5)+
  geom_hline(yintercept=7,color="PURPLE")
KRSr <- KR[KR$Parameter == 'Sr',]
KRSrSample=data.frame(cbind(KRSr$First.Sample,KRSr$Second..Sample,KRSr$Third.Sample,KRSr$Fourth.Sample,KRSr$Fifth.Sample))
names(KRSrSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRSrSample$First <- as.numeric(KRSrSample$First)
KRSrSample$Second <- as.numeric(KRSrSample$Second)
KRSrSample$Third <- as.numeric(KRSrSample$Third)
KRSrSample$Fourth <- as.numeric(KRSrSample$Fourth)
KRSrSample$Fifth <- as.numeric(KRSrSample$Fifth)
MeanKRSr<-c()
for (i in 1:5) {
  MeanKRSr[i]=mean(KRSrSample[,i])
}
KRSrggplot <- ggplot(data=data.frame(MeanKRSr)
                     ,aes(x=c(1:5),y=MeanKRSr))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Sr (PPM)')+
  ggtitle("KR")+
  ylim(0,7.5)+
  geom_hline(yintercept=7,color="PURPLE")
KTSr <- KT[KT$Parameter == 'Sr',]
KTSrSample=data.frame(cbind(KTSr$First.Sample,KTSr$Second..Sample,KTSr$Third.Sample,KTSr$Fourth.Sample,KTSr$Fifth.Sample))
names(KTSrSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTSrSample$First <- as.numeric(KTSrSample$First)
KTSrSample$Second <- as.numeric(KTSrSample$Second)
KTSrSample$Third <- as.numeric(KTSrSample$Third)
KTSrSample$Fourth <- as.numeric(KTSrSample$Fourth)
KTSrSample$Fifth <- as.numeric(KTSrSample$Fifth)
MeanKTSr<-c()
for (i in 1:5) {
  MeanKTSr[i]=mean(KTSrSample[,i])
}
KTSrggplot <- ggplot(data=data.frame(MeanKTSr)
                     ,aes(x=c(1:5),y=MeanKTSr))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Sr (PPM)')+
  ggtitle("KT")+
  ylim(0,7.5)+
  geom_hline(yintercept=7,color="PURPLE")
grid.arrange(Srkggplot,KRSrggplot,KTSrggplot,nrow=1)
###############Ti###############
Tik <- K[K$Parameter == 'Ti',]
TikSample=data.frame(cbind(Tik$First.Sample,Tik$Second..Sample,Tik$Third.Sample,Tik$Fourth.Sample,Tik$Fifth.Sample))
names(TikSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
TikSample$First <- as.numeric(TikSample$First)
TikSample$Second <- as.numeric(TikSample$Second)
TikSample$Third <- as.numeric(TikSample$Third)
TikSample$Fourth <- as.numeric(TikSample$Fourth)
TikSample$Fifth <- as.numeric(TikSample$Fifth)
MeanTik<-c()
for (i in 1:5) {
  MeanTik[i]=mean(TikSample[,i])
}
require(gridExtra)
Tikggplot <- ggplot(data=data.frame(MeanTik)
                    ,aes(x=c(1:5),y=MeanTik))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Ti (PPM)')+
  ggtitle("K")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="PURPLE")
KRTi <- KR[KR$Parameter == 'Ti',]
KRTiSample=data.frame(cbind(KRTi$First.Sample,KRTi$Second..Sample,KRTi$Third.Sample,KRTi$Fourth.Sample,KRTi$Fifth.Sample))
names(KRTiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRTiSample$First <- as.numeric(KRTiSample$First)
KRTiSample$Second <- as.numeric(KRTiSample$Second)
KRTiSample$Third <- as.numeric(KRTiSample$Third)
KRTiSample$Fourth <- as.numeric(KRTiSample$Fourth)
KRTiSample$Fifth <- as.numeric(KRTiSample$Fifth)
MeanKRTi<-c()
for (i in 1:5) {
  MeanKRTi[i]=mean(KRTiSample[,i])
}
KRTiggplot <- ggplot(data=data.frame(MeanKRTi)
                     ,aes(x=c(1:5),y=MeanKRTi))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Ti (PPM)')+
  ggtitle("KR")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="PURPLE")
KTTi <- KT[KT$Parameter == 'Ti',]
KTTiSample=data.frame(cbind(KTTi$First.Sample,KTTi$Second..Sample,KTTi$Third.Sample,KTTi$Fourth.Sample,KTTi$Fifth.Sample))
names(KTTiSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTTiSample$First <- as.numeric(KTTiSample$First)
KTTiSample$Second <- as.numeric(KTTiSample$Second)
KTTiSample$Third <- as.numeric(KTTiSample$Third)
KTTiSample$Fourth <- as.numeric(KTTiSample$Fourth)
KTTiSample$Fifth <- as.numeric(KTTiSample$Fifth)
MeanKTTi<-c()
for (i in 1:5) {
  MeanKTTi[i]=mean(KTTiSample[,i])
}
KTTiggplot <- ggplot(data=data.frame(MeanKTTi)
                     ,aes(x=c(1:5),y=MeanKTTi))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Ti (PPM)')+
  ggtitle("KT")+
  ylim(0,0.15)+
  geom_hline(yintercept=0.1,color="PURPLE")
grid.arrange(Tikggplot,KRTiggplot,KTTiggplot,nrow=1)
###############Tl###############
Tlk <- K[K$Parameter == 'Tl',]
TlkSample=data.frame(cbind(Tlk$First.Sample,Tlk$Second..Sample,Tlk$Third.Sample,Tlk$Fourth.Sample,Tlk$Fifth.Sample))
names(TlkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
TlkSample$First <- as.numeric(TlkSample$First)
TlkSample$Second <- as.numeric(TlkSample$Second)
TlkSample$Third <- as.numeric(TlkSample$Third)
TlkSample$Fourth <- as.numeric(TlkSample$Fourth)
TlkSample$Fifth <- as.numeric(TlkSample$Fifth)
MeanTlk<-c()
for (i in 1:5) {
  MeanTlk[i]=mean(TlkSample[,i])
}
require(gridExtra)
Tlkggplot <- ggplot(data=data.frame(MeanTlk)
                    ,aes(x=c(1:5),y=MeanTlk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Tl (PPM)')+
  ggtitle("K")+
  ylim(0,15)
KRTl <- KR[KR$Parameter == 'Tl',]
KRTlSample=data.frame(cbind(KRTl$First.Sample,KRTl$Second..Sample,KRTl$Third.Sample,KRTl$Fourth.Sample,KRTl$Fifth.Sample))
names(KRTlSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRTlSample$First <- as.numeric(KRTlSample$First)
KRTlSample$Second <- as.numeric(KRTlSample$Second)
KRTlSample$Third <- as.numeric(KRTlSample$Third)
KRTlSample$Fourth <- as.numeric(KRTlSample$Fourth)
KRTlSample$Fifth <- as.numeric(KRTlSample$Fifth)
MeanKRTl<-c()
for (i in 1:5) {
  MeanKRTl[i]=mean(KRTlSample[,i])
}
KRTlggplot <- ggplot(data=data.frame(MeanKRTl)
                     ,aes(x=c(1:5),y=MeanKRTl))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Tl (PPM)')+
  ggtitle("KR")+
  ylim(0,15)
KTTl <- KT[KT$Parameter == 'Tl',]
KTTlSample=data.frame(cbind(KTTl$First.Sample,KTTl$Second..Sample,KTTl$Third.Sample,KTTl$Fourth.Sample,KTTl$Fifth.Sample))
names(KTTlSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTTlSample$First <- as.numeric(KTTlSample$First)
KTTlSample$Second <- as.numeric(KTTlSample$Second)
KTTlSample$Third <- as.numeric(KTTlSample$Third)
KTTlSample$Fourth <- as.numeric(KTTlSample$Fourth)
KTTlSample$Fifth <- as.numeric(KTTlSample$Fifth)
MeanKTTl<-c()
for (i in 1:5) {
  MeanKTTl[i]=mean(KTTlSample[,i])
}
KTTlggplot <- ggplot(data=data.frame(MeanKTTl)
                     ,aes(x=c(1:5),y=MeanKTTl))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Tl (PPM)')+
  ggtitle("KT")+
  ylim(0,15)
grid.arrange(Tlkggplot,KRTlggplot,KTTlggplot,nrow=1)
###############V###############
Vvk <- K[K$Parameter == 'V',]
VvkSample=data.frame(cbind(Vvk$First.Sample,Vvk$Second..Sample,Vvk$Third.Sample,Vvk$Fourth.Sample,Vvk$Fifth.Sample))
names(VvkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
VvkSample$First <- as.numeric(VvkSample$First)
VvkSample$Second <- as.numeric(VvkSample$Second)
VvkSample$Third <- as.numeric(VvkSample$Third)
VvkSample$Fourth <- as.numeric(VvkSample$Fourth)
VvkSample$Fifth <- as.numeric(VvkSample$Fifth)
MeanVvk<-c()
for (i in 1:5) {
  MeanVvk[i]=mean(VvkSample[,i])
}
require(gridExtra)
Vvkggplot <- ggplot(data=data.frame(MeanVvk)
                    ,aes(x=c(1:5),y=MeanVvk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of V (PPM)')+
  ggtitle("K")+
  ylim(0,1)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")
KRVv <- KR[KR$Parameter == 'V',]
KRVvSample=data.frame(cbind(KRVv$First.Sample,KRVv$Second..Sample,KRVv$Third.Sample,KRVv$Fourth.Sample,KRVv$Fifth.Sample))
names(KRVvSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRVvSample$First <- as.numeric(KRVvSample$First)
KRVvSample$Second <- as.numeric(KRVvSample$Second)
KRVvSample$Third <- as.numeric(KRVvSample$Third)
KRVvSample$Fourth <- as.numeric(KRVvSample$Fourth)
KRVvSample$Fifth <- as.numeric(KRVvSample$Fifth)
MeanKRVv<-c()
for (i in 1:5) {
  MeanKRVv[i]=mean(KRVvSample[,i])
}
KRVvggplot <- ggplot(data=data.frame(MeanKRVv)
                     ,aes(x=c(1:5),y=MeanKRVv))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of V (PPM)')+
  ggtitle("KR")+
  ylim(0,1)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")
KTVv <- KT[KT$Parameter == 'V',]
KTVvSample=data.frame(cbind(KTVv$First.Sample,KTVv$Second..Sample,KTVv$Third.Sample,KTVv$Fourth.Sample,KTVv$Fifth.Sample))
names(KTVvSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTVvSample$First <- as.numeric(KTVvSample$First)
KTVvSample$Second <- as.numeric(KTVvSample$Second)
KTVvSample$Third <- as.numeric(KTVvSample$Third)
KTVvSample$Fourth <- as.numeric(KTVvSample$Fourth)
KTVvSample$Fifth <- as.numeric(KTVvSample$Fifth)
MeanKTVv<-c()
for (i in 1:5) {
  MeanKTVv[i]=mean(KTVvSample[,i])
}
KTVvggplot <- ggplot(data=data.frame(MeanKTVv)
                     ,aes(x=c(1:5),y=MeanKTVv))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of V (PPM)')+
  ggtitle("KT")+
  ylim(0,1)+
  geom_hline(yintercept=0.1,color="ORANGE")+
  geom_hline(yintercept=0.1,color="PURPLE")
grid.arrange(Vvkggplot,KRVvggplot,KTVvggplot,nrow=1)
###############Zn###############
Znk <- K[K$Parameter == 'Zn',]
ZnkSample=data.frame(cbind(Znk$First.Sample,Znk$Second..Sample,Znk$Third.Sample,Znk$Fourth.Sample,Znk$Fifth.Sample))
names(ZnkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
ZnkSample$First <- as.numeric(ZnkSample$First)
ZnkSample$Second <- as.numeric(ZnkSample$Second)
ZnkSample$Third <- as.numeric(ZnkSample$Third)
ZnkSample$Fourth <- as.numeric(ZnkSample$Fourth)
ZnkSample$Fifth <- as.numeric(ZnkSample$Fifth)
MeanZnk<-c()
for (i in 1:5) {
  MeanZnk[i]=mean(ZnkSample[,i])
}
require(gridExtra)
Znkggplot <- ggplot(data=data.frame(MeanZnk)
                    ,aes(x=c(1:5),y=MeanZnk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Zn (PPM)')+
  ggtitle("K")+
  ylim(0,10)+
  geom_hline(yintercept=3,color="PURPLE")+
  geom_hline(yintercept=2,color="ORANGE")
KRZn <- KR[KR$Parameter == 'Zn',]
KRZnSample=data.frame(cbind(KRZn$First.Sample,KRZn$Second..Sample,KRZn$Third.Sample,KRZn$Fourth.Sample,KRZn$Fifth.Sample))
names(KRZnSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRZnSample$First <- as.numeric(KRZnSample$First)
KRZnSample$Second <- as.numeric(KRZnSample$Second)
KRZnSample$Third <- as.numeric(KRZnSample$Third)
KRZnSample$Fourth <- as.numeric(KRZnSample$Fourth)
KRZnSample$Fifth <- as.numeric(KRZnSample$Fifth)
MeanKRZn<-c()
for (i in 1:5) {
  MeanKRZn[i]=mean(KRZnSample[,i])
}
KRZnggplot <- ggplot(data=data.frame(MeanKRZn)
                     ,aes(x=c(1:5),y=MeanKRZn))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Zn (PPM)')+
  ggtitle("KR")+
  ylim(0,10)+
  geom_hline(yintercept=3,color="PURPLE")+
  geom_hline(yintercept=2,color="ORANGE")
KTZn <- KT[KT$Parameter == 'Zn',]
KTZnSample=data.frame(cbind(KTZn$First.Sample,KTZn$Second..Sample,KTZn$Third.Sample,KTZn$Fourth.Sample,KTZn$Fifth.Sample))
names(KTZnSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTZnSample$First <- as.numeric(KTZnSample$First)
KTZnSample$Second <- as.numeric(KTZnSample$Second)
KTZnSample$Third <- as.numeric(KTZnSample$Third)
KTZnSample$Fourth <- as.numeric(KTZnSample$Fourth)
KTZnSample$Fifth <- as.numeric(KTZnSample$Fifth)
MeanKTZn<-c()
for (i in 1:5) {
  MeanKTZn[i]=mean(KTZnSample[,i])
}
KTZnggplot <- ggplot(data=data.frame(MeanKTZn)
                     ,aes(x=c(1:5),y=MeanKTZn))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Zn (PPM)')+
  ggtitle("KT")+
  ylim(0,10)+
  geom_hline(yintercept=3,color="PURPLE")+
  geom_hline(yintercept=2,color="ORANGE")
grid.arrange(Znkggplot,KRZnggplot,KTZnggplot,nrow=1)
###############F###############
Ffk <- K[K$Parameter == 'F-',]
FfkSample=data.frame(cbind(Ffk$First.Sample,Ffk$Second..Sample,Ffk$Third.Sample,Ffk$Fourth.Sample,Ffk$Fifth.Sample))
names(FfkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
FfkSample$First <- as.numeric(FfkSample$First)
FfkSample$Second <- as.numeric(FfkSample$Second)
FfkSample$Third <- as.numeric(FfkSample$Third)
FfkSample$Fourth <- as.numeric(FfkSample$Fourth)
FfkSample$Fifth <- as.numeric(FfkSample$Fifth)
MeanFfk<-c()
for (i in 1:5) {
  MeanFfk[i]=mean(FfkSample[,i])
}
require(gridExtra)
Ffkggplot <- ggplot(data=data.frame(MeanFfk)
                    ,aes(x=c(1:5),y=MeanFfk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of F (PPM)')+
  ggtitle("K")+
  ylim(0,3)+
  geom_hline(yintercept=0.5,color="PURPLE")+
  geom_hline(yintercept=1.5,color="PURPLE")+
  geom_hline(yintercept=1,color="ORANGE")
KRFf <- KR[KR$Parameter == 'F-',]
KRFfSample=data.frame(cbind(KRFf$First.Sample,KRFf$Second..Sample,KRFf$Third.Sample,KRFf$Fourth.Sample,KRFf$Fifth.Sample))
names(KRFfSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRFfSample$First <- as.numeric(KRFfSample$First)
KRFfSample$Second <- as.numeric(KRFfSample$Second)
KRFfSample$Third <- as.numeric(KRFfSample$Third)
KRFfSample$Fourth <- as.numeric(KRFfSample$Fourth)
KRFfSample$Fifth <- as.numeric(KRFfSample$Fifth)
MeanKRFf<-c()
for (i in 1:5) {
  MeanKRFf[i]=mean(KRFfSample[,i])
}
KRFfggplot <- ggplot(data=data.frame(MeanKRFf)
                     ,aes(x=c(1:5),y=MeanKRFf))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of F (PPM)')+
  ggtitle("KR")+
  ylim(0,3)+
  geom_hline(yintercept=0.5,color="PURPLE")+
  geom_hline(yintercept=1.5,color="PURPLE")+
  geom_hline(yintercept=1,color="ORANGE")
KTFf <- KT[KT$Parameter == 'F-',]
KTFfSample=data.frame(cbind(KTFf$First.Sample,KTFf$Second..Sample,KTFf$Third.Sample,KTFf$Fourth.Sample,KTFf$Fifth.Sample))
names(KTFfSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTFfSample$First <- as.numeric(KTFfSample$First)
KTFfSample$Second <- as.numeric(KTFfSample$Second)
KTFfSample$Third <- as.numeric(KTFfSample$Third)
KTFfSample$Fourth <- as.numeric(KTFfSample$Fourth)
KTFfSample$Fifth <- as.numeric(KTFfSample$Fifth)
MeanKTFf<-c()
for (i in 1:5) {
  MeanKTFf[i]=mean(KTFfSample[,i])
}
KTFfggplot <- ggplot(data=data.frame(MeanKTFf)
                     ,aes(x=c(1:5),y=MeanKTFf))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of F (PPM)')+
  ggtitle("KT")+
  ylim(0,3)+
  geom_hline(yintercept=0.5,color="PURPLE")+
  geom_hline(yintercept=1.5,color="PURPLE")+
  geom_hline(yintercept=1,color="ORANGE")
grid.arrange(Ffkggplot,KRFfggplot,KTFfggplot,nrow=1)
###############Cl###############
Clk <- K[K$Parameter == 'Cl-',]
ClkSample=data.frame(cbind(Clk$First.Sample,Clk$Second..Sample,Clk$Third.Sample,Clk$Fourth.Sample,Clk$Fifth.Sample))
names(ClkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
ClkSample$First <- as.numeric(ClkSample$First)
ClkSample$Second <- as.numeric(ClkSample$Second)
ClkSample$Third <- as.numeric(ClkSample$Third)
ClkSample$Fourth <- as.numeric(ClkSample$Fourth)
ClkSample$Fifth <- as.numeric(ClkSample$Fifth)
MeanClk<-c()
for (i in 1:5) {
  MeanClk[i]=mean(ClkSample[,i])
}
require(gridExtra)
Clkggplot <- ggplot(data=data.frame(MeanClk)
                    ,aes(x=c(1:5),y=MeanClk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Cl (PPM)')+
  ggtitle("K")+
  ylim(0,1000)+
  geom_hline(yintercept=250,color="PURPLE")+
  geom_hline(yintercept=400,color="PURPLE")+
  geom_hline(yintercept=600,color="ORANGE")
KRCl <- KR[KR$Parameter == 'Cl-',]
KRClSample=data.frame(cbind(KRCl$First.Sample,KRCl$Second..Sample,KRCl$Third.Sample,KRCl$Fourth.Sample,KRCl$Fifth.Sample))
names(KRClSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRClSample$First <- as.numeric(KRClSample$First)
KRClSample$Second <- as.numeric(KRClSample$Second)
KRClSample$Third <- as.numeric(KRClSample$Third)
KRClSample$Fourth <- as.numeric(KRClSample$Fourth)
KRClSample$Fifth <- as.numeric(KRClSample$Fifth)
MeanKRCl<-c()
for (i in 1:5) {
  MeanKRCl[i]=mean(KRClSample[,i])
}
KRClggplot <- ggplot(data=data.frame(MeanKRCl)
                     ,aes(x=c(1:5),y=MeanKRCl))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Cl (PPM)')+
  ggtitle("KR")+
  ylim(0,1000)+
  geom_hline(yintercept=250,color="PURPLE")+
  geom_hline(yintercept=400,color="PURPLE")+
  geom_hline(yintercept=600,color="ORANGE")
KTCl <- KT[KT$Parameter == 'Cl-',]
KTClSample=data.frame(cbind(KTCl$First.Sample,KTCl$Second..Sample,KTCl$Third.Sample,KTCl$Fourth.Sample,KTCl$Fifth.Sample))
names(KTClSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTClSample$First <- as.numeric(KTClSample$First)
KTClSample$Second <- as.numeric(KTClSample$Second)
KTClSample$Third <- as.numeric(KTClSample$Third)
KTClSample$Fourth <- as.numeric(KTClSample$Fourth)
KTClSample$Fifth <- as.numeric(KTClSample$Fifth)
MeanKTCl<-c()
for (i in 1:5) {
  MeanKTCl[i]=mean(KTClSample[,i])
}
KTClggplot <- ggplot(data=data.frame(MeanKTCl)
                     ,aes(x=c(1:5),y=MeanKTCl))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Cl (PPM)')+
  ggtitle("KT")+
  ylim(0,1000)+
  geom_hline(yintercept=250,color="PURPLE")+
  geom_hline(yintercept=400,color="PURPLE")+
  geom_hline(yintercept=600,color="ORANGE")
grid.arrange(Clkggplot,KRClggplot,KTClggplot,nrow=1)
###############Br###############
Brk <- K[K$Parameter == 'Br-',]
BrkSample=data.frame(cbind(Brk$First.Sample,Brk$Second..Sample,Brk$Third.Sample,Brk$Fourth.Sample,Brk$Fifth.Sample))
names(BrkSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
BrkSample$First <- as.numeric(BrkSample$First)
BrkSample$Second <- as.numeric(BrkSample$Second)
BrkSample$Third <- as.numeric(BrkSample$Third)
BrkSample$Fourth <- as.numeric(BrkSample$Fourth)
BrkSample$Fifth <- as.numeric(BrkSample$Fifth)
MeanBrk<-c()
for (i in 1:5) {
  MeanBrk[i]=mean(BrkSample[,i])
}
require(gridExtra)
Brkggplot <- ggplot(data=data.frame(MeanBrk)
                    ,aes(x=c(1:5),y=MeanBrk))+
  geom_line(color="GREEN")+
  labs(x='Month',y='Mean of Br (PPM)')+
  ggtitle("K")+
  ylim(0,75)+
  geom_hline(yintercept=6,color="PURPLE")+
  geom_hline(yintercept=2,color="PURPLE")
KRBr <- KR[KR$Parameter == 'Br-',]
KRBrSample=data.frame(cbind(KRBr$First.Sample,KRBr$Second..Sample,KRBr$Third.Sample,KRBr$Fourth.Sample,KRBr$Fifth.Sample))
names(KRBrSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KRBrSample$First <- as.numeric(KRBrSample$First)
KRBrSample$Second <- as.numeric(KRBrSample$Second)
KRBrSample$Third <- as.numeric(KRBrSample$Third)
KRBrSample$Fourth <- as.numeric(KRBrSample$Fourth)
KRBrSample$Fifth <- as.numeric(KRBrSample$Fifth)
MeanKRBr<-c()
for (i in 1:5) {
  MeanKRBr[i]=mean(KRBrSample[,i])
}
KRBrggplot <- ggplot(data=data.frame(MeanKRBr)
                     ,aes(x=c(1:5),y=MeanKRBr))+
  geom_line(color="RED")+
  labs(x='Month',y='Mean of Br (PPM)')+
  ggtitle("KR")+
  ylim(0,75)+
  geom_hline(yintercept=6,color="PURPLE")+
  geom_hline(yintercept=2,color="PURPLE")
KTBr <- KT[KT$Parameter == 'Br-',]
KTBrSample=data.frame(cbind(KTBr$First.Sample,KTBr$Second..Sample,KTBr$Third.Sample,KTBr$Fourth.Sample,KTBr$Fifth.Sample))
names(KTBrSample)[1:5]=c("First","Second","Third","Fourth","Fifth")
KTBrSample$First <- as.numeric(KTBrSample$First)
KTBrSample$Second <- as.numeric(KTBrSample$Second)
KTBrSample$Third <- as.numeric(KTBrSample$Third)
KTBrSample$Fourth <- as.numeric(KTBrSample$Fourth)
KTBrSample$Fifth <- as.numeric(KTBrSample$Fifth)
MeanKTBr<-c()
for (i in 1:5) {
  MeanKTBr[i]=mean(KTBrSample[,i])
}
KTBrggplot <- ggplot(data=data.frame(MeanKTBr)
                     ,aes(x=c(1:5),y=MeanKTBr))+
  geom_line(color="BLUE")+
  labs(x='Month',y='Mean of Br (PPM)')+
  ggtitle("KT")+
  ylim(0,75)+
  geom_hline(yintercept=6,color="PURPLE")+
  geom_hline(yintercept=2,color="PURPLE")
grid.arrange(Brkggplot,KRBrggplot,KTBrggplot,nrow=1)
######################################
MeTemperature <- c()
SddTemperature <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeTemperature[i] <- mean (as.numeric(KTemperature[i,6:10]))
  SddTemperature[i] <- sd (as.numeric(KTemperature[i,6:10]))
}
(MeanSd1 <- data.frame("Temperature",mean(MeTemperature),mean(SddTemperature)))
names(MeanSd1)<-c("Parameters","Mean","Sd")

MeOpacity <- c()
SddOpacity <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeOpacity[i] <- mean (as.numeric(KOpacity[i,6:10]))
  SddOpacity[i] <- sd (as.numeric(KOpacity[i,6:10]))
}
(MeanSd2 <- data.frame("Opacity",mean(MeOpacity),mean(SddOpacity)))
names(MeanSd2)<-c("Parameters","Mean","Sd")

MeOxygenSolution <- c()
SddOxygenSolution <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeOxygenSolution[i] <- mean (as.numeric(KOxygenSolution[i,6:10]))
  SddOxygenSolution[i] <- sd (as.numeric(KOxygenSolution[i,6:10]))
}
(MeanSd3 <- data.frame("Oxygen Solution",mean(MeOxygenSolution),mean(SddOxygenSolution)))
names(MeanSd3)<-c("Parameters","Mean","Sd")

MeTotalSolubleSolids <- c()
SddTotalSolubleSolids <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeTotalSolubleSolids[i] <- mean (as.numeric(KTotalSolubleSolids[i,6:10]))
  SddTotalSolubleSolids[i] <- sd (as.numeric(KTotalSolubleSolids[i,6:10]))
}
(MeanSd4 <- data.frame("Total Soluble Solids",mean(MeTotalSolubleSolids),mean(SddTotalSolubleSolids)))
names(MeanSd4)<-c("Parameters","Mean","Sd")

MepH <- c()
SddpH <- c()
for (i in 1:(dim(K)[1]/41)) {
  MepH[i] <- mean (as.numeric(KpH[i,6:10]))
  SddpH[i] <- sd (as.numeric(KpH[i,6:10]))
}
(MeanSd5 <- data.frame("pH",mean(MepH),mean(SddpH)))
names(MeanSd5)<-c("Parameters","Mean","Sd")

MeElectricalConductivity <- c()
SddElectricalConductivity <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeElectricalConductivity[i] <- mean (as.numeric(KElectricalConductivity[i,6:10]))
  SddElectricalConductivity[i] <- sd (as.numeric(KElectricalConductivity[i,6:10]))
}
(MeanSd6 <- data.frame("Electrical Conductivity",mean(MeElectricalConductivity),mean(SddElectricalConductivity)))
names(MeanSd6)<-c("Parameters","Mean","Sd")

MeTOC <- c()
SddTOC <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeTOC[i] <- mean (as.numeric(KTOC[i,6:10]))
  SddTOC[i] <- sd (as.numeric(KTOC[i,6:10]))
}
(MeanSd7 <- data.frame("TOC",mean(MeTOC),mean(SddTOC)))
names(MeanSd7)<-c("Parameters","Mean","Sd")

MeAg <- c()
SddAg <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeAg[i] <- mean (as.numeric(KAg[i,6:10]))
  SddAg[i] <- sd (as.numeric(KAg[i,6:10]))
}
(MeanSd8 <- data.frame("Ag",mean(MeAg),mean(SddAg)))
names(MeanSd8)<-c("Parameters","Mean","Sd")

MeAl <- c()
SddAl <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeAl[i] <- mean (as.numeric(KAl[i,6:10]))
  SddAl[i] <- sd (as.numeric(KAl[i,6:10]))
}
(MeanSd9 <- data.frame("Al",mean(MeAl),mean(SddAl)))
names(MeanSd9)<-c("Parameters","Mean","Sd")

MeAs <- c()
SddAs <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeAs[i] <- mean (as.numeric(KAs[i,6:10]))
  SddAs[i] <- sd (as.numeric(KAs[i,6:10]))
}
(MeanSd10 <- data.frame("As",mean(MeAs),mean(SddAs)))
names(MeanSd10)<-c("Parameters","Mean","Sd")

MeB <- c()
SddB <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeB[i] <- mean (as.numeric(KBb[i,6:10]))
  SddB[i] <- sd (as.numeric(KBb[i,6:10]))
}
(MeanSd11 <- data.frame("B",mean(MeB),mean(SddB)))
names(MeanSd11)<-c("Parameters","Mean","Sd")

MeBa <- c()
SddBa <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeBa[i] <- mean (as.numeric(KBa[i,6:10]))
  SddBa[i] <- sd (as.numeric(KBa[i,6:10]))
}
(MeanSd12 <- data.frame("Ba",mean(MeBa),mean(SddBa)))
names(MeanSd12)<-c("Parameters","Mean","Sd")

MeBi <- c()
SddBi <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeBi[i] <- mean (as.numeric(KBi[i,6:10]))
  SddBi[i] <- sd (as.numeric(KBi[i,6:10]))
}
(MeanSd13 <- data.frame("Bi",mean(MeBi),mean(SddBi)))
names(MeanSd13)<-c("Parameters","Mean","Sd")

MeCa <- c()
SddCa <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeCa[i] <- mean (as.numeric(KCa[i,6:10]))
  SddCa[i] <- sd (as.numeric(KCa[i,6:10]))
}
(MeanSd14 <- data.frame("Ca",mean(MeCa),mean(SddCa)))
names(MeanSd14)<-c("Parameters","Mean","Sd")

MeCd <- c()
SddCd <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeCd[i] <- mean (as.numeric(KCd[i,6:10]))
  SddCd[i] <- sd (as.numeric(KCd[i,6:10]))
}
(MeanSd15 <- data.frame("Cd",mean(MeCd),mean(SddCd)))
names(MeanSd15)<-c("Parameters","Mean","Sd")

MeCo <- c()
SddCo <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeCo[i] <- mean (as.numeric(KCo[i,6:10]))
  SddCo[i] <- sd (as.numeric(KCo[i,6:10]))
}
(MeanSd16 <- data.frame("Co",mean(MeCo),mean(SddCo)))
names(MeanSd16)<-c("Parameters","Mean","Sd")

MeCr <- c()
SddCr <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeCr[i] <- mean (as.numeric(KCr[i,6:10]))
  SddCr[i] <- sd (as.numeric(KCr[i,6:10]))
}
(MeanSd17 <- data.frame("Cr",mean(MeCr),mean(SddCr)))
names(MeanSd17)<-c("Parameters","Mean","Sd")

MeCu <- c()
SddCu <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeCu[i] <- mean (as.numeric(KCu[i,6:10]))
  SddCu[i] <- sd (as.numeric(KCu[i,6:10]))
}
(MeanSd18 <- data.frame("Cu",mean(MeCu),mean(SddCu)))
names(MeanSd18)<-c("Parameters","Mean","Sd")

MeFe <- c()
SddFe <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeFe[i] <- mean (as.numeric(KFe[i,6:10]))
  SddFe[i] <- sd (as.numeric(KFe[i,6:10]))
}
(MeanSd19 <- data.frame("Fe",mean(MeFe),mean(SddFe)))
names(MeanSd19)<-c("Parameters","Mean","Sd")

MeHg <- c()
SddHg <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeHg[i] <- mean (as.numeric(KHg[i,6:10]))
  SddHg[i] <- sd (as.numeric(KHg[i,6:10]))
}
(MeanSd20 <- data.frame("Hg",mean(MeHg),mean(SddHg)))
names(MeanSd20)<-c("Parameters","Mean","Sd")

MeK <- c()
SddK <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeK[i] <- mean (as.numeric(KKk[i,6:10]))
  SddK[i] <- sd (as.numeric(KKk[i,6:10]))
}
(MeanSd21 <- data.frame("K",mean(MeK),mean(SddK)))
names(MeanSd21)<-c("Parameters","Mean","Sd")

MeLi <- c()
SddLi <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeLi[i] <- mean (as.numeric(Lik[i,6:10]))
  SddLi[i] <- sd (as.numeric(Lik[i,6:10]))
}
(MeanSd22 <- data.frame("Li",mean(MeLi),mean(SddLi)))
names(MeanSd22)<-c("Parameters","Mean","Sd")

MeMg <- c()
SddMg <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeMg[i] <- mean (as.numeric(Mgk[i,6:10]))
  SddMg[i] <- sd (as.numeric(Mgk[i,6:10]))
}
(MeanSd23 <- data.frame("Mg",mean(MeMg),mean(SddMg)))
names(MeanSd23)<-c("Parameters","Mean","Sd")

MeMn <- c()
SddMn <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeMn[i] <- mean (as.numeric(Mnk[i,6:10]))
  SddMn[i] <- sd (as.numeric(Mnk[i,6:10]))
}
(MeanSd24 <- data.frame("Mn",mean(MeMn),mean(SddMn)))
names(MeanSd24)<-c("Parameters","Mean","Sd")

MeMo <- c()
SddMo <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeMo[i] <- mean (as.numeric(Mok[i,6:10]))
  SddMo[i] <- sd (as.numeric(Mok[i,6:10]))
}
(MeanSd25 <- data.frame("Mo",mean(MeMo),mean(SddMo)))
names(MeanSd25)<-c("Parameters","Mean","Sd")

MeNa <- c()
SddNa <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeNa[i] <- mean (as.numeric(Nak[i,6:10]))
  SddNa[i] <- sd (as.numeric(Nak[i,6:10]))
}
(MeanSd26 <- data.frame("Na",mean(MeNa),mean(SddNa)))
names(MeanSd26)<-c("Parameters","Mean","Sd")

MeNi <- c()
SddNi <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeNi[i] <- mean (as.numeric(Nik[i,6:10]))
  SddNi[i] <- sd (as.numeric(Nik[i,6:10]))
}
(MeanSd27 <- data.frame("Ni",mean(MeNi),mean(SddNi)))
names(MeanSd27)<-c("Parameters","Mean","Sd")

MePb <- c()
SddPb <- c()
for (i in 1:(dim(K)[1]/41)) {
  MePb[i] <- mean (as.numeric(Pbk[i,6:10]))
  SddPb[i] <- sd (as.numeric(Pbk[i,6:10]))
}
(MeanSd28 <- data.frame("Pb",mean(MePb),mean(SddPb)))
names(MeanSd28)<-c("Parameters","Mean","Sd")

MeP <- c()
SddP <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeP[i] <- mean (as.numeric(Ppk[i,6:10]))
  SddP[i] <- sd (as.numeric(Ppk[i,6:10]))
}
(MeanSd29 <- data.frame("P",mean(MeP),mean(SddP)))
names(MeanSd29)<-c("Parameters","Mean","Sd")

MeSb <- c()
SddSb <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeSb[i] <- mean (as.numeric(Sbk[i,6:10]))
  SddSb[i] <- sd (as.numeric(Sbk[i,6:10]))
}
(MeanSd30 <- data.frame("Sb",mean(MeSb),mean(SddSb)))
names(MeanSd30)<-c("Parameters","Mean","Sd")

MeSe <- c()
SddSe <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeSe[i] <- mean (as.numeric(Sek[i,6:10]))
  SddSe[i] <- sd (as.numeric(Sek[i,6:10]))
}
(MeanSd31 <- data.frame("Se",mean(MeSe),mean(SddSe)))
names(MeanSd31)<-c("Parameters","Mean","Sd")

MeSi <- c()
SddSi <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeSi[i] <- mean (as.numeric(Sik[i,6:10]))
  SddSi[i] <- sd (as.numeric(Sik[i,6:10]))
}
(MeanSd32 <- data.frame("Si",mean(MeSi),mean(SddSi)))
names(MeanSd32)<-c("Parameters","Mean","Sd")

MeSn <- c()
SddSn <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeSn[i] <- mean (as.numeric(Snk[i,6:10]))
  SddSn[i] <- sd (as.numeric(Snk[i,6:10]))
}
(MeanSd33 <- data.frame("Sn",mean(MeSn),mean(SddSn)))
names(MeanSd33)<-c("Parameters","Mean","Sd")

MeSr <- c()
SddSr <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeSr[i] <- mean (as.numeric(Srk[i,6:10]))
  SddSr[i] <- sd (as.numeric(Srk[i,6:10]))
}
(MeanSd34 <- data.frame("Sr",mean(MeSr),mean(SddSr)))
names(MeanSd34)<-c("Parameters","Mean","Sd")

MeTi <- c()
SddTi <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeTi[i] <- mean (as.numeric(Tik[i,6:10]))
  SddTi[i] <- sd (as.numeric(Tik[i,6:10]))
}
(MeanSd35 <- data.frame("Ti",mean(MeTi),mean(SddTi)))
names(MeanSd35)<-c("Parameters","Mean","Sd")

MeTl <- c()
SddTl <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeTl[i] <- mean (as.numeric(Tlk[i,6:10]))
  SddTl[i] <- sd (as.numeric(Tlk[i,6:10]))
}
(MeanSd36 <- data.frame("Tl",mean(MeTl),mean(SddTl)))
names(MeanSd36)<-c("Parameters","Mean","Sd")

MeV <- c()
SddV <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeV[i] <- mean (as.numeric(Vvk[i,6:10]))
  SddV[i] <- sd (as.numeric(Vvk[i,6:10]))
}
(MeanSd37 <- data.frame("V",mean(MeV),mean(SddV)))
names(MeanSd37)<-c("Parameters","Mean","Sd")

MeZn <- c()
SddZn <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeZn[i] <- mean (as.numeric(Znk[i,6:10]))
  SddZn[i] <- sd (as.numeric(Znk[i,6:10]))
}
(MeanSd38 <- data.frame("Zn",mean(MeZn),mean(SddZn)))
names(MeanSd38)<-c("Parameters","Mean","Sd")

MeF <- c()
SddF <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeF[i] <- mean (as.numeric(Ffk[i,6:10]))
  SddF[i] <- sd (as.numeric(Ffk[i,6:10]))
}
(MeanSd39 <- data.frame("F",mean(MeF),mean(SddF)))
names(MeanSd39)<-c("Parameters","Mean","Sd")

MeCl <- c()
SddCl <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeCl[i] <- mean (as.numeric(Clk[i,6:10]))
  SddCl[i] <- sd (as.numeric(Clk[i,6:10]))
}
(MeanSd40 <- data.frame("Cl",mean(MeCl),mean(SddCl)))
names(MeanSd40)<-c("Parameters","Mean","Sd")

MeBr <- c()
SddBr <- c()
for (i in 1:(dim(K)[1]/41)) {
  MeBr[i] <- mean (as.numeric(Brk[i,6:10]))
  SddBr[i] <- sd (as.numeric(Brk[i,6:10]))
}
(MeanSd41 <- data.frame("Br",mean(MeBr),mean(SddBr)))
names(MeanSd41)<-c("Parameters","Mean","Sd")
(MeanSd <- rbind(MeanSd1,MeanSd2,MeanSd3,MeanSd4,MeanSd5,MeanSd6,MeanSd7,MeanSd8,
                MeanSd9,MeanSd10,MeanSd11,MeanSd12,MeanSd13,MeanSd14,MeanSd15,MeanSd16,
                MeanSd17,MeanSd18,MeanSd19,MeanSd20,MeanSd21,MeanSd22,MeanSd23,MeanSd24,
                MeanSd25,MeanSd26,MeanSd27,MeanSd28,MeanSd29,MeanSd30,MeanSd31,MeanSd32,
                MeanSd33,MeanSd34,MeanSd35,MeanSd36,MeanSd37,MeanSd38,MeanSd39,MeanSd40,
                MeanSd41))
MeTemperature <- c()
SddTemperature <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeTemperature[i] <- mean (as.numeric(KRTemperature[i,6:10]))
  SddTemperature[i] <- sd (as.numeric(KRTemperature[i,6:10]))
}
(MeanSd1 <- data.frame("Temperature",mean(MeTemperature),mean(SddTemperature)))
names(MeanSd1)<-c("Parameters","Mean","Sd")

MeOpacity <- c()
SddOpacity <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeOpacity[i] <- mean (as.numeric(KROpacity[i,6:10]))
  SddOpacity[i] <- sd (as.numeric(KROpacity[i,6:10]))
}
(MeanSd2 <- data.frame("Opacity",mean(MeOpacity),mean(SddOpacity)))
names(MeanSd2)<-c("Parameters","Mean","Sd")

MeOxygenSolution <- c()
SddOxygenSolution <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeOxygenSolution[i] <- mean (as.numeric(KROxygenSolution[i,6:10]))
  SddOxygenSolution[i] <- sd (as.numeric(KROxygenSolution[i,6:10]))
}
(MeanSd3 <- data.frame("Oxygen Solution",mean(MeOxygenSolution),mean(SddOxygenSolution)))
names(MeanSd3)<-c("Parameters","Mean","Sd")

MeTotalSolubleSolids <- c()
SddTotalSolubleSolids <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeTotalSolubleSolids[i] <- mean (as.numeric(KRTotalSolubleSolids[i,6:10]))
  SddTotalSolubleSolids[i] <- sd (as.numeric(KRTotalSolubleSolids[i,6:10]))
}
(MeanSd4 <- data.frame("Total Soluble Solids",mean(MeTotalSolubleSolids),mean(SddTotalSolubleSolids)))
names(MeanSd4)<-c("Parameters","Mean","Sd")

MepH <- c()
SddpH <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MepH[i] <- mean (as.numeric(KRpH[i,6:10]))
  SddpH[i] <- sd (as.numeric(KRpH[i,6:10]))
}
(MeanSd5 <- data.frame("pH",mean(MepH),mean(SddpH)))
names(MeanSd5)<-c("Parameters","Mean","Sd")

MeElectricalConductivity <- c()
SddElectricalConductivity <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeElectricalConductivity[i] <- mean (as.numeric(KRElectricalConductivity[i,6:10]))
  SddElectricalConductivity[i] <- sd (as.numeric(KRElectricalConductivity[i,6:10]))
}
(MeanSd6 <- data.frame("Electrical Conductivity",mean(MeElectricalConductivity),mean(SddElectricalConductivity)))
names(MeanSd6)<-c("Parameters","Mean","Sd")

MeTOC <- c()
SddTOC <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeTOC[i] <- mean (as.numeric(KRTOC[i,6:10]))
  SddTOC[i] <- sd (as.numeric(KRTOC[i,6:10]))
}
(MeanSd7 <- data.frame("TOC",mean(MeTOC),mean(SddTOC)))
names(MeanSd7)<-c("Parameters","Mean","Sd")

MeAg <- c()
SddAg <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeAg[i] <- mean (as.numeric(KRAg[i,6:10]))
  SddAg[i] <- sd (as.numeric(KRAg[i,6:10]))
}
(MeanSd8 <- data.frame("Ag",mean(MeAg),mean(SddAg)))
names(MeanSd8)<-c("Parameters","Mean","Sd")

MeAl <- c()
SddAl <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeAl[i] <- mean (as.numeric(KRAl[i,6:10]))
  SddAl[i] <- sd (as.numeric(KRAl[i,6:10]))
}
(MeanSd9 <- data.frame("Al",mean(MeAl),mean(SddAl)))
names(MeanSd9)<-c("Parameters","Mean","Sd")

MeAs <- c()
SddAs <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeAs[i] <- mean (as.numeric(KRAs[i,6:10]))
  SddAs[i] <- sd (as.numeric(KRAs[i,6:10]))
}
(MeanSd10 <- data.frame("As",mean(MeAs),mean(SddAs)))
names(MeanSd10)<-c("Parameters","Mean","Sd")

MeB <- c()
SddB <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeB[i] <- mean (as.numeric(KRBb[i,6:10]))
  SddB[i] <- sd (as.numeric(KRBb[i,6:10]))
}
(MeanSd11 <- data.frame("B",mean(MeB),mean(SddB)))
names(MeanSd11)<-c("Parameters","Mean","Sd")

MeBa <- c()
SddBa <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeBa[i] <- mean (as.numeric(KRBa[i,6:10]))
  SddBa[i] <- sd (as.numeric(KRBa[i,6:10]))
}
(MeanSd12 <- data.frame("Ba",mean(MeBa),mean(SddBa)))
names(MeanSd12)<-c("Parameters","Mean","Sd")

MeBi <- c()
SddBi <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeBi[i] <- mean (as.numeric(KRBi[i,6:10]))
  SddBi[i] <- sd (as.numeric(KRBi[i,6:10]))
}
(MeanSd13 <- data.frame("Bi",mean(MeBi),mean(SddBi)))
names(MeanSd13)<-c("Parameters","Mean","Sd")

MeCa <- c()
SddCa <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeCa[i] <- mean (as.numeric(KRCa[i,6:10]))
  SddCa[i] <- sd (as.numeric(KRCa[i,6:10]))
}
(MeanSd14 <- data.frame("Ca",mean(MeCa),mean(SddCa)))
names(MeanSd14)<-c("Parameters","Mean","Sd")

MeCd <- c()
SddCd <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeCd[i] <- mean (as.numeric(KRCd[i,6:10]))
  SddCd[i] <- sd (as.numeric(KRCd[i,6:10]))
}
(MeanSd15 <- data.frame("Cd",mean(MeCd),mean(SddCd)))
names(MeanSd15)<-c("Parameters","Mean","Sd")

MeCo <- c()
SddCo <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeCo[i] <- mean (as.numeric(KRCo[i,6:10]))
  SddCo[i] <- sd (as.numeric(KRCo[i,6:10]))
}
(MeanSd16 <- data.frame("Co",mean(MeCo),mean(SddCo)))
names(MeanSd16)<-c("Parameters","Mean","Sd")

MeCr <- c()
SddCr <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeCr[i] <- mean (as.numeric(KRCr[i,6:10]))
  SddCr[i] <- sd (as.numeric(KRCr[i,6:10]))
}
(MeanSd17 <- data.frame("Cr",mean(MeCr),mean(SddCr)))
names(MeanSd17)<-c("Parameters","Mean","Sd")

MeCu <- c()
SddCu <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeCu[i] <- mean (as.numeric(KRCu[i,6:10]))
  SddCu[i] <- sd (as.numeric(KRCu[i,6:10]))
}
(MeanSd18 <- data.frame("Cu",mean(MeCu),mean(SddCu)))
names(MeanSd18)<-c("Parameters","Mean","Sd")

MeFe <- c()
SddFe <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeFe[i] <- mean (as.numeric(KRFe[i,6:10]))
  SddFe[i] <- sd (as.numeric(KRFe[i,6:10]))
}
(MeanSd19 <- data.frame("Fe",mean(MeFe),mean(SddFe)))
names(MeanSd19)<-c("Parameters","Mean","Sd")

MeHg <- c()
SddHg <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeHg[i] <- mean (as.numeric(KRHg[i,6:10]))
  SddHg[i] <- sd (as.numeric(KRHg[i,6:10]))
}
(MeanSd20 <- data.frame("Hg",mean(MeHg),mean(SddHg)))
names(MeanSd20)<-c("Parameters","Mean","Sd")

MeKR <- c()
SddKR <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeKR[i] <- mean (as.numeric(KRKk[i,6:10]))
  SddKR[i] <- sd (as.numeric(KRKk[i,6:10]))
}
(MeanSd21 <- data.frame("KR",mean(MeKR),mean(SddKR)))
names(MeanSd21)<-c("Parameters","Mean","Sd")

MeLi <- c()
SddLi <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeLi[i] <- mean (as.numeric(KRLi[i,6:10]))
  SddLi[i] <- sd (as.numeric(KRLi[i,6:10]))
}
(MeanSd22 <- data.frame("Li",mean(MeLi),mean(SddLi)))
names(MeanSd22)<-c("Parameters","Mean","Sd")

MeMg <- c()
SddMg <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeMg[i] <- mean (as.numeric(KRMg[i,6:10]))
  SddMg[i] <- sd (as.numeric(KRMg[i,6:10]))
}
(MeanSd23 <- data.frame("Mg",mean(MeMg),mean(SddMg)))
names(MeanSd23)<-c("Parameters","Mean","Sd")

MeMn <- c()
SddMn <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeMn[i] <- mean (as.numeric(KRMn[i,6:10]))
  SddMn[i] <- sd (as.numeric(KRMn[i,6:10]))
}
(MeanSd24 <- data.frame("Mn",mean(MeMn),mean(SddMn)))
names(MeanSd24)<-c("Parameters","Mean","Sd")

MeMo <- c()
SddMo <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeMo[i] <- mean (as.numeric(KRMo[i,6:10]))
  SddMo[i] <- sd (as.numeric(KRMo[i,6:10]))
}
(MeanSd25 <- data.frame("Mo",mean(MeMo),mean(SddMo)))
names(MeanSd25)<-c("Parameters","Mean","Sd")

MeNa <- c()
SddNa <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeNa[i] <- mean (as.numeric(KRNa[i,6:10]))
  SddNa[i] <- sd (as.numeric(KRNa[i,6:10]))
}
(MeanSd26 <- data.frame("Na",mean(MeNa),mean(SddNa)))
names(MeanSd26)<-c("Parameters","Mean","Sd")

MeNi <- c()
SddNi <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeNi[i] <- mean (as.numeric(KRNi[i,6:10]))
  SddNi[i] <- sd (as.numeric(KRNi[i,6:10]))
}
(MeanSd27 <- data.frame("Ni",mean(MeNi),mean(SddNi)))
names(MeanSd27)<-c("Parameters","Mean","Sd")

MePb <- c()
SddPb <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MePb[i] <- mean (as.numeric(KRPb[i,6:10]))
  SddPb[i] <- sd (as.numeric(KRPb[i,6:10]))
}
(MeanSd28 <- data.frame("Pb",mean(MePb),mean(SddPb)))
names(MeanSd28)<-c("Parameters","Mean","Sd")

MeP <- c()
SddP <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeP[i] <- mean (as.numeric(KRPb[i,6:10]))
  SddP[i] <- sd (as.numeric(KRPp[i,6:10]))
}
(MeanSd29 <- data.frame("P",mean(MeP),mean(SddP)))
names(MeanSd29)<-c("Parameters","Mean","Sd")

MeSb <- c()
SddSb <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeSb[i] <- mean (as.numeric(KRSb[i,6:10]))
  SddSb[i] <- sd (as.numeric(KRSb[i,6:10]))
}
(MeanSd30 <- data.frame("Sb",mean(MeSb),mean(SddSb)))
names(MeanSd30)<-c("Parameters","Mean","Sd")

MeSe <- c()
SddSe <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeSe[i] <- mean (as.numeric(KRSe[i,6:10]))
  SddSe[i] <- sd (as.numeric(KRSe[i,6:10]))
}
(MeanSd31 <- data.frame("Se",mean(MeSe),mean(SddSe)))
names(MeanSd31)<-c("Parameters","Mean","Sd")

MeSi <- c()
SddSi <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeSi[i] <- mean (as.numeric(KRSi[i,6:10]))
  SddSi[i] <- sd (as.numeric(KRSi[i,6:10]))
}
(MeanSd32 <- data.frame("Si",mean(MeSi),mean(SddSi)))
names(MeanSd32)<-c("Parameters","Mean","Sd")

MeSn <- c()
SddSn <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeSn[i] <- mean (as.numeric(KRSn[i,6:10]))
  SddSn[i] <- sd (as.numeric(KRSn[i,6:10]))
}
(MeanSd33 <- data.frame("Sn",mean(MeSn),mean(SddSn)))
names(MeanSd33)<-c("Parameters","Mean","Sd")

MeSr <- c()
SddSr <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeSr[i] <- mean (as.numeric(KRSr[i,6:10]))
  SddSr[i] <- sd (as.numeric(KRSr[i,6:10]))
}
(MeanSd34 <- data.frame("Sr",mean(MeSr),mean(SddSr)))
names(MeanSd34)<-c("Parameters","Mean","Sd")

MeTi <- c()
SddTi <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeTi[i] <- mean (as.numeric(KRTi[i,6:10]))
  SddTi[i] <- sd (as.numeric(KRTi[i,6:10]))
}
(MeanSd35 <- data.frame("Ti",mean(MeTi),mean(SddTi)))
names(MeanSd35)<-c("Parameters","Mean","Sd")

MeTl <- c()
SddTl <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeTl[i] <- mean (as.numeric(KRTl[i,6:10]))
  SddTl[i] <- sd (as.numeric(KRTl[i,6:10]))
}
(MeanSd36 <- data.frame("Tl",mean(MeTl),mean(SddTl)))
names(MeanSd36)<-c("Parameters","Mean","Sd")

MeV <- c()
SddV <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeV[i] <- mean (as.numeric(KRVv[i,6:10]))
  SddV[i] <- sd (as.numeric(KRVv[i,6:10]))
}
(MeanSd37 <- data.frame("V",mean(MeV),mean(SddV)))
names(MeanSd37)<-c("Parameters","Mean","Sd")

MeZn <- c()
SddZn <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeZn[i] <- mean (as.numeric(KRZn[i,6:10]))
  SddZn[i] <- sd (as.numeric(KRZn[i,6:10]))
}
(MeanSd38 <- data.frame("Zn",mean(MeZn),mean(SddZn)))
names(MeanSd38)<-c("Parameters","Mean","Sd")

MeF <- c()
SddF <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeF[i] <- mean (as.numeric(KRFf[i,6:10]))
  SddF[i] <- sd (as.numeric(KRFf[i,6:10]))
}
(MeanSd39 <- data.frame("F",mean(MeF),mean(SddF)))
names(MeanSd39)<-c("Parameters","Mean","Sd")

MeCl <- c()
SddCl <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeCl[i] <- mean (as.numeric(KRCl[i,6:10]))
  SddCl[i] <- sd (as.numeric(KRCl[i,6:10]))
}
(MeanSd40 <- data.frame("Cl",mean(MeCl),mean(SddCl)))
names(MeanSd40)<-c("Parameters","Mean","Sd")

MeBr <- c()
SddBr <- c()
for (i in 1:(dim(KR)[1]/41)) {
  MeBr[i] <- mean (as.numeric(KRBr[i,6:10]))
  SddBr[i] <- sd (as.numeric(KRBr[i,6:10]))
}
(MeanSd41 <- data.frame("Br",mean(MeBr),mean(SddBr)))
names(MeanSd41)<-c("Parameters","Mean","Sd")
(MeanSd <- rbind(MeanSd1,MeanSd2,MeanSd3,MeanSd4,MeanSd5,MeanSd6,MeanSd7,MeanSd8,
                 MeanSd9,MeanSd10,MeanSd11,MeanSd12,MeanSd13,MeanSd14,MeanSd15,MeanSd16,
                 MeanSd17,MeanSd18,MeanSd19,MeanSd20,MeanSd21,MeanSd22,MeanSd23,MeanSd24,
                 MeanSd25,MeanSd26,MeanSd27,MeanSd28,MeanSd29,MeanSd30,MeanSd31,MeanSd32,
                 MeanSd33,MeanSd34,MeanSd35,MeanSd36,MeanSd37,MeanSd38,MeanSd39,MeanSd40,
                 MeanSd41))
MeTemperature <- c()
SddTemperature <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeTemperature[i] <- mean (as.numeric(KTTemperature[i,6:10]))
  SddTemperature[i] <- sd (as.numeric(KTTemperature[i,6:10]))
}
(MeanSd1 <- data.frame("Temperature",mean(MeTemperature),mean(SddTemperature)))
names(MeanSd1)<-c("Parameters","Mean","Sd")

MeOpacity <- c()
SddOpacity <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeOpacity[i] <- mean (as.numeric(KTOpacity[i,6:10]))
  SddOpacity[i] <- sd (as.numeric(KTOpacity[i,6:10]))
}
(MeanSd2 <- data.frame("Opacity",mean(MeOpacity),mean(SddOpacity)))
names(MeanSd2)<-c("Parameters","Mean","Sd")

MeOxygenSolution <- c()
SddOxygenSolution <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeOxygenSolution[i] <- mean (as.numeric(KTOxygenSolution[i,6:10]))
  SddOxygenSolution[i] <- sd (as.numeric(KTOxygenSolution[i,6:10]))
}
(MeanSd3 <- data.frame("Oxygen Solution",mean(MeOxygenSolution),mean(SddOxygenSolution)))
names(MeanSd3)<-c("Parameters","Mean","Sd")

MeTotalSolubleSolids <- c()
SddTotalSolubleSolids <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeTotalSolubleSolids[i] <- mean (as.numeric(KTTotalSolubleSolids[i,6:10]))
  SddTotalSolubleSolids[i] <- sd (as.numeric(KTTotalSolubleSolids[i,6:10]))
}
(MeanSd4 <- data.frame("Total Soluble Solids",mean(MeTotalSolubleSolids),mean(SddTotalSolubleSolids)))
names(MeanSd4)<-c("Parameters","Mean","Sd")

MepH <- c()
SddpH <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MepH[i] <- mean (as.numeric(KTpH[i,6:10]))
  SddpH[i] <- sd (as.numeric(KTpH[i,6:10]))
}
(MeanSd5 <- data.frame("pH",mean(MepH),mean(SddpH)))
names(MeanSd5)<-c("Parameters","Mean","Sd")

MeElectricalConductivity <- c()
SddElectricalConductivity <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeElectricalConductivity[i] <- mean (as.numeric(KTElectricalConductivity[i,6:10]))
  SddElectricalConductivity[i] <- sd (as.numeric(KTElectricalConductivity[i,6:10]))
}
(MeanSd6 <- data.frame("Electrical Conductivity",mean(MeElectricalConductivity),mean(SddElectricalConductivity)))
names(MeanSd6)<-c("Parameters","Mean","Sd")

MeTOC <- c()
SddTOC <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeTOC[i] <- mean (as.numeric(KTTOC[i,6:10]))
  SddTOC[i] <- sd (as.numeric(KTTOC[i,6:10]))
}
(MeanSd7 <- data.frame("TOC",mean(MeTOC),mean(SddTOC)))
names(MeanSd7)<-c("Parameters","Mean","Sd")

MeAg <- c()
SddAg <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeAg[i] <- mean (as.numeric(KTAg[i,6:10]))
  SddAg[i] <- sd (as.numeric(KTAg[i,6:10]))
}
(MeanSd8 <- data.frame("Ag",mean(MeAg),mean(SddAg)))
names(MeanSd8)<-c("Parameters","Mean","Sd")

MeAl <- c()
SddAl <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeAl[i] <- mean (as.numeric(KTAl[i,6:10]))
  SddAl[i] <- sd (as.numeric(KTAl[i,6:10]))
}
(MeanSd9 <- data.frame("Al",mean(MeAl),mean(SddAl)))
names(MeanSd9)<-c("Parameters","Mean","Sd")

MeAs <- c()
SddAs <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeAs[i] <- mean (as.numeric(KTAs[i,6:10]))
  SddAs[i] <- sd (as.numeric(KTAs[i,6:10]))
}
(MeanSd10 <- data.frame("As",mean(MeAs),mean(SddAs)))
names(MeanSd10)<-c("Parameters","Mean","Sd")

MeB <- c()
SddB <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeB[i] <- mean (as.numeric(KTBb[i,6:10]))
  SddB[i] <- sd (as.numeric(KTBb[i,6:10]))
}
(MeanSd11 <- data.frame("B",mean(MeB),mean(SddB)))
names(MeanSd11)<-c("Parameters","Mean","Sd")

MeBa <- c()
SddBa <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeBa[i] <- mean (as.numeric(KTBa[i,6:10]))
  SddBa[i] <- sd (as.numeric(KTBa[i,6:10]))
}
(MeanSd12 <- data.frame("Ba",mean(MeBa),mean(SddBa)))
names(MeanSd12)<-c("Parameters","Mean","Sd")

MeBi <- c()
SddBi <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeBi[i] <- mean (as.numeric(KTBi[i,6:10]))
  SddBi[i] <- sd (as.numeric(KTBi[i,6:10]))
}
(MeanSd13 <- data.frame("Bi",mean(MeBi),mean(SddBi)))
names(MeanSd13)<-c("Parameters","Mean","Sd")

MeCa <- c()
SddCa <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeCa[i] <- mean (as.numeric(KTCa[i,6:10]))
  SddCa[i] <- sd (as.numeric(KTCa[i,6:10]))
}
(MeanSd14 <- data.frame("Ca",mean(MeCa),mean(SddCa)))
names(MeanSd14)<-c("Parameters","Mean","Sd")

MeCd <- c()
SddCd <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeCd[i] <- mean (as.numeric(KTCd[i,6:10]))
  SddCd[i] <- sd (as.numeric(KTCd[i,6:10]))
}
(MeanSd15 <- data.frame("Cd",mean(MeCd),mean(SddCd)))
names(MeanSd15)<-c("Parameters","Mean","Sd")

MeCo <- c()
SddCo <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeCo[i] <- mean (as.numeric(KTCo[i,6:10]))
  SddCo[i] <- sd (as.numeric(KTCo[i,6:10]))
}
(MeanSd16 <- data.frame("Co",mean(MeCo),mean(SddCo)))
names(MeanSd16)<-c("Parameters","Mean","Sd")

MeCr <- c()
SddCr <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeCr[i] <- mean (as.numeric(KTCr[i,6:10]))
  SddCr[i] <- sd (as.numeric(KTCr[i,6:10]))
}
(MeanSd17 <- data.frame("Cr",mean(MeCr),mean(SddCr)))
names(MeanSd17)<-c("Parameters","Mean","Sd")

MeCu <- c()
SddCu <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeCu[i] <- mean (as.numeric(KTCu[i,6:10]))
  SddCu[i] <- sd (as.numeric(KTCu[i,6:10]))
}
(MeanSd18 <- data.frame("Cu",mean(MeCu),mean(SddCu)))
names(MeanSd18)<-c("Parameters","Mean","Sd")

MeFe <- c()
SddFe <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeFe[i] <- mean (as.numeric(KTFe[i,6:10]))
  SddFe[i] <- sd (as.numeric(KTFe[i,6:10]))
}
(MeanSd19 <- data.frame("Fe",mean(MeFe),mean(SddFe)))
names(MeanSd19)<-c("Parameters","Mean","Sd")

MeHg <- c()
SddHg <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeHg[i] <- mean (as.numeric(KTHg[i,6:10]))
  SddHg[i] <- sd (as.numeric(KTHg[i,6:10]))
}
(MeanSd20 <- data.frame("Hg",mean(MeHg),mean(SddHg)))
names(MeanSd20)<-c("Parameters","Mean","Sd")

MeKT <- c()
SddKT <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeKT[i] <- mean (as.numeric(KTKk[i,6:10]))
  SddKT[i] <- sd (as.numeric(KTKk[i,6:10]))
}
(MeanSd21 <- data.frame("KT",mean(MeKT),mean(SddKT)))
names(MeanSd21)<-c("Parameters","Mean","Sd")

MeLi <- c()
SddLi <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeLi[i] <- mean (as.numeric(KTLi[i,6:10]))
  SddLi[i] <- sd (as.numeric(KTLi[i,6:10]))
}
(MeanSd22 <- data.frame("Li",mean(MeLi),mean(SddLi)))
names(MeanSd22)<-c("Parameters","Mean","Sd")

MeMg <- c()
SddMg <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeMg[i] <- mean (as.numeric(KTMg[i,6:10]))
  SddMg[i] <- sd (as.numeric(KTMg[i,6:10]))
}
(MeanSd23 <- data.frame("Mg",mean(MeMg),mean(SddMg)))
names(MeanSd23)<-c("Parameters","Mean","Sd")

MeMn <- c()
SddMn <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeMn[i] <- mean (as.numeric(KTMn[i,6:10]))
  SddMn[i] <- sd (as.numeric(KTMn[i,6:10]))
}
(MeanSd24 <- data.frame("Mn",mean(MeMn),mean(SddMn)))
names(MeanSd24)<-c("Parameters","Mean","Sd")

MeMo <- c()
SddMo <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeMo[i] <- mean (as.numeric(KTMo[i,6:10]))
  SddMo[i] <- sd (as.numeric(KTMo[i,6:10]))
}
(MeanSd25 <- data.frame("Mo",mean(MeMo),mean(SddMo)))
names(MeanSd25)<-c("Parameters","Mean","Sd")

MeNa <- c()
SddNa <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeNa[i] <- mean (as.numeric(KTNa[i,6:10]))
  SddNa[i] <- sd (as.numeric(KTNa[i,6:10]))
}
(MeanSd26 <- data.frame("Na",mean(MeNa),mean(SddNa)))
names(MeanSd26)<-c("Parameters","Mean","Sd")

MeNi <- c()
SddNi <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeNi[i] <- mean (as.numeric(KTNi[i,6:10]))
  SddNi[i] <- sd (as.numeric(KTNi[i,6:10]))
}
(MeanSd27 <- data.frame("Ni",mean(MeNi),mean(SddNi)))
names(MeanSd27)<-c("Parameters","Mean","Sd")

MePb <- c()
SddPb <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MePb[i] <- mean (as.numeric(KTPb[i,6:10]))
  SddPb[i] <- sd (as.numeric(KTPb[i,6:10]))
}
(MeanSd28 <- data.frame("Pb",mean(MePb),mean(SddPb)))
names(MeanSd28)<-c("Parameters","Mean","Sd")

MeP <- c()
SddP <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeP[i] <- mean (as.numeric(KTPb[i,6:10]))
  SddP[i] <- sd (as.numeric(KTPp[i,6:10]))
}
(MeanSd29 <- data.frame("P",mean(MeP),mean(SddP)))
names(MeanSd29)<-c("Parameters","Mean","Sd")

MeSb <- c()
SddSb <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeSb[i] <- mean (as.numeric(KTSb[i,6:10]))
  SddSb[i] <- sd (as.numeric(KTSb[i,6:10]))
}
(MeanSd30 <- data.frame("Sb",mean(MeSb),mean(SddSb)))
names(MeanSd30)<-c("Parameters","Mean","Sd")

MeSe <- c()
SddSe <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeSe[i] <- mean (as.numeric(KTSe[i,6:10]))
  SddSe[i] <- sd (as.numeric(KTSe[i,6:10]))
}
(MeanSd31 <- data.frame("Se",mean(MeSe),mean(SddSe)))
names(MeanSd31)<-c("Parameters","Mean","Sd")

MeSi <- c()
SddSi <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeSi[i] <- mean (as.numeric(KTSi[i,6:10]))
  SddSi[i] <- sd (as.numeric(KTSi[i,6:10]))
}
(MeanSd32 <- data.frame("Si",mean(MeSi),mean(SddSi)))
names(MeanSd32)<-c("Parameters","Mean","Sd")

MeSn <- c()
SddSn <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeSn[i] <- mean (as.numeric(KTSn[i,6:10]))
  SddSn[i] <- sd (as.numeric(KTSn[i,6:10]))
}
(MeanSd33 <- data.frame("Sn",mean(MeSn),mean(SddSn)))
names(MeanSd33)<-c("Parameters","Mean","Sd")

MeSr <- c()
SddSr <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeSr[i] <- mean (as.numeric(KTSr[i,6:10]))
  SddSr[i] <- sd (as.numeric(KTSr[i,6:10]))
}
(MeanSd34 <- data.frame("Sr",mean(MeSr),mean(SddSr)))
names(MeanSd34)<-c("Parameters","Mean","Sd")

MeTi <- c()
SddTi <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeTi[i] <- mean (as.numeric(KTTi[i,6:10]))
  SddTi[i] <- sd (as.numeric(KTTi[i,6:10]))
}
(MeanSd35 <- data.frame("Ti",mean(MeTi),mean(SddTi)))
names(MeanSd35)<-c("Parameters","Mean","Sd")

MeTl <- c()
SddTl <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeTl[i] <- mean (as.numeric(KTTl[i,6:10]))
  SddTl[i] <- sd (as.numeric(KTTl[i,6:10]))
}
(MeanSd36 <- data.frame("Tl",mean(MeTl),mean(SddTl)))
names(MeanSd36)<-c("Parameters","Mean","Sd")

MeV <- c()
SddV <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeV[i] <- mean (as.numeric(KTVv[i,6:10]))
  SddV[i] <- sd (as.numeric(KTVv[i,6:10]))
}
(MeanSd37 <- data.frame("V",mean(MeV),mean(SddV)))
names(MeanSd37)<-c("Parameters","Mean","Sd")

MeZn <- c()
SddZn <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeZn[i] <- mean (as.numeric(KTZn[i,6:10]))
  SddZn[i] <- sd (as.numeric(KTZn[i,6:10]))
}
(MeanSd38 <- data.frame("Zn",mean(MeZn),mean(SddZn)))
names(MeanSd38)<-c("Parameters","Mean","Sd")

MeF <- c()
SddF <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeF[i] <- mean (as.numeric(KTFf[i,6:10]))
  SddF[i] <- sd (as.numeric(KTFf[i,6:10]))
}
(MeanSd39 <- data.frame("F",mean(MeF),mean(SddF)))
names(MeanSd39)<-c("Parameters","Mean","Sd")

MeCl <- c()
SddCl <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeCl[i] <- mean (as.numeric(KTCl[i,6:10]))
  SddCl[i] <- sd (as.numeric(KTCl[i,6:10]))
}
(MeanSd40 <- data.frame("Cl",mean(MeCl),mean(SddCl)))
names(MeanSd40)<-c("Parameters","Mean","Sd")

MeBr <- c()
SddBr <- c()
for (i in 1:(dim(KT)[1]/41)) {
  MeBr[i] <- mean (as.numeric(KTBr[i,6:10]))
  SddBr[i] <- sd (as.numeric(KTBr[i,6:10]))
}
(MeanSd41 <- data.frame("Br",mean(MeBr),mean(SddBr)))
names(MeanSd41)<-c("Parameters","Mean","Sd")
(MeanSd <- rbind(MeanSd1,MeanSd2,MeanSd3,MeanSd4,MeanSd5,MeanSd6,MeanSd7,MeanSd8,
                 MeanSd9,MeanSd10,MeanSd11,MeanSd12,MeanSd13,MeanSd14,MeanSd15,MeanSd16,
                 MeanSd17,MeanSd18,MeanSd19,MeanSd20,MeanSd21,MeanSd22,MeanSd23,MeanSd24,
                 MeanSd25,MeanSd26,MeanSd27,MeanSd28,MeanSd29,MeanSd30,MeanSd31,MeanSd32,
                 MeanSd33,MeanSd34,MeanSd35,MeanSd36,MeanSd37,MeanSd38,MeanSd39,MeanSd40,
                 MeanSd41))
library(corrplot)
jo1 <- read.csv("GIS-K.csv",header = TRUE)
jo1 <- jo1[,-c(1:6)]
jo2 <- read.csv("GIS-KR.csv",header = TRUE)
jo2 <- jo2[,-c(1:6)]
jo3 <- read.csv("GIS-KT.csv",header = TRUE)
jo3 <- jo3[,-c(1:6)]
png(height=1200, width=1500, pointsize=10, file="Pearson-K.png")
corrplot(cor(jo1), method = "color",addCoef.col="grey")
png(height=2000, width=2000, pointsize=10, file="Pearson-KR.png")
corrplot(cor(jo2), method = "color",addCoef.col="grey")
png(height=2000, width=2000, pointsize=10, file="Pearson-KT.png")
corrplot(cor(jo3), method = "color",addCoef.col="grey")
#View(cor(jo1))
#View(cor(jo2))
#View(cor(jo3))
