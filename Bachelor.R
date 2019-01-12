

library(TraMineR)
library(cluster)
library(fpc)



#Die leeren Felder der Comma-Separated-Values Tabelle werden nicht automatisch als NA erkannt#

tun <- read.csv2(file = "[...]TunSTS.csv", header = TRUE)
tun[tun == ""] <- NA


#Daten mit allen 18 Codes#

tunalle <- read.csv2(file = "[...]TunalleSTS.csv", header = TRUE)
tunalle[tunalle == ""] <- NA

#Drittvariablen#

#Die Variable Geschlecht#

attach(tun)
tun$gender<-factor(tun$gender)
gtable<-table(aff2, gender)
prop.table(gtable, 1)




#Die Variable Herkunft#

attach(tun)
tun$ptype<-factor(tun$ptype)
hptable<-table(ptype, aff2)
chisq.test(hptable)




#Die Variable Alter#

attach(tun)
tun$age<-tun$takeover-tun$birthyear
aggregate(age~aff2, tun, mean)


age.box<-data.frame(x=tun$aff2, y=tun$age)
with(age.box ,boxplot(y~x))


#Deskriptiv mit allen Codes#

#Durchschnittliche Dauer der Sequenzen#

tun.seq <- seqdef(tun, var = c("a48", "a47", "a46", "a45", "a44", "a43", "a42", "a41", "a40", "a39", "a38", "a37", "a36", "a35", "a34", "a33", "a32", "a31", "a30", "a29", "a28", "a27", "a26", "a25", "a24", "a23", "a22", "a21", "a20", "a19", "a18", "a17", "a16", "a15", "a14", "a13", "a12", "a11", "a10", "a9","a8", "a7", "a6", "a5", "a4", "a3", "a2", "a1", "a0"), labels=c("Administration", "Posten in Demokratie", "Justiz", "Opposition", "Private Tätigkeit", "Politische Tätigkeit", "Posten in Diktatur", "Tätigkeit an einer Universität", "Studierende im Ausland", "Studierende in Tunesien"  ))

seqlegend(tun.seq, fontsize = 1.3)


tunallelabels<-c( "Administration niedrig", "Administration hoch", "Private Tätigkeit in Führungsposition", "Universitätsprofessor", "Posten in Demokratie", "Gefängis", "Gewerkschaftliche Tätigkeit", "Justiz", "Oppositionelle Tätigkeit", "Private Tätigkeit", "Politische Tätigkeit in einer Partei", "Hoher Posten im Diktatur", "Niedriger Posten im Diktatur", "Einstellung an Universität", "Studierende im Ausland","Studierende in Frankreich", "Studierende in Tunesien")
tunalle.seq <-seqdef(tunalle, var = c("a48", "a47", "a46", "a45", "a44", "a43", "a42", "a41", "a40", "a39", "a38", "a37", "a36", "a35", "a34", "a33", "a32", "a31", "a30", "a29", "a28", "a27", "a26", "a25", "a24", "a23", "a22", "a21", "a20", "a19", "a18", "a17", "a16", "a15", "a14", "a13", "a12", "a11", "a10", "a9","a8", "a7", "a6", "a5", "a4", "a3", "a2", "a1", "a0"), cpal= c( "azure", "azure3" ,  "darseagreen" , "gold" , "firebrick", "firebrick1" , "firebrick3" , "gray3", "indianred" ,"darkseagreen1", "darkorange1", "gray71", "grey85" , "gold3", "cyan4", "cyan3" , "cyan2"), missing=NA, left=NA, right=NA, labels=tunallelabels)

seqmtplot(tunalle.seq, title="Durchschnittliche Dauer aller Sequenzen 18 Codes", withlegend=FALSE, ylim= c(0,5))




#Verdichtende Maßzahlen#

#Subsequenzen, relevanteste#


tuned.seq <-seqdef(tun, var = c("a48", "a47", "a46", "a45", "a44", "a43", "a42", "a41", "a40", "a39", "a38", "a37", "a36", "a35", "a34", "a33", "a32", "a31", "a30", "a29", "a28", "a27", "a26", "a25", "a24", "a23", "a22", "a21", "a20", "a19", "a18", "a17", "a16", "a15", "a14", "a13", "a12", "a11", "a10", "a9","a8", "a7", "a6", "a5", "a4", "a3", "a2", "a1", "a0"),  missing=NA, left="DEL", right=NA)

alphabet (tuned.seq) <- c( "a", "d", "ju", "op", "p", "pol", "r", "u", "ue", "ut")
tuned.seqe <-seqecreate(tuned.seq, tevent="state")
fsubseq <-seqefsub(tuned.seqe, pMinSupport = 0.05)
distuncohort<-seqecmpgroup(fsubseq, group=tun$aff2, method="bonferroni")
plot(distuncohort[1:10])


#Letzter Zustand getrennt nach Diktatur/Demokratie#

tunta<-tapply(tun.seq$a0 ,tun$aff2, function(x){prop.table(table(x))})
tuntar<-do.call(rbind,tunta)
tuntar2<-tuntar*100
tuntar3<-tuntar2[, 1:8]
barplot(tuntar3, main="Letzter Zustand nach Affiliation in Prozent (ohne missing values)", xlab="Position", col=c("darkblue", "red"), beside=TRUE, names=c("Administration", "Posten in Demokratie", "Justiz", "Opposition", "Private Tätigkeit", "Politische Tätigkeit", "Posten in Diktatur", "Tätigkeit an Universität"), cex.axis=0.6, cex.names=0.6, ylab="Prozent")
legend("top", legend=c("Demokratie", "Diktatur"), fill=c("darkblue", "red"))


#Deskirptiv mit reduzierten Codes#

seqmtplot(tun.seq, withlegend=FALSE, group=tun$aff2, ylim= c(0,10))
seqdplot(tun.seq, group=tun$aff2, border = NA, title = "State distribution plot",  withlegend=FALSE, with.missing=TRUE)


#Entropie Index#

seqHtplot(tun.seq, title = "Entropy index", group=tun$aff2)


#Clusteranalyse#

#Zur Clusteranalyse muss left=DEL#

tun2.seq<-seqdef(tun, missing=NA, left="DEL", right=NA, var = c("a48", "a47", "a46", "a45", "a44", "a43", "a42", "a41", "a40", "a39", "a38", "a37", "a36", "a35", "a34", "a33", "a32", "a31", "a30", "a29", "a28", "a27", "a26", "a25", "a24", "a23", "a22", "a21", "a20", "a19", "a18", "a17", "a16", "a15", "a14", "a13", "a12", "a11", "a10", "a9","a8", "a7", "a6", "a5", "a4", "a3", "a2", "a1", "a0"), labels=c("Administration", "Posten in Demokratie", "Justiz", "Opposition", "Private Tätigkeit", "Politische Tätigkeit", "Posten in Diktatur", "Tätigkeit an einer Universität", "Studierende im Ausland", "Studierende in Tunesien" ))

#Distanzmatrix und Dendogramm#

tun.lcs <-seqdist(tun2.seq, method="LCS", with.miss=TRUE, norm=TRUE)

cluster <-agnes(tun.lcs, diss=TRUE, method="ward")
plot(cluster, which.plots=2)

#Ellenbogenkriterium#

cluster$height
hclust.alg<-as.hclust(cluster)
hclust.alg$height
end.pos<-length(hclust.alg$height)
start.pos<-end.pos - 14
y<-hclust.alg$height [start.pos:end.pos]
x<-seq(15, 1, by=-1)
plot(x,y, type="b", xlab="Anzahl Cluster", ylab="Fehlerquadratsumme")


#Within/Between-Ratio (nach Aisenbrey und Fasang)#


cluster2<-cutree(cluster, k=2)
cluster3<-cutree(cluster, k=3)
cluster4<-cutree(cluster, k=4)
cluster5<-cutree(cluster, k=5)
cluster6<-cutree(cluster, k=6)
cluster7<-cutree(cluster, k=7)
cluster8<-cutree(cluster, k=8)
cluster9<-cutree(cluster, k=9)
cluster10<-cutree(cluster, k=10)
cluster11<-cutree(cluster, k=11)
cluster12<-cutree(cluster, k=12)
cluster13<-cutree(cluster, k=13)
cluster14<-cutree(cluster, k=14)
cluster15<-cutree(cluster, k=15)
clustats2 <-cluster.stats(tun.lcs, cluster2)
clustats3 <-cluster.stats(tun.lcs, cluster3)
clustats4 <-cluster.stats(tun.lcs, cluster4)
clustats5 <-cluster.stats(tun.lcs, cluster5)
clustats6 <-cluster.stats(tun.lcs, cluster6)
clustats7 <-cluster.stats(tun.lcs, cluster7)
clustats8 <-cluster.stats(tun.lcs, cluster8)
clustats9 <-cluster.stats(tun.lcs, cluster9)
clustats10<-cluster.stats(tun.lcs, cluster10)
clustats11<-cluster.stats(tun.lcs, cluster11)
clustats12<-cluster.stats(tun.lcs, cluster12)
clustats13<-cluster.stats(tun.lcs, cluster13)
clustats14<-cluster.stats(tun.lcs, cluster14)
clustats15<-cluster.stats(tun.lcs, cluster15)


wb.ratio <-c(clustats2$wb.ratio, clustats3$wb.ratio, clustats4$wb.ratio, clustats5$wb.ratio, clustats6$wb.ratio, clustats7$wb.ratio, clustats8$wb.ratio, clustats9$wb.ratio, clustats10$wb.ratio, clustats11$wb.ratio, clustats12$wb.ratio, clustats13$wb.ratio, clustats14$wb.ratio, clustats15$wb.ratio)

plot(2:15, wb.ratio, type="b", xlab="Anzahl Cluster", ylab="w/b ratio")

#Genau die Within-Between-Ration ausrechnen:
clustatssix<-cluster.stats(tun.lcs, clustersix)
clustatssix



# Cluster inhaltlich#

clustersix<-cutree(cluster, k=6)
clustersix<-factor(clustersix, labels=c("Type 1", "Type 2", "Type 3", "Type 4", "Type 5", "Type 6"))
seqdplot(tun.seq, border=NA, group=clustersix, withlegend=FALSE, with.missing=TRUE)


cluster8<-cutree(cluster, k=8)
cluster8<-factor(cluster8, labels=c("Type 1", "Type 2", "Type 3", "Type 4", "Type 5", "Type 6", "Type 7", "Type 8"))


#Most common Transitions 6-Cluster#
#fsubseq muss hier bereits definiert sein#

tunclfac <-cbind(tun, clusterID=clustersix)
dis6crcohort<-seqecmpgroup(fsubseq, group=tunclfac$clusterID, method="bonferroni")
plot(dis6crcohort[1:10])


#Chi-Test 6-Cluster#

attach(tunclfac)
chisq.test(ktable)



# Logistische Regression mit 6-Cluster#
# Die Variablen Alter und Geschlecht müssen hier bereits definiert sein#


tn1 <-(clustersix =="Type 1")
tn2 <-(clustersix =="Type 2")
tn3 <-(clustersix =="Type 3")
tn4 <-(clustersix =="Type 4")
tn5 <-(clustersix =="Type 5")
tn6 <-(clustersix =="Type 6")
         
glm1<-glm(tn1~aff2 +gender+age, data=tun, family="binomial" )
glm2<-glm(tn2~aff2 +gender+age, data=tun, family="binomial" )
glm3<-glm(tn3~aff2 +gender+age, data=tun, family="binomial" )
glm4<-glm(tn4~aff2 +gender+age, data=tun, family="binomial" )
glm5<-glm(tn5~aff2 +gender+age, data=tun, family="binomial" )
glm6<-glm(tn6~aff2 +gender+age, data=tun, family="binomial" )
         
exp(coef(glm1))
exp(coef(glm2))
exp(coef(glm3))
exp(coef(glm4))
exp(coef(glm5))
exp(coef(glm6))
         
#Hieraus lässt sich die Tabelle mit den Odds bilden#
         
         
# Regierungen und Clustermitgliedschaft#
         
attach(tunclfac)
rrtable<-table(tunclfac$clusterID, tun$affiliation)
table(rrtable)
         
         
         
         
