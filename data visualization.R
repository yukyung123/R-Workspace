library(ggplot2)
library(quantmod)
library(dplyr)
#시간시각화-ggplot

TrafficAccident<-read.csv('TrafficAccident.csv')
head(TrafficAccident)
TrafficAccident

traffic<-subset(TrafficAccident,연도==2015)
#traffic<-subset(traffic,자치구==c("종로구","동대문구","서대문구","용산구","마포구"))
traffic
p1 <-ggplot(data=traffic, aes(x=월, y=발생건수))
p1 <- p1 + geom_line(aes(color=자치구),lwd=1)
p1 <- p1 + labs(x="month(월)",y="발생건수",title="2015년 월별 교통사고 발생건수", subtitle = "선도표 이용")
p1 <- p1 + ggtitle("2015 월별 교통사고 발생건수")+theme(plot.title = element_text(size=14,face="bold",color="red"))
p1 <- p1 + scale_x_continuous(breaks=seq(1,12,1))+ scale_y_continuous(breaks=seq(0,200,40))
p1 <- p1 + theme(axis.title.x = element_text(face="bold",
                                             colour="blue", size=14))
p1 <- p1 + theme(axis.title.y = element_text( angle=0,face="bold",
                                              colour="blue", size=14))
p1 <- p1+ theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                           hjust = 1, size=10))
p1

#시간시각화-R 그래픽스
dong<-c(129,119,120,142,158,152,172,145,157,155,164,165)
x<-1:12
plot(type="b",x,dong,xlab="month(월)",ylab="동대문구의 발생건수",col="blue")
title(main="2011년 동대문구의 월별 교통사고 발생건수",col.main="red")

#분포시각화-ggplot/ R 그래픽스
install.packages("vcd")
library(vcd)
data("Arthritis")
head(Arthritis)

par(mfrow=c(2,1))
m<-matrix(c(1,2,3,4),ncol=2,byrow=T)
layout(mat=m)
slices<-c(42,14,28)
lbls<-c("None","Some","Marked")
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, " ", pct, "%", "")
b2<-pie(slices, labels=lbls2, col=c("#0000FF1A","#0000FF43","#0000FFAE"),
        main="관절염 치료효과 분포도")

table(Arthritis$Sex)
slice1<-c(59,25)
lbls1<-c("Female","Male")
pct1 <- round(slice1/sum(slice1)*100)
lbls3 <- paste(lbls1, " ", pct1, "%", "")
p2<-pie(slice1, labels=lbls3,,col=c("pink","skyblue"),main="관절염 치료 실험 성분포도")

counts2<-table(Arthritis$Improved,Arthritis$Treatment)
counts2
barplot(counts2,main="관절염 치료방법에 따른 효과 분포도",xlab="Treatment",ylab="Frequency",legend=rownames(counts2),col=c("#0000FF1A","#0000FF43","#0000FFAE"))
counts3<-table(Arthritis$Sex,Arthritis$Improved)

counts<-table(Arthritis$Treatment,Arthritis$Improved)

counts4<-table(Arthritis$Sex,Arthritis$Improved)
counts4
counts4<-barplot(counts4,main="성별 치료효과 분포도",xlab="Treatment",ylab="sex",legend=rownames(counts4),col=c("pink","skyblue"))

#관계시각화-ggplot

traffic2015 <- subset(traffic, 연도==2015)
p1 <- ggplot(data=traffic2015, aes(x=발생건수, y=부상자수,col=자치구))
p1 <- p1 + geom_point(shape=15,size=3)
p1 <- p1 + ggtitle("2015년도 교통사고 발생건수와 부상자수와의 관계")+theme(plot.title=element_text(face="bold", size=20, vjust=2, color="red"))
p1 <- p1 + xlab("발생건수")
p1 <- p1 + stat_smooth(method="lm", se=FALSE, colour="red")
p1 <- p1 + theme(axis.title.x = element_text(face="bold",
                                             colour="blue", size=14))
p1 <- p1 + theme(axis.title.y = element_text( angle=0,face="bold",
                                              colour="blue", size=14))
p1 <-p1+ theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                          hjust = 1, size=10))
p1
#dev.off()아래 오류해결
#Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state


#관계시각화-R 그래픽스
hist(traffic2011$발생건수,main="2011년도 발생건수에 대한 히스토그램",col="blue", xlab="2011년도 발생건수")


#비교시각화- ggplot
y1<-ggplot(USArrests,aes(x=rownames(USArrests),y= Assault))
y1<-y1+geom_bar(stat="identity",aes(fill=Murder),color="black")
y1<-y1+ggtitle("미국 주별 폭행율과 살인율을 나타낸 그래프")
y1<-y1+xlab("주")+ylab("폭행")+scale_fill_gradient(low="white",high="brown")
y1<-y1+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
y1<-y1+theme(plot.title=element_text(face="bold",hjust=0.5,size=20))
y1 <- y1 + theme(axis.title.x = element_text(face="bold",
                                             colour="blue", size=14))
y1 <- y1 + theme(axis.title.y = element_text( angle=0,face="bold",
                                              colour="blue", size=14))
y1 <- y1+ theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                           hjust = 1, size=10))
y1


#공간시각화-ggplot
install.packages("maps")
library(maps)
library(tibble)
data("USArrests")
head(USArrests)
crime <- rownames_to_column(USArrests, var = "region")
crime$region <- tolower(crime$region) 
states_map <- map_data("state")
head(USArrests)
p1 <- ggplot(crime,type=n, aes(map_id = region))+ggtitle("미국 강간범죄율 지도" )+theme(plot.title=element_text(face="bold", size=20, vjust=2, color="red"))
p2 <- p1 + geom_map(aes(fill = Rape), color = "black", map = states_map)
p3 <- p2 + expand_limits(x = states_map$long, y = states_map$lat)
p4 <- p3 + scale_fill_continuous(name="Rape", low = "white",
                                 high = "red")+annotate("text", x=-117, y=40, label="State of Nevada", family="serif", fontface="italic", color="black", size=4)
p4

