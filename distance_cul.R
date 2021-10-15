#flights5 is the right data, last 4 columns.
library(geosphere)
library(dplyr)
library(nycflights13)
library(lubridate)
library(ggplot2)
library(tidyr)
library(cowplot)
#function to culculate flat earth
to_radian <- function(a){
  return(a*pi/180)
}
EQ <- 6371*pi/2
distFlat <- function(p1, p2, E){
  lat1 <- to_radian(p1[[2]])
  lat2 <- to_radian(p2[[2]])
  lng1 <- to_radian(p1[[1]])
  lng2 <- to_radian(p2[[1]])
  pi2 <- pi / 2;
  r1 <- (1 - lat1/pi2) * E
  r2 <- (1 - lat2/pi2) * E
  x1 <- r1 * cos( lng1 )
  y1 <- r1 * sin( lng1 )
  x2 <- r2 * cos( lng2 )
  y2 <- r2 * sin( lng2 )
  dx <- x2 - x1
  dy <- y2 - y1
  LF <- as.numeric(sqrt( dx*dx + dy*dy ))
  return (LF*1000)
}
#
flights3=flights%>%mutate(date=make_date(year,month,day))%>%
  select(origin,dest,distance)
airports3=airports%>%select(faa,lon,lat,alt)

flights4=flights3%>%left_join(airports3,by=c('origin'='faa'))%>%
  rename(ori_lon=lon,ori_lat=lat,ori_alt=alt)%>%
  left_join(airports3,by=c('dest'='faa'))%>%
  rename(dest_lon=lon,dest_lat=lat,dest_alt=alt)%>%
  drop_na

get_dist=function(data){
  p1=data[4:5]#origin
  p2=data[7:8]#dest
  distance=list(distCosine(p1, p2, r=6378137),
                distGeo(p1, p2, a=6378137, f=1/298.257223563),
                distFlat(p1,p2,EQ))
  return(distance)
}
z=flights4%>%rowwise()%>%get_dist()%>%as.data.frame()
names(z)=c('round','episoid','flat')
flights5=flights4%>%mutate(theory_dist=distance*1609,z)%>%
          distinct(origin,dest,theory_dist,round,episoid,flat)
flights5=flights5[-219,]
flights5$num=seq.int(nrow(flights5))

#flights5%>%select(num,theory_dist,round,episoid,flat)%>%View



p1=flights5%>%ggplot(aes(x=num,y=(theory_dist-round)/theory_dist))+geom_smooth(color='Red',se=F)+	
  geom_abline(intercept = 0, slope = 0)+geom_point(size=1,color='Grey')+
  labs(title = "Dist~Round_Dist", x = "Index", y = "Relative Difference")+
  theme(plot.title = element_text(hjust = 0.5))
p2=flights5%>%ggplot(aes(x=num,y=(theory_dist-episoid)/theory_dist))+geom_smooth(color='Red',se=F)+	
  geom_abline(intercept = 0, slope = 0)+geom_point(size=1,color='Grey')+
  labs(title = "Dist~Ellipsoid_Dist", x = "Index", y = "Relative Difference")+
  theme(plot.title = element_text(hjust = 0.5))
p3=flights5%>%ggplot(aes(x=num,y=(theory_dist-flat)/theory_dist))+geom_smooth(color='Red',se=F)+	
  geom_abline(intercept = 0, slope = 0)+geom_point(size=1,color='Grey')+
  labs(title = "Dist~Flat_Dist", x = "Index", y = "Relative Difference")+
  theme(plot.title = element_text(hjust = 0.5))
plot_grid(p1,p2,p3,labels='AUTO')
#hyp test, null hyp: no difference
x1=flights5$theory_dist
x2=flights5$round
x3=flights5$episoid
x4=flights5$flat
l=length(x1)
#t.test we don't use t test because it does not follow normal distribution.
#t.test(log(x1),log(x2),conf.level=0.95)# reject
#t.test(log(x1),log(x3),conf.level=0.95)# reject
#t.test(log(x1),log(x4),conf.level=0.95)# reject
#sign.test
binom.test(sum(abs(x1-x2)/x1<0.10), l)#reject
binom.test(sum(abs(x1-x3)/x1<0.10), l)#reject
binom.test(sum(abs(x1-x4)/x1<0.10), l)#fail to reject

