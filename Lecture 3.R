require(dplyr)
require(mosaic)
require(lubridate)
Births <- mutate(Births78,
                 date=ymd(date)-years(100), # y2k fix
                 wd = wday(date),           # as a number
                 wday = wday(date,label=TRUE,abbr=TRUE)
                 )
head(Births)


head(HELPrct)
qplot(substance, data=HELPrct)
qplot(i2,data=HELPrct)
qplot(date,births,data=Births)
qplot(sex,substance,data=HELPrct)

qplot(substance,age,data=HELPrct)   #Two qualitative variables

qplot(date,births,color=wday,data=Births)
qplot(date,births,color=wd,data=Births)


#This doesn't display navy colors because it is mapping color to a newly created variable called "navy"
qplot(date,births,color="navy",data=Births)
#This will actually display the color navy
qplot(date,births,color=I("navy"),data=Births)


ggplot(data=Births,aes(x=date,y=births,color=wday))+geom_point()

#This will show you a family of functions starting with "geom"
apropos("^geom_")
apropos("^stat_")


ggplot(data=HELPrct,aes(x=age,fill=substance))+stat_density(alpha=0,5)


ggplot(data=HELPrct,aes(x=age,color=substance))+stat_density(geom="line")


ggplot(data=HELPrct,aes(x=sex,y=age))+
  geom_boxplot(outlier.size=0)+
  geom_jitter(alpha=.6)+
  coord_flip()


require(ggthemes)
qplot(x=date,y=births,data=Births)+theme_wsj()
qplot(x=date,y=births,data=Births,geom="line")+theme_xkcd()

require(xkcd)
qplot(x=date,y=births,data=Births,color=wday,geom="smooth",se=FALSE)+theme+xkcd()
#####################################################################################


head(mtcars)
qplot(wt,data=mtcars,color=cyl)
qplot(wt,mpg,data=mtcars,size=factor(cyl),size=5)
#####################################################################################

head(EuStockMarkets)
ds = as.data.frame(EuStockMarkets)
ds$day = seq(nrow(ds))#Add a sequential count of the records to indicate the "day"
library(reshape)
head(ds)
EuStock <- melt(ds,id=c("day"))
head(EuStock)
names(EuStock)[2]="Index"
head(EuStock)
ggplot(EuStock,aes(day,value,color=Index))+geom_line()
ggplot(EuStock,aes(day,value,color=Index))+geom_line(size=3)
ggplot(EuStock,aes(day,value,color=Index))+geom_line(size=1.5,alpha=.5)


library(reshape)




