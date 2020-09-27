library(readxl)
library(dplyr)
library(NbClust)
library(ggplot2)
library(plotly)

#미국시장에 가장 적합한 한국맥주 모델링

#로딩 및 필요행,열만 추출 
usbeer <- read_xlsx(file.choose())
us_beer <- select(usbeer,c(2:5))
is.na(us_beer)
us_beer <- us_beer[c(1:25),]
us_beer$gubun <- c("US") #국가 변수 추가

kobeer <- read_xlsx(file.choose())
ko_beer <- select(kobeer,c(2:5))
names(ko_beer) <- c("Name","ABV","IBU","SRM")
is.na(ko_beer)
tail(ko_beer)
ko_beer$gubun <- c("KOR") # 국가 변수 추가


#데이터 병합, 형변환 
beers <- rbind(us_beer,ko_beer)
beers$ABV <- as.numeric(beers$ABV)


#산점도 확인(2차원으로는 3개의 변수를 동시에 볼 수 없기때문에 3차원 함수 사용)
p <- plot_ly(beers, x= beers$ABV, y = beers$IBU, z = beers$SRM,
             color = beers$gubun, colors = c('#BF382A','#0C4B8E'), alpha = .7) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'ABV'),
                      yaxis = list(title = 'IBU'),
                      zaxis = list(title = 'SRM')
                      ))   # 3차원으로 확인결과, 범위값들의 오차가 큰 부분은 제외하고 
                           # 서로 가까운 거리에 군집된 x=6(ABV),y=25(IBU),z=9(SRM)이내 값들만 샘플링한다


#데이터 재추출
re_beers <- beers
re_beers$ABV <- ifelse(beers$ABV <= 6,beers$ABV,0)
re_beers$IBU <- ifelse(beers$IBU <= 25,beers$IBU,0)
re_beers$SRM <- ifelse(beers$SRM <= 9,beers$SRM,0)
re_beers <- re_beers[!(re_beers$ABV == 0 | re_beers$IBU == 0 | re_beers$SRM == 0),]



#모델링에 사용할 연속형 변수 추출
x <- re_beers[,c(2:4)]
y <- re_beers$Name

##계층적 클러스터링
scaled_x <- scale(x) #정규화
dist_x <- dist(scaled_x) #거리 알아보기
cluster_x_avg <- hclust(dist_x,method = "average") #평균거리법으로 군집화
NbClust(scaled_x,distance="euclidean",min.nc = 2,max.nc = 18,method = "average") #군집개수 추천받기
cutree_x_avg <- cutree(cluster_x_avg,k=16) #k개의 군집화에 따른 각각의 값 확인
table(cutree_x_avg)

plot(cluster_x_avg,hang=-1,cex=.6,labels = re_beers$Name,main = "맥주 클러스터링",xlab = "맥주") #그룹 시각화
rect.hclust(cluster_x_avg,k=16) #그룹표시

#탐색적 데이터에서 스타일별 특징으로 추정한것과 같이,
# 미국시장 점유율 TOP1,2,3위인 Bud light,Coors light,Miller light와 가장 가까운 한국 맥주는 CASS LIGHT이다

