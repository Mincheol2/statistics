## 기초의학통계
## 2018/11/23

################
# Data 준비
################
# excel 파일을 읽어주는 readxl 라이브러리 설치 필요
# install.packages('readxl')
# 실습용 서버에는 이미 설치되어 있음

# readxl 라이브러리 사용을 R에게 통보
library(readxl)

# 구별 cctv data 읽어오기
dataFile <- "https://raw.githubusercontent.com/gehoon/statistics/master/data/seoul/seoul_CCTV.csv"
df <- read.csv(url(dataFile))
head(df)
df.cctv <- data.frame(district = df$기관명, cctv = df$소계)
head(df.cctv)

# 구별 인구 읽어오기
dataFile <- "~/data/seoul/서울시_주민등록인구_(구별).xls"
df <- read_excel(dataFile, skip=2)
head(df)
df <- df[-1,] # df의 첫번째 row를 제거후 df에 저장하시오 (합계 row 삭제)
head(df)

df.pop <- data.frame(district = df$자치구, population = df$계)
head(df.pop)

# 구별 자동차 등록수
dataFile <- "~/data/seoul/서울시_자동차등록_(월별_구별).xls"
df <- read_excel(dataFile, skip=1)
head(df)
df <- df[-1,] # df의 첫번째 row를 제거후 df에 저장하시오 (합계 row 삭제)

df.car <- data.frame(district = df$자치구, car = as.numeric(df$소계))
head(df.car)

# 구별 공시지가
dataFile <- "~/data/seoul/공시지가_2018년.xlsx"
df <- read_excel(dataFile)

df$SIGUNGU_NM <- with(df, reorder(SIGUNGU_NM, JIGA, FUN = median))
boxplot(JIGA ~ SIGUNGU_NM, data=df)

district <- unique(df$SIGUNGU_NM)
medJIGA <- sapply(district, function(x) median(df[df$SIGUNGU_NM == x,]$JIGA))
df.house <- data.frame(district = district, price = medJIGA)

# 구별 5대 범죄 발생율
dataFile <- "https://raw.githubusercontent.com/gehoon/statistics/master/data/crime/seoul.2017.csv"
df <- read.csv(url(dataFile))

df$구분 <- paste0(df$구분,'구')
df$구분[df$구분 == '중부구'] <- '중구'

mergeDistrict <- function(df, dist1, dist2) {
  df[df$구분 == dist1,-1] <- colSums(df[c(which(df$구분 == dist1),which(df$구분 == dist2)),-1])
  df <- df[-which(df$구분 == dist2),]
  return(df)
}

df <- mergeDistrict(df, '서초구', '방배구')
df <- mergeDistrict(df, '강남구', '수서구')
df <- mergeDistrict(df, '종로구', '혜화구')
df <- mergeDistrict(df, '중구', '남대문구')
df <- mergeDistrict(df, '성북구', '종암구')
df <- mergeDistrict(df, '은평구', '서부구')

df.crime <- data.frame(district = df$구분,
                       살인 = df$살인발생,
                       강간 = df$강간발생,
                       강도 = df$강도발생,
                       폭력 = df$폭력발생,
                       절도 = df$절도발생)

# data.frame 합하기
seoul <- merge(df.pop, df.cctv, by="district")
seoul <- merge(seoul, df.car, by="district")
seoul <- merge(seoul, df.house, by="district")
seoul <- merge(seoul, df.crime, by="district")

rownames(seoul) <- seoul$district
seoul <- seoul[,-1]

head(seoul)
plot(seoul)
cor(seoul)

################################################
# Multiple Linear Regression Example
################################################
fit <- lm(price ~ ., data=seoul)
anova(fit)

fit2 <- lm(price ~ cctv+car+강간+절도, data=seoul)
anova(fit2)
anova(fit2, fit)

fit3 <- lm(price ~ car+강간, data=seoul)
anova(fit3, fit2, fit)

summary(fit3)
