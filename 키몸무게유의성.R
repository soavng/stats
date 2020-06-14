setwd('/Users/agun/Downloads/6th_data')
regular_season_Batter <- read.csv("Regular_Season_Batter.csv", fileEncoding = "UTF-8", na.strings = c("","-"),stringsAsFactors=F)
View(regular_season_Batter)
nrow(regular_season_Batter)
nrow(regular_season_Batter[!is.na(regular_season_Batter['height.weight']),])
heightweight = regular_season_Batter[!is.na(regular_season_Batter['height.weight']), ]
heightweight = heightweight[!is.na(heightweight['OPS']), ]
heightweight = heightweight[!heightweight['OPS'] == 0, ]
nrow(heightweight)

split_list = strsplit(heightweight$height.weight, "/")
height_list = list()
weight_list = list()
for (p in split_list) {
  height = as.numeric(gsub('cm', '', p[1]))
  weight = as.numeric(gsub('kg', '', p[2]))
  height_list[length(height_list)+1] <- height
  weight_list[length(weight_list)+1] <- weight
}
print(height_list)
print(weight_list)

## BMI 변환
bmi_convert <- function(w, h){
  bmi <- (  w / (h/100)**2)
  return (bmi)
}

bmi_class <- function(bmi){
  if(bmi < 18.5){
    return("A")
  }
  if( bmi <= 22.9){
    return("B")
  }
  if(  bmi <= 24.9){
    return("C")
  }
  
  if(bmi <= 29.9){
    return("D")
  }
  if(bmi <= 34.9){
    return("E")
  }
  return("F")
}

heightweight$weight <- as.vector(unlist(weight_list))
heightweight$height <- as.vector(unlist(height_list))

heightweight$bmi <-bmi_convert(heightweight$weight, heightweight$height)
heightweight$bmi_class <- sapply(heightweight$bmi, bmi_class)
View(heightweight)

heightweight = heightweight[!heightweight['bmi_class'] == 'F', ]

## 빈도 분석
table(heightweight$bmi_class)
## 평균 및 표준편차 
with(heightweight, tapply(OPS, bmi_class, mean))
with(heightweight, tapply(OPS, bmi_class, sd))


## anova 검증
out = lm(OPS~bmi_class, data=heightweight)
anova(out)

library(corrplot)
M<-cor(heightweight[, c("height", "weight", "OPS")])
corrplot.mixed(M)

out = lm(OPS~., data=heightweight[, c("height", "weight", "OPS")])
summary(out)
