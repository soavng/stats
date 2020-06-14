setwd('/Users/agun/Downloads/6th_data')
regular_season_Batter <- read.csv("Regular_Season_Batter.csv", fileEncoding = "UTF-8", na.strings = c("","-"),stringsAsFactors=F)
View(regular_season_Batter)
names(regular_season_Batter)

regular_season_Batter["team"]
team <- as.factor(regular_season_Batter$team)
del_team <- c("OB", "쌍방울", "우리", "해태", "히어로즈")
nrow(regular_season_Batter)
regular_season_Batter <- regular_season_Batter[!(regular_season_Batter$team == "OB" ), ]
regular_season_Batter <- regular_season_Batter[!(regular_season_Batter$team == "쌍방울" ), ]
regular_season_Batter <- regular_season_Batter[!(regular_season_Batter$team == "우리" ), ]
regular_season_Batter <- regular_season_Batter[!(regular_season_Batter$team == "해태" ), ]
regular_season_Batter <- regular_season_Batter[!(regular_season_Batter$team == "히어로즈" ), ]
regular_season_Batter <- regular_season_Batter[!is.na(regular_season_Batter$OPS), ]
regular_season_Batter <- regular_season_Batter[!regular_season_Batter$OPS==0, ]

## 소속팀 결측치 확인 
nrow(regular_season_Batter[is.na(regular_season_Batter$team), ])
## 총 row수 확인
nrow(regular_season_Batter)
## 반도 분석
table(regular_season_Batter["team"])

team <- as.factor(regular_season_Batter$team)
## 평균과 표준편차 구함
with(regular_season_Batter, tapply(OPS, team, mean))
with(regular_season_Batter, tapply(OPS, team, sd))
## anova 검증
out = lm(OPS~team, data=regular_season_Batter)
anova(out)

shapiro.test(resid(out))
install.packages("multcomp")
library(multcomp)
out = lm(OPS~team, data=regular_season_Batter)

regular_season_Batter$team =  as.factor(regular_season_Batter$team)
dunnett = glht(out, linfct=mcp(team="Dunnett"))
summary(dunnett)
plot(dunnett)
class(regular_season_Batter$team)
regular_season_Batter$team
length(unique(team))
