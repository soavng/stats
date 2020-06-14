setwd('/Users/agun/Downloads/6th_data')
regular_season_Batter <- read.csv("Regular_Season_Batter.csv", fileEncoding = "UTF-8", na.strings = c("","-"),stringsAsFactors=F)
table(regular_season_Batter["position"])
position = regular_season_Batter[!is.na(regular_season_Batter['position']), ]

### 포지션별 결측치, 0 값 처리
position = position[!is.na(position['OPS']), ]
position = position[!position['OPS'] == 0, ]
nrow(position)
table(position["position"])

## 30개 미만이 포지션 삭제 
position = position[!position['position'] == '내야수(우투양타)', ]
position = position[!position['position'] == '외야수(우투양타)', ]
position = position[!position['position'] == '포수(우투좌타)', ]
table(position["position"])

with(position, tapply(OPS, position, mean))
with(position, tapply(OPS, position, sd))
nrow(position)
## anova 검증
out = lm(OPS~position, data=position)
anova(out)
