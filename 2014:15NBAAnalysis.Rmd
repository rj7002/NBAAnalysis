```{r}
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)
```

```{r}
df = read.csv("~/Downloads/shot_logs.csv")
```

```{r}
head(df)
```

```{r}
df$player_name = sapply(df$player_name, str_to_title)
```

```{r}
unique(df$player_name)
```

```{r}
df %>% filter(player_name == 'Stephen Curry')
```

```{r}
ggplot(df,aes(x=SHOT_CLOCK)) + geom_histogram(color='black')
```


```{r}
ggplot(df,aes(x=SHOT_CLOCK,y=DRIBBLES)) + geom_point()
```
```{r}
ggplot(df,aes(x=SHOT_DIST)) + geom_histogram(color='black')
```
```{r}
ggplot(df,aes(x=CLOSE_DEF_DIST,y=DRIBBLES)) + geom_point(aes(color='red'))
```
```{r}
df %>% select(CLOSEST_DEFENDER)
```
```{r}
df$CLOSEST_DEFENDER = sapply(df$CLOSEST_DEFENDER, function(r){
  parts = str_split(r,pattern = ',')[[1]]
  return(paste(parts[2],parts[1],sep=' '))
})
```

```{r}
df %>% select(CLOSEST_DEFENDER)
```
```{r}
df$LOCATION = sapply(df$LOCATION, function(r) {
  if (r == 'A') {
    return('Away')
  } else if (r == 'H') {
    return('Home')
  }
})
```

```{r}
df$MIN = sapply(df$GAME_CLOCK, function(r) {
  parts = str_split(r,pattern=':')[[1]]
  return(as.integer(parts[1]))
})
```

```{r}
df$SEC = sapply(df$GAME_CLOCK, function(r) {
  parts = str_split(r,pattern=':')[[1]]
  return(as.integer(parts[2]))
})
```

```{r}
df %>% select(GAME_CLOCK,MIN,SEC)
```

```{r}
league = read.csv("~/Downloads/NBA_2015_Shots.csv")
```

```{r}
league
```
```{r}
# df = rename(df, MINS_LEFT = 'MIN',SECS_LEFT = 'SEC')
# df = rename(df,QUARTER = 'PERIOD')
# df = rename(df,PLAYER_ID = 'player_id')
df$SHOT_MADE = sapply(df$SHOT_RESULT, function(r) {
  if (r=='made') {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
df
```

```{r}
length(unique(df$GAME_ID))
```


```{r}
finalleague = merge(df,league,by=c('GAME_ID','PLAYER_ID','QUARTER','MINS_LEFT','SHOT_MADE'))
```

```{r}
colnames(finalleague)
```

```{r}
finalleague %>% select(SHOT_DIST,SHOT_DISTANCE,player_name,PLAYER_NAME,SHOT_RESULT,SHOT_MADE,ZONE_NAME,ZONE_ABB,SECS_LEFT.x,SECS_LEFT.y)
```
```{r}
length(unique(finalleague$GAME_ID))
```
```{r}
head(finalleague)
```

```{r}
finalleague %>% filter(abs(SECS_LEFT.x-SECS_LEFT.y) <= 5) %>% select(SHOT_DIST,SHOT_DISTANCE,player_name,PLAYER_NAME,SHOT_RESULT,SHOT_MADE,ZONE_NAME,ZONE_ABB,SECS_LEFT.x,SECS_LEFT.y,CLOSEST_DEFENDER)
```

```{r}
library(grid)
library(jpeg)
 
# half court image
courtImg.URL <- "https://thedatagame.com.au/wp-content/uploads/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
           width=unit(1,"npc"), height=unit(1,"npc"))
 
# plot using NBA court background and colour by shot zone
ggplot(filter(finalleague,CLOSE_DEF_DIST <= 30,player_name=='James Harden'), aes(x=LOC_X*10, y=LOC_Y*10-41.75)) + 
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(color=SHOT_MADE,alpha = CLOSE_DEF_DIST),size=3) +
      xlim(-250, 250) +
      ylim(-50, 420)
```
```{r}
finalleague %>% filter(CLOSEST_DEFENDER == unique(finalleague$CLOSEST_DEFENDER)[176])
```
```{r}
colnames(finalleague)
```

```{r}
finalleague$LOC_X2 = finalleague$LOC_X
finalleague$LOC_Y2 = finalleague$LOC_Y-50
```


```{r}
BasketballAnalyzeR::shotchart(filter(finalleague),'LOC_X2','LOC_Y2','SHOT_DIST',scatter = F,type="sectors",result = 'result',num.sect = 6)
```
```{r}
unique(finalleague$CLOSEST_DEFENDER)
```
```{r}
finalleague %>% group_by(PLAYER_NAME) %>% summarise(avg_dribbles = mean(DRIBBLES),shots=n()) %>% arrange(desc(avg_dribbles))
```
```{r}
finalleague %>% group_by(PLAYER_NAME) %>% summarise(avg_dribbles = mean(DRIBBLES),shots=n()) %>% arrange((avg_dribbles))
```


```{r}
finalleague %>% group_by(PLAYER_NAME) %>% summarise(avg_touch_time = mean(TOUCH_TIME),shots=n()) %>% arrange(desc(avg_touch_time))
```
```{r}
finalleague %>% group_by(PLAYER_NAME) %>% summarise(avg_touch_time = mean(TOUCH_TIME),shots=n()) %>% arrange((avg_touch_time))

```
```{r}
touchtime = finalleague %>% group_by(PLAYER_NAME) %>% summarise(avg_touch_time = mean(TOUCH_TIME),fgperc=sum(SHOT_MADE==1)/n(),avg_dribbles = mean(DRIBBLES)) %>% arrange((avg_touch_time))
```

```{r}
ggplot(touchtime,aes(x=avg_touch_time,y=fgperc)) + geom_point() 
```
```{r}
ggplot(touchtime,aes(x=avg_dribbles,y=fgperc)) + geom_point()  
```

```{r}
finalleague %>% group_by(PLAYER_NAME) %>% summarise(avg_def_dist = mean(CLOSE_DEF_DIST),shots=n(),fgperc=sum(SHOT_MADE==1)/n()) %>% arrange(desc(avg_def_dist))
```
```{r}
finalleague %>% group_by(PLAYER_NAME) %>% summarise(avg_def_dist = mean(CLOSE_DEF_DIST),shots=n(),fgperc=sum(SHOT_MADE==1)/n()) %>% arrange((avg_def_dist))
```

```{r}
defense = finalleague %>% group_by(CLOSEST_DEFENDER) %>% summarise(shots = n(),avg_dist=mean(CLOSE_DEF_DIST),fgperc=sum(SHOT_MADE ==1)/n(),avg_shot_dist =mean(SHOT_DIST)) %>% filter(shots >=mean(shots)) %>% arrange(fgperc)
defense
```
```{r}
ggplot(defense,aes(x=avg_dist,y=avg_shot_dist)) + geom_point(aes(color = fgperc)) + geom_smooth()
```
```{r}
finalleague$LOC_Y2 = finalleague$LOC_Y-46
shot_chart = BasketballAnalyzeR::shotchart(filter(finalleague,CLOSEST_DEFENDER==unique(finalleague$CLOSEST_DEFENDER)[176]),'LOC_X2','LOC_Y2','SHOT_DIST',scatter = F,type="sectors",result = 'result',num.sect = 6) 
ggplot_chart = ggplot(filter(finalleague, CLOSEST_DEFENDER==unique(finalleague$CLOSEST_DEFENDER)[176]), aes(x=LOC_X*10, y=LOC_Y*10-41.75)) + 
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(color=SHOT_MADE,alpha = CLOSE_DEF_DIST),size=3) +
      xlim(-250, 250) +
      ylim(-50, 420)

```

```{r}
shot_chart
#Shooting percentages when defended by Tony Allen

```
```{r}
ggplot_chart
#Shot chart when defended by Tony Allen
```
```{r}
finalleague %>% filter(CLOSEST_DEFENDER == unique(finalleague$CLOSEST_DEFENDER)[176]) %>% group_by(player_name) %>% summarise(shots=n(),shootingperc = sum(SHOT_MADE==1)/n()) %>% filter(shots >=mean(shots)) %>% arrange(desc(shootingperc))
#Best shooting percentages when defended by Tony Allen
```
```{r}
finalleague %>% filter(CLOSEST_DEFENDER == unique(finalleague$CLOSEST_DEFENDER)[176]) %>% group_by(player_name) %>% summarise(shots=n(),shootingperc = sum(SHOT_MADE==1)/n()) %>% filter(shots >=mean(shots)) %>% arrange(shootingperc)
#Worst shooting percentages when defended by Tony Allen
```

```{r}
finalleague$LOC_Y2 = finalleague$LOC_Y-46
shot_chart = BasketballAnalyzeR::shotchart(filter(finalleague,CLOSEST_DEFENDER==unique(finalleague$CLOSEST_DEFENDER)[301]),'LOC_X2','LOC_Y2','SHOT_DIST',scatter = F,type="sectors",result = 'result',num.sect = 6) 
ggplot_chart = ggplot(filter(finalleague,CLOSEST_DEFENDER==unique(finalleague$CLOSEST_DEFENDER)[301]), aes(x=LOC_X*10, y=LOC_Y*10-41.75)) + 
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(color=SHOT_MADE,alpha = CLOSE_DEF_DIST),size=3) +
      xlim(-250, 250) +
      ylim(-50, 420)
```

```{r}
#Shooting percentages when defended by LeBron James
shot_chart
```

```{r}
#Shot chart when defended by LeBron James
ggplot_chart
```


```{r}
finalleaguemodel = finalleague
```

```{r}
colnames(finalleaguemodel)
```

```{r}
finalleaguemodel$BASIC_ZONE_encoded = as.integer(factor(finalleaguemodel$BASIC_ZONE))
finalleaguemodel$ACTION_TYPE_encoded = as.integer(factor(finalleaguemodel$ACTION_TYPE))
finalleaguemodel$ZONE_NAME_encoded = as.integer(factor(finalleaguemodel$ZONE_NAME))

```

```{r}
library(caTools)

split = caTools::sample.split(finalleaguemodel$SHOT_MADE,SplitRatio = 0.7)
```

```{r}
test = subset(finalleaguemodel,split==F)
train = subset(finalleaguemodel,split==T)

```

```{r}
model = glm(SHOT_MADE~MINS_LEFT+DRIBBLES+BASIC_ZONE_encoded+ACTION_TYPE_encoded+TOUCH_TIME+CLOSE_DEF_DIST+SHOT_DISTANCE+QUARTER+SHOT_CLOCK+SECS_LEFT.x,data=train,family = binomial)
```

```{r}
probabilities <- predict(model, test,type = "response")
```

```{r}
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
```

```{r}
predsdf = data.frame(predicted_classes)
```

```{r}
table(predsdf$predicted_classes,test$SHOT_MADE)
```



