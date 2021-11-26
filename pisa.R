data = read.csv(file = "C:/Users/merry/Desktop/pisa.csv",fileEncoding="UTF-8-BOM", na.strings = '..')
data

library(tidyverse)  #package for tidying up the data set
library(ggplot2)  #package for visualizations
library(corrplot)  #package for correlation plot

str(data)
df <- data[1:1161, c(1, 4, 7)] %>%  spread(key=Series.Code, value=X2015..YR2015.) %>%  rename(Maths = LO.PISA.MAT,                        
            Maths.F = LO.PISA.MAT.FE,
            Maths.M = LO.PISA.MAT.MA,
            Reading = LO.PISA.REA,
            Reading.F = LO.PISA.REA.FE,
            Reading.M = LO.PISA.REA.MA,
            Science = LO.PISA.SCI,
            Science.F = LO.PISA.SCI.FE,
            Science.M = LO.PISA.SCI.MA
) %>%
  drop_na()
view(df)

#Ranking of Maths Score by Countries
ggplot(data=df,aes(x=reorder(Country.Name,Maths),y=Maths)) + 
  geom_bar(stat ='identity',aes(fill=Maths))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Maths Score Level")+
  labs(title = 'Ranking of Countries by Maths Score',
       y='Score',x='Countries')+ 
  geom_hline(yintercept = mean(df$Maths),size = 1, color = 'blue')

df2 = df[,c(1,3,4,6,7,9,10)] %>%   # select relevant columns 
  pivot_longer(c(2,3,4,5,6,7),names_to = 'Score')
view(df2) 

ggplot(data = df2, aes(x=Score,y=value, color=Score)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='Test Type')

ggplot(data = df2, aes(x=Score,y=value, fill=Score)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Green") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='Test Type')

S = numeric(408)     # create an empty vector
for (i in 1:length(df2$Score)) {
  S[i] = strsplit(df2$Score[i],".",fixed = TRUE)
}

df3 = S%>%unlist() %>% matrix(ncol = 2, byrow = TRUE)%>% as.data.frame()
view(df3)

df4 = cbind(df2,df3) 
colnames(df4) = c('Country','Score','Value','Test','Gender')
df4$Score = NULL # since the 'Score' column is redundant
view(df4)

ggplot(data = df4, aes(x=Test,y=Value, fill=Test)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Green") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='Test')+
  facet_wrap(~Gender,nrow = 1)

ggplot(data = df4, aes(x=Gender,y=Value, fill=Gender)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Green") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='')+
  facet_wrap(~Test,nrow = 1)

df = df %>% mutate(Maths.Diff = ((Maths.M - Maths.F)/Maths.F)*100,
                   Reading.Diff = ((Reading.M - Reading.F)/Reading.F)*100,
                   Science.Diff = ((Science.M - Science.F)/Science.F)*100,
                   Total.Score = Maths + Reading + Science,
                   Avg.Diff = (Maths.Diff+Reading.Diff+Science.Diff)/3
)
view(df)

##### MATHS SCORE #####
ggplot(data=df, aes(x=reorder(Country.Name, Maths.Diff), y=Maths.Diff)) +
  geom_bar(stat = "identity", aes(fill=Maths.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$Maths.Diff), size=1, color="black") +
  scale_fill_gradient(name="% Difference Level") +
  labs(title="Are Males better at math?", x="", y="% difference from female")

df = df[,c(1,3,4,6,7,9,10)] #select relevant columns
res = cor(df[,-1]) # -1 here means we look at all columns except the first column
res

install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(df[,-1))

install.packages("corrplot")

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
