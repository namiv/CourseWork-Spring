install.packages("dplyr")
library(dplyr)
library(ggplot2)

#################################################################################################
# LOAD INITIAL DATA FRAMES
deaths<-read.csv("data/deaths.csv")
ylds<-read.csv("data/ylds.csv")
life_ex<-read.csv("data/life_expectancy.csv")

#################################################################################################
# MERGING DATA FRAMES 
join<-merge(deaths,ylds, by = c("cause_name","age","sex"))
join[is.na(join)]<-0
prep_data<-merge(join,life_ex, by = "age")
prep_data %>% arrange(cause_name) %>% mutate(age_group = substr(age, 1, 2)) -> prep_data
prep_data[prep_data$age_group=='Un',"age_group"]<- 0
prep_data$age_group<-as.numeric(prep_data$age_group)
prep_data$life.ex.up <- prep_data$life.expectancy + prep_data$age_group

#################################################################################################
# COMPUTING YLL AND DALY
prep_data$yll<- (prep_data$life.ex.up - prep_data$age_group)*prep_data$deaths
prep_data$dalys<-prep_data$yll + prep_data$ylds

#################################################################################################
# AGGREGATING DATA PER CAUSE, AGE AND SEX
prep_data %>% 
  group_by(cause_name) %>% 
  summarise( total_dalys = sum(dalys),
             total_ylls = sum(yll),
             total_ylds = sum(ylds),
             total_deaths = sum(deaths)) -> cause_data
cause_data %>% arrange(-total_dalys) -> temp
top_cause_data<- head(temp,10)

prep_data %>% 
  group_by(age) %>% 
  summarise( total_dalys = sum(dalys),
             total_ylls = sum(yll),
             total_ylds = sum(ylds),
             total_deaths = sum(deaths))-> age_data
age_data %>% arrange(-total_dalys) -> temp2
#top_age_data<- head(temp2,10)

prep_data %>% 
  group_by(age,sex) %>% 
  summarise( total_dalys = sum(dalys),
             total_ylls = sum(yll),
             total_ylds = sum(ylds),
             total_deaths = sum(deaths))-> sex_data
sex_data %>% arrange(-total_dalys) -> temp3
#top_sex_data<- head(temp3,10)

###########################################################################################
install.packages("gridExtra")
library(gridExtra)
p1<- ggplot(data=top_cause_data, aes(x=cause_name,y=total_dalys, fill = total_ylls)) +
  geom_bar(stat="identity", position="dodge", width = 0.25) +
  coord_flip() 
p2<- ggplot(data=top_cause_data, aes(x=cause_name,y=total_dalys, fill = total_ylds)) +
  geom_bar(stat="identity", position="dodge", width = 0.25) +
  coord_flip() 
grid.arrange(p1,p2, nrow=2, ncol=1)

###############################################################################################
top_cause_data %>% select(cause_name,total_dalys) ->t1
t1$Metric<- "DALYS"
colnames(t1)<-c("Causes","Burden","Metric") 
top_cause_data %>% select(cause_name,total_ylls) ->t2
t2$Metric<- "YLLs"
colnames(t2)<-c("Causes","Burden","Metric")
top_cause_data %>% select(cause_name,total_ylds) ->t3
t3$metric<- "YLDs"
colnames(t3)<-c("Causes","Burden","Metric")
top_cause_data2<- rbind(t1,t2,t3)

p3 <- ggplot(data=top_cause_data2, aes(x=Causes,y=Burden, fill = Metric)) +
  geom_bar(stat="identity", position="dodge", width = 0.5) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed')) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  facet_grid(Metric~.)

p4<- ggplot(data=top_cause_data2, aes(x=Causes,y=Burden, fill = Metric)) +
  geom_bar(stat="identity", position="dodge", width = 0.5) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed')) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

grid.arrange(p3,p4, nrow=1, ncol=2)
###############################################################################################

age_data %>% mutate(age_group = substr(age, 1, 2)) -> age_data
age_data[age_data$age_group=='Un',"age_group"]<- 0
age_data$age_group <- as.numeric(age_data$age_group)
#age_data %>% arrange(age_group) -> age_data
#age_data$age_group <- factor(age_data$age_group, levels = age_data$age_group) 
ggplot(age_data, aes(x=total_dalys, y=reorder(age, age_group))) + 
  geom_point(aes(size = total_ylls), colour = "orange") +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))

p5<- ggplot(data=age_data, aes(x=reorder(age, age_group),y=total_ylls)) +
  geom_bar(stat="identity", position="dodge", width = 0.25, fill = "blue", color = "white") +
  coord_flip()+
  ylim(0,30000000)+
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))
p6<- ggplot(data=age_data, aes(x=reorder(age, age_group),y=total_ylds)) +
  geom_bar(stat="identity", position="dodge", width = 0.25, fill = "blue", color = "white") +
  coord_flip()+
  ylim(0,30000000)+
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))
grid.arrange(p5,p6, nrow=1, ncol=2)

#############################################################################################
sex_data %>% mutate(age_group = substr(age, 1, 2)) -> sex_data
sex_data[sex_data$age_group=='Un',"age_group"]<- 0
sex_data$age_group <- as.numeric(sex_data$age_group)
#sex_data %>% arrange(age_group) -> sex_data

ggplot(data = sex_data, aes(x=reorder(age,age_group), y=total_dalys, fill = sex)) +
  geom_bar(stat="identity", position="dodge", width = 0.65) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))



p8<- ggplot(data=sex_data, aes(x=reorder(age, age_group),y=total_ylls, fill = sex)) +
  geom_bar(stat="identity", position="dodge", width = 0.5) +
  coord_flip()+
 ylim(0,15000000)+
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))
p9<- ggplot(data=sex_data, aes(x=reorder(age, age_group),y=total_ylds, fill = sex)) +
  geom_bar(stat="identity", position="dodge", width = 0.5) +
  coord_flip()+
  ylim(0,15000000)+
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))
grid.arrange(p8,p9, nrow=1, ncol=2)

##############################################################################################
# 
# ggplot(data=top_cause_data, aes(x=cause_name,y=total_dalys)) +
#   geom_bar(stat="identity", position="dodge", width = 0.25, fill = "light blue") +
#   coord_flip() +
#   coord_polar(start = 0)+ 
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-2,4), "cm"))+
# geom_text(data=top_cause_data, aes(x=cause_name, y=total_dalys, label=cause_name), color="black", size=3,  inherit.aes = FALSE )
##############################################################################################

prep_data%>% 
  group_by(cause_name, age, sex) %>% 
  summarise(total_dalys = sum(dalys),
            total_ylls = sum(yll),
            total_ylds = sum(ylds),
            total_deaths = sum(deaths)) -> trial
trial %>% arrange(-total_dalys) -> trial2
trial3<- head(trial2,10)  
ggplot(data=trial3, aes(x=cause_name,y=total_ylls)) +
  geom_bar(stat="identity", position="dodge", width = 0.5, fill = "light blue") +
  coord_flip()+
  ylim(0,15000000)+
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))
trial %>% arrange(cause_name) %>% mutate(age_group = substr(age, 1, 2)) -> trial
trial[trial$age_group=='Un',"age_group"]<- 0
trial %>% filter(age_group>20 & age_group<65) ->trial4
trial4 %>% arrange(-total_ylls) %>% select(cause_name,sex,age,total_dalys,total_ylls,total_ylds)-> trial4
final_table<-head(trial4,15)

prep_data%>% 
  group_by(cause_name, age) %>% 
  summarise(total_dalys = sum(dalys),
            total_ylls = sum(yll),
            total_ylds = sum(ylds),
            total_deaths = sum(deaths)) -> test
test %>% arrange(age,-total_dalys) %>% group_by(age) ->test3
test %>% arrange(-total_dalys, age) ->test2

