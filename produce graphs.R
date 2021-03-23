library(ggplot2)
library(dplyr)
library(readxl)
library(reshape2)
library(hrbrthemes)
library(RColorBrewer)

setwd("C:/Users/Daria/Dropbox/Childcare_female_employment/data")

# average weekly hours worked 

average_hrs_worked_women_24_54_xls <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/average hrs worked women 24-54.xls.xlsx",
  sheet = "Sheet2")

a_text <- ifelse(average_hrs_worked_women_24_54_xls$Country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(average_hrs_worked_women_24_54_xls$Country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(average_hrs_worked_women_24_54_xls$Country=="Россия", "#ffbb33", "#000000")

average_hrs_worked_women_24_54_xls %>% ggplot(mapping = aes(x=reorder(Country,-`2019`),
                                                            y=`2019`)) + 
  geom_col(width = 0.5, show.legend = FALSE, fill=a_bar) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(axis.text.x = element_text(angle=60, hjust=0.9,color = a_text) ) + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())



# share of women working 30+ hours

data_for_graphs <- read_excel("data for graphs.xlsx", sheet = "working hours>30")

data_for_graphs %>% melt() %>%  ggplot(aes(Year, value, color=variable, group=variable)) + 
  geom_line() +
  labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9))+  
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())+
  scale_color_discrete(labels = c("Восточная Европа", "Cеверная Европа", "Россия", "ОЭСР", "Западная Европа", "Южная Европа"))+
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm"), legend.position="top")

ggsave("working hours 30.svg", width=16.5, height = 8, units="cm")


# employment/pop ratio women

data_for_graphs <- read_excel("data for graphs.xlsx", sheet = "empl-to-pop women OECD", range = "A28:G48")

data_for_graphs %>% melt() %>%  ggplot(aes(Year, value, color=variable, group=variable)) + 
  geom_line() +
  labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9))+  
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())+
  scale_color_discrete(labels = c("Восточная Европа", "Cеверная Европа", "Россия", "ОЭСР", "Западная Европа", "Южная Европа"))+
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm"), legend.position="top")

ggsave("empl-to-pop women OECD.svg", width=16.5, height = 8, units="cm")

# mothers employment with children under 14

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
sheet = "employment rate children<14")

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")

data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-Employment_rate)

data_for_graphs$country <- reorder(data_for_graphs$country,-data_for_graphs$Employment_rate) 

data_for_graphs %>% melt() %>% filter(variable!="Employment_rate") %>% ggplot(mapping = aes(x=country,
                                         y=value, fill=variable)) + 
  geom_col(width = 0.5,  position = "stack") + 
  labs(x=NULL, 
       y=NULL) + 
  scale_fill_brewer(palette = "Set2")+
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank(), 
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm"), 
        legend.margin = margin(0, 0, 0, 0, "cm")) +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

ggsave("employment mothers 14.svg", width=16.5, height = 8, units="cm")

# mother employment agechild

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "mother employed agechild")

data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-have_children)
data_for_graphs$country <- reorder(data_for_graphs$country,-data_for_graphs$have_children) 

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_dot1 <- ifelse(data_for_graphs$country=="Россия", "#ffff33", "#66ffff") #кружок
a_dot2 <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#3385ff") #ромб
a_dot3 <- ifelse(data_for_graphs$country=="Россия", "#ff471a", "#0000b3") #треугольник

cols    <- c( "c1" =  "#66ffff", "c2" = "#3385ff" , "c3" = "#0000b3")
shapes  <- c("s1" = 21, "s2" = 23, "s3" = 24)

data_for_graphs %>% ggplot(mapping = aes(x=country)) + 
  geom_linerange(aes(x=country, ymax=fourteen, ymin=five), color="#bfbfbf")+
  geom_linerange(aes(x=country, ymax=fourteen, ymin=two) , color="#bfbfbf")+
  geom_point(aes(y=fourteen, colour="c1", shape = "s1"), fill=a_dot1, size=3)+
  geom_point(aes(y=five, colour="c2", shape = "s2"), fill=a_dot2, size=3)+
  geom_point(aes(y=two, colour="c3", shape = "s3"),fill=a_dot3, size=3)+
  scale_color_manual(name = "", 
                     breaks = c("c1", "c2", "c3"), 
                     values = cols,
                     labels =c("6-14", "3-5", "0-2"))+
  scale_shape_manual(name = "", 
                     breaks = c("c1", "c2", "c3"), 
                     values = shapes,
                     labels =c("6-14", "3-5", "0-2"))+
  labs(x=NULL, y=NULL) + 
  guides(fill = guide_legend(reverse=T))+
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm"), legend.position="top") +
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank())

ggsave("mother_empl_agechild.svg", width=16.5, height = 8, units="cm")


# mother employment numchild

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "mother employment numchild")

data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-have_children)
data_for_graphs$country <- reorder(data_for_graphs$country,-data_for_graphs$have_children) 

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_dot1 <- ifelse(data_for_graphs$country=="Россия", "#ffff33", "#66ffff") #кружок
a_dot2 <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#3385ff") #ромб
a_dot3 <- ifelse(data_for_graphs$country=="Россия", "#ff471a", "#0000b3") #треугольник

cols    <- c( "c1" =  "#66ffff", "c2" = "#3385ff" , "c3" = "#0000b3")
shapes  <- c("s1" = 21, "s2" = 23, "s3" = 24)

data_for_graphs %>% ggplot(mapping = aes(x=country)) + 
  geom_linerange(aes(x=country, ymax=one, ymin=three), color="#bfbfbf")+
  geom_linerange(aes(x=country, ymax=one, ymin=two) , color="#bfbfbf")+
  geom_point(aes(y=one, colour="c1", shape = "s1"), fill=a_dot1, size=3)+
  geom_point(aes(y=two, colour="c2", shape = "s2"), fill=a_dot2, size=3)+
  geom_point(aes(y=three, colour="c3", shape = "s3"),fill=a_dot3, size=3)+
  scale_color_manual(name = "", 
                     breaks = c("c1", "c2", "c3"), 
                     values = cols,
                     labels =c("Один ребенок", "Два ребенка", "Три и более"))+
  scale_shape_manual(name = "", 
                     breaks = c("c1", "c2", "c3"), 
                     values = shapes,
                     labels =c("Один ребенок", "Два ребенка", "Три и более"))+
  labs(x=NULL, y=NULL) + 
  guides(fill = guide_legend(reverse=T))+
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm"), legend.position="top") +
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank())

ggsave("mother_empl_numchild.svg", width=16.5, height = 8, units="cm")



# gender pay gap

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "gender gap")

data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-`Взвешенный по факторам`)

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")

data_for_graphs$country <- reorder(data_for_graphs$country,-data_for_graphs$"Взвешенный по факторам") 

data_for_graphs %>% melt() %>%  ggplot(mapping = aes(x=country,  y=value, fill=variable)) + 
  geom_col(width = 0.5,  position = "dodge")+
  labs(x=NULL, 
       y=NULL) + 
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

# inequality index

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "inequality index")

data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-index)

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")

data_for_graphs %>% ggplot(mapping = aes(x=reorder(country,-index),
                                         y=index)) + 
  geom_col(width = 0.5, show.legend = FALSE, fill=a_bar) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
ggsave("inequality index.svg", width=16.5, height = 8, units="cm")


# gender role 1

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "gender role 1")
 
data_for_graphs$country <- reorder(data_for_graphs$country, -data_for_graphs$female_answer) 
data_for_graphs$country <- reorder(data_for_graphs$country, data_for_graphs$region) 

data_for_graphs <- data_for_graphs  %>% arrange(-female_answer,desc(region)) 

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")

ggplot(dat, aes(grouping, value, fill=letters, label = letters)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(position = position_dodge(width = 1), aes(x=grouping, y=0))

data_for_graphs %>% mutate(region = factor(region, unique(region))) %>% ggplot(mapping = aes(x=country, fill=region)) + 
  geom_col(width = 0.5,aes(y=female_answer)) + 
  geom_point(aes(y=male_answer, fill=region))+
  labs(x=NULL, 
       y=NULL) + 
  
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank()) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())





# parental leave fathers 1

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "parental leave fathers")


data_for_graphs <- data_for_graphs %>% select(country,`Отпуск`=`Отпуск...2`, `Выплаты`=`Выплаты...3`) %>% arrange(country)  %>% arrange(-Отпуск) 

data_for_graphs$country <- reorder(data_for_graphs$country, -data_for_graphs$Отпуск) 

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")

data_for_graphs %>% ggplot(mapping = aes(x=country) ) + 
  geom_col(aes(y=Отпуск), width = 0.5, fill=a_bar) + 
  geom_point(aes(y=Выплаты/20),colour=a_dot, size=2)+
  scale_y_continuous( name = "Длина отпуска (недели)", 
    sec.axis = sec_axis(~.*20, name="Выплаты в % от зарплаты")
  ) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(
    axis.title.y = element_text(color = "#69b3a2"),
    axis.title.y.right = element_text(colour = 'black')
  ) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())


# parental leave fathers 2

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "parental leave fathers")

data <- data_for_graphs %>% select(country,`Отпуск`=`Отпуск...4`, `Выплаты`=`Выплаты...5`) %>% arrange(country)  %>% arrange(-Отпуск)

data$country <- reorder(data$country, -data$Отпуск) 

a_text <- ifelse(data$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data$country=="Россия", "#ffbb33", "#000000")

data %>% ggplot(mapping = aes(x=country) ) + 
  geom_col(aes(y=Отпуск), width = 0.5, fill=a_bar) + 
  geom_point(aes(y=Выплаты*9/10),colour=a_dot, size=2)+
  scale_y_continuous( name = "Длина отпуска (недели)", 
                      sec.axis = sec_axis(~.*10/9, name="Выплаты в % от зарплаты")
  ) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(
    axis.title.y = element_text(color = "#69b3a2"),
    axis.title.y.right = element_text(colour = 'black')
  ) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())



# parental leave moters 1

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "parental leave mothers")


data_for_graphs <- data_for_graphs %>% select(country,`Отпуск`=`Отпуск...2`, `Выплаты`=`Выплаты...3`) %>% arrange(country)  %>% arrange(-Отпуск) 

data_for_graphs$country <- reorder(data_for_graphs$country, -data_for_graphs$Отпуск) 

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")

data_for_graphs %>% ggplot(mapping = aes(x=country) ) + 
  geom_col(aes(y=Отпуск), width = 0.5, fill=a_bar) + 
  geom_point(aes(y=Выплаты*6/10),colour=a_dot, size=2)+
  scale_y_continuous( name = "Длина отпуска (недели)", 
                      sec.axis = sec_axis(~.*10/6, name="Выплаты в % от зарплаты")
  ) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(
    axis.title.y = element_text(color = "#69b3a2"),
    axis.title.y.right = element_text(colour = 'black')
  ) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())


# parental leave mothers 2

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "parental leave mothers")

data <- data_for_graphs %>% select(country,`Отпуск`=`Отпуск...4`, `Выплаты`=`Выплаты...5`) %>% arrange(country)  %>% arrange(-Отпуск)

data$country <- reorder(data$country, -data$Отпуск) 

a_text <- ifelse(data$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data$country=="Россия", "#ffbb33", "#000000")

data %>% ggplot(mapping = aes(x=country) ) + 
  geom_col(aes(y=Отпуск), width = 0.5, fill=a_bar) + 
  geom_point(aes(y=Выплаты*1.5),colour=a_dot, size=2)+
  scale_y_continuous( name = "Длина отпуска (недели)", 
                      sec.axis = sec_axis(~./1.5, name="Выплаты в % от зарплаты")
  ) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(
    axis.title.y = element_text(color = "#69b3a2"),
    axis.title.y.right = element_text(colour = 'black')
  ) +
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())



# weekly hours in kindergarten


data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "weekly hours preschool")


data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-Hours)


a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")

data_for_graphs %>% ggplot(mapping = aes(x=reorder(country,-Hours),
                                         y=Hours)) + 
  geom_col(width = 0.5, show.legend = FALSE, fill=a_bar) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

# enrolment 0-2

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "enrolment02")


data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-enrolment)


a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")


data_for_graphs %>% ggplot(mapping = aes(x=reorder(country,-enrolment),
                                         y=enrolment)) + 
  geom_col(width = 0.5, show.legend = FALSE, fill=a_bar) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

ggsave("enrolment02.svg", width=16.5, height = 8, units="cm")

# enrolment 3-5

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "enrolment35")


data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-enrolment)


a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")


data_for_graphs %>% ggplot(mapping = aes(x=reorder(country,-enrolment),
                                         y=enrolment)) + 
  geom_col(width = 0.5, show.legend = FALSE, fill=a_bar) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

ggsave("enrolment35.svg", width=16.5, height = 8, units="cm")


# public spendings on education as % of GDP

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "public spendings")

data_for_graphs <- data_for_graphs %>% arrange(country)  %>% arrange(-spendings)


a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")
a_bar <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#69b3a2")
a_dot <- ifelse(data_for_graphs$country=="Россия", "#ffbb33", "#000000")


data_for_graphs %>% ggplot(mapping = aes(x=reorder(country,-spendings),
                                         y=spendings)) + 
  geom_col(width = 0.5, show.legend = FALSE, fill=a_bar) + 
  labs(x=NULL, 
       y=NULL) + 
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

ggsave("public spendings.svg", width=16.5, height = 8, units="cm")


# share mother-father leave users

data_for_graphs <- read_excel("C:/Users/Daria/Dropbox/Childcare_female_employment/data/data for graphs.xlsx",
                              sheet = "mother-father")

data_for_graphs <- data_for_graphs  %>% arrange(country)  %>% arrange(share1)

data_for_graphs$country <- reorder(data_for_graphs$country,data_for_graphs$share1) 

data_for_graphs <- melt(data_for_graphs)

a_text <- ifelse(data_for_graphs$country=="Россия", "#ff6666", "#000000")

data_for_graphs %>% ggplot(mapping = aes(x=country,
                                                    y=value, fill=variable)) + 
  geom_col(width = 0.5, position="fill") + 
  labs(x=NULL, 
       y=NULL) + 
  theme(axis.text.x = element_text(angle=60, hjust=0.9, color=a_text)) + 
  scale_fill_discrete(labels =c("Доля женщин", "Доля мужчин"))+
  theme(legend.background = element_rect(fill="#f7f7f7"), legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5,"cm"), legend.position="top") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank())

ggsave("mother_father.svg", width=16.5, height = 8, units="cm")
