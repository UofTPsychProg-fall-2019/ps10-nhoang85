setwd('C:/Users/hoang/Documents/0_MASTERS/Programming/ps10-nhoang85')
getwd()

library(tidyverse)
install.packages("Hmisc")
library(Hmisc)


# This problem set will test out your ggploting skills using the Big 5 health dataset 
# that you wrangled in problem set 9

# load in the data
ipip <- read_csv('ipip50_sample.csv')

# As a reminder, this dataset includes measures of the Big 5 Inventory personality index, which
# measures traits of Agreeableness, Conscientiousness, Extroversion, 
# Neuroticism, and Openness, along with measures of age, BMI, exercise 
# habits, and median income for 1000 participants. 
# 
# In the dataset, each trait has a set of associated survey items (e.g., 
# Agreeableness has A_1, A_2, A_3, ... A_10). The total number of  items 
# vary for the different traits (e.g., Agreeableness has 10, but Openness only 
# has 2). For each participant, there are measures for each of the items as well
# as the participant's age, BMI, gender, exercise habits which are 
# categorically coded in terms of frequency, and log transformed median income.

# For each of the questions, you can find an example plot in the figures folder
# Your graph should use the same geoms, and map the same variables 
# to the same aesthetics
# Feel free to get creative when customizing color, lines, etc!

# wrangling the data into long fromat ---------------------------------------
# This pipeline will wrangle the wide data into a tidy long fromat for easy plotting
# It's basically what you worked on in PS9

# recode data in wide format 
ipip <- ipip %>% 
    mutate(exer=factor(exer,levels=c('veryRarelyNever','less1mo','less1wk',
                                     '1or2wk','3or5wk','more5wk')),  #orders levels of exercise appriately
           BMI_cat=case_when(BMI<18.5~'underweight',
                             BMI>=18.5&BMI<25~'healthy',
                             BMI>=25&BMI<30~'overweight',
                             BMI>=30~'obese'), #geneartes a categorical version of BMI
           BMI_cat=factor(BMI_cat, levels=c('underweight', 'healthy', 'overweight','obese'))) #orders levels of BMI_cat
levels(ipip$BMI_cat)

# generate long format
ipip.l <- ipip %>% 
    gather(A_1:O_10,key=item,value=value) %>% 
    separate(item,into=c('trait','item'),sep='_')  %>% 
    group_by(RID,trait) %>% 
    summarise(value=mean(value)) %>%  #calculates average trait values 
    left_join(select(ipip,RID,age,gender,BMI,BMI_cat,exer,logMedInc),.) #merges summarized traits with health and income information
    

# note that the original wide data frame (ipip) will come in handy when examining relationships that don't depend on personality 
# because this data frame has one row per person
# for each of the question below, be sure to use the most appropriate data frame

# Q1 visulizing BMI's relationship to exercise habits ---------------------------------------

# create a boxplot that visualizes BMI distributions according to exercise habits, separately for females and males
# include at least two customizations to the look of the boxplot 
# check the documentation for options
Q1 <-ipip %>% 
    ggplot(aes(x = exer, y = BMI, color = gender)) +
    geom_boxplot( notch = TRUE) +
    scale_color_manual(values=c("blueviolet", "cyan"))+
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title='Body Mass Index (BMI) by Exercise frequency',
         x='Exercise', y='BMI')

Q1 <- Q1 + theme(plot.title=element_text(size=16, face="bold", color="tomato", hjust=0.5, lineheight=1.2),
             axis.title.x=element_text(vjust=-1, size=15),
             axis.title.y=element_text(vjust=3,size=15),
             axis.text.x=element_text(size=10),
             axis.text.y=element_text(size=10))
Q1
ggsave('figures/Q1.pdf',units='in',width=4,height=5)

# Q2 visulizing BMI's relationship to income  ---------------------------------------

# create a scatter plot to visualize the relationship between income and BMI, coloring points according to gender
# use geom_smooth to add linear model fit lines, separately for males and females
Q2a <- ipip %>%  
    ggplot(aes(x=logMedInc,y=BMI, color=gender))+
    geom_point(size=.5,alpha=.4)+
    scale_color_manual(values=c("blueviolet", "cyan"))+
    geom_smooth(method='lm') +
    theme_bw(base_size = 14) +
    theme(legend.position = c(.85, .85),legend.background=element_blank()) +
    labs(title='Body Mass Index (BMI) by Income',
         x='Income', y='BMI')

Q2a <- Q2a + theme(plot.title=element_text(size=16, face="bold", color="forestgreen", hjust=0.5, lineheight=1.2),
                 axis.title.x=element_text(vjust=-1, size=15),
                 axis.title.y=element_text(vjust=3,size=15),
                 axis.text.x=element_text(size=10),
                 axis.text.y=element_text(size=10))

Q2a
ggsave('figures/Q2a.pdf',units='in',width=4,height=5)

# there are some outlying lower income points, especially for females
# recreate this graph filtering for log median income>10
Q2b <- ipip %>%  
    filter(logMedInc >10) %>% 
    ggplot(aes(x=logMedInc,y=BMI, color=gender))+
    geom_point(size=1.0,alpha=.4)+
    scale_color_manual(values=c("blueviolet", "cyan"))+
    geom_smooth(method='lm') +
    theme_bw(base_size = 14) +
    theme(legend.position = c(.85, .85),legend.background=element_blank()) +
    labs(title='Body Mass Index (BMI) by Income',
         x='Income', y='BMI')

Q2b <- Q2b + theme(plot.title=element_text(size=16, face="bold", color="forestgreen", hjust=0.5, lineheight=1.2),
                   axis.title.x=element_text(vjust=-1, size=15),
                   axis.title.y=element_text(vjust=3,size=15),
                   axis.text.x=element_text(size=10),
                   axis.text.y=element_text(size=10))


Q2b
ggsave('figures/Q2b.pdf',units='in',width=4,height=5)


# Q3 visualizing income's relationship with exercise habits  ---------------------------------------

# create a bar graph displaying the average income level of men and women who exercise at different rates
# add errorbars reflecting bootstrapped confidence intervals using the Hmisc package
# the default range on the y-axis will be very large given the range of the data
# add a +coord_cartesian(ylim = c(10, 12)) to rescale it.

Q3 <- ipip %>%  
    ggplot(aes(x= gender,y=logMedInc, color = exer, fill=exer))+
    stat_summary(fun.y=mean, geom="bar", position=position_dodge(width=.9), alpha = .4) +
    stat_summary(fun.data='mean_cl_boot', geom="errorbar", position=position_dodge(width=.9), width=.3) +
    coord_cartesian(ylim = c(10, 12)) +
    theme_bw(base_size = 14) +
    labs(title='Gender Differences in Average Income As Function of Exercise Rate',
         x='Gender', y='Average Income (logMedInc)')

Q3 <- Q3 + theme(plot.title=element_text(size=8, face="bold", color="blue1", hjust=0.5, lineheight=1.2),
                   axis.title.x=element_text(vjust=-1, size=15),
                   axis.title.y=element_text(vjust=3,size=15),
                   axis.text.x=element_text(size=10),
                   axis.text.y=element_text(size=10))
Q3
ggsave('figures/Q3.pdf',units='in',width=4,height=5)

# Q4 visualizing gender differences in personality as function of BMI  ---------------------------------------

# use stat_summary with the pointrange geom to plot bootstrapped confidence intervals around mean personality trait values
# for each BMI category, separately for males and females
# this is a lot to visualize in a single plot! use +facet_wrap(vars(trait)) to generate seperate plots for each personality trait

Q4 <- ipip.l %>%  
    ggplot(aes(x=BMI_cat,y=value, color=gender))+
    stat_summary(fun.data='mean_cl_boot', geom="pointrange",position=position_dodge(.5)) +
    scale_color_manual(values=c("blueviolet", "cyan"))+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(vars(trait)) +
    labs(title='Gender Differences in Mean Personality Trait As Function of BMI Category',
     x='BMI Category', y='Mean Trait Value')

Q4 <- Q4 + theme(plot.title=element_text(size=7, face="bold", color="darkmagenta", hjust=0.5, lineheight=1.2),
                 axis.title.x=element_text(vjust=-1, size=15),
                 axis.title.y=element_text(vjust=2,size=15),
                 axis.text.x=element_text(size=10),
                 axis.text.y=element_text(size=10))
Q4
ggsave('figures/Q4.pdf',units='in',width=4,height=5)


# Q5 re-visualizing gender differences in personality as function of BMI  ---------------------------------------

# use dplyr functions to calculate the mean of each personality trait for each combination of gender, BMI group
ipip.g <- ipip.l %>%
    group_by(trait,gender, BMI_cat) %>% 
    summarise(Big5_mean = mean(value))


# plot the average value of personality trait (colored as separate lines), according to the BMI category
# facet_warp gender so that you can see these relationships separately for females and males
Q5 <- ipip.g %>% 
    ggplot(aes(x=BMI_cat, y= Big5_mean, color = trait, group = trait))+
    geom_point()+
    geom_line()+
    facet_wrap(~gender) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title='Gender Differences in Mean Personality Trait As Function of BMI Category',
         x='BMI Category', y='Mean Trait Value')

Q5 <- Q5 + theme(plot.title=element_text(size=7, face="bold", color="firebrick1", hjust=0.5, lineheight=1.2),
                 axis.title.x=element_text(vjust=-1, size=15),
                 axis.title.y=element_text(vjust=2,size=15),
                 axis.text.x=element_text(size=10),
                 axis.text.y=element_text(size=10))
Q5
ggsave('figures/Q5.pdf',units='in',width=4,height=5)
    
