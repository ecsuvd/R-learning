#------------------------------
# Exercise 15.3.1 R4DS: Explore income distribution in forcats::gss_cat data.
#------------------------------
library("forcats")
library("tidyverse")

summarize(group_by(gss_cat, rincome))
tt<-gss_cat
tt$rincome <-as.character(tt$rincome)
tt$rincome[tt$rincome %in% c("No answer","Don't know","Refused","Not applicable")] <- "NA"
# it's probably better to have income intervals with equal width:
tt$rincome[tt$rincome %in% c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999")] <- "Lt 5000"
tt$rincome[tt$rincome %in% c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999")] <- "$5000 - 9999"
income_levels=c("Lt 5000","$5000 - 9999", "$10000 - 14999", "$15000 - 19999", "$20000 - 24999", "$25000 or more")
yy<-within(tt, rincome<-factor(rincome, levels=income_levels))  
ggplot(yy, aes(rincome)) + geom_bar() + theme(text = element_text(size=5))
count(yy, rincome)
# About one third of the survey participants has not provided their income. 
# In some other cases of analyses the reasons of not providing income should not be replaced as NA just as I did. 


