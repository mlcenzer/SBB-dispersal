library(dplyr)
data = read.csv("/Users/anastasiabernat/Desktop/mother_laying_bydate.csv")

dates = unique(data$collect_date)[1:5]
twoweeks = data[data$collect_date == dates[1] | 
              data$collect_date == dates[2] |
              data$collect_date == dates[3] |
              data$collect_date == dates[4] |
              data$collect_date == dates[5]
            ,]
d = twoweeks[, c("MID", "n_eggs")]

test = summarise_at(group_by(d, MID), vars(n_eggs), funs(sum(.,na.rm=TRUE)))
mean(test$n_eggs) # mean is about 27 eggs
