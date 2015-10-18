library(dplyr)
setwd("/Users/magnusglindvad/Documents/Rprogramming/RepResearch")
activity = read.csv("RepData_PeerAssessment1/activity.csv")
tot_steps_day = with(activity, tapply(steps, date, sum, na.rm = T))

mean_steps_int = with(activity, tapply(steps, interval, mean, na.rm = T))

act.fill.nas = function(x) {
        y = vector()
        for (i in seq_along(x))
                if(is.na(x[i])) {
                        int.val = as.character(activity[,"interval"][i])
                        index = which(names(mean_steps_int) == int.val)
                        y[i] = mean_steps_int[index]
                } else {
                        y[i] = x[i]
                }
        x = y
        x
}
activity$steps = act.fill.nas(activity$steps)
head(activity)

##Lørdag and Søndag is Danish for Saturday and Sunday
activity = mutate(activity,
                  date = as.Date(date, format = "%Y-%m-%d"),
                  daytype = ifelse(weekdays(date) %in% c("Lørdag", "Søndag"), "weekend", "weekday")) %>%
        group_by(daytype, interval) %>%
        summarise(av_steps = mean(steps))

ggplot(activity, aes(x = interval, y = av_steps)) +
        geom_line() + 
        facet_wrap( ~ daytype)