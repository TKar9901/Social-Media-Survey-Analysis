## MTH1004 Course Work 1 R Script

# Import Libraries and Data
library(tidyverse)
data = read_csv("survey.csv")
data

## 1 Introduction

# How many people responded to the survey?
data %>%
  summarise(total_respondents = n())

# Over what time period did the survey run?
data %>%
  summarise(start_dttm = min(time),
            end_dttm = max(time))

# Which social media platforms do survey respondents use, and how many
# respondents use each platform?
data %>%
  group_by(Q2) %>%
  summarise(no_respondents = n())

# Analyse the age distribution of respondents (median, lower and upper
# quartile), and comment on how representative the survey might be of the
# whole population of Bangladesh.
data %>%
  summarise(lower_q = quantile(age, 0.25),
            median = median(age),
            upper_q = quantile(age, 0.75))

ggplot(data) +
  geom_boxplot(aes(x=age))

# Use appropriate R commands to produce a small map that highlights the
# country outlines of Bangladesh, as shown in Figure 1.

world = map_data("world")
bang = world[world$region == "Bangladesh",]

world %>%
  filter(between(long, 70, 120),
         between(lat, 5, 45)) %>%
  ggplot() +
    geom_path(aes(x=long, y=lat, group=group),
              show.legend=FALSE) +
    geom_path(data=bang,
              aes(x=long, y=lat, group=group),
              colour="red", show.legend=FALSE)

## 2 Social media usage and sleep quality

# Using suitable summary statistics, summarise the responses to the two
# questions related to sleep quality and daily social media usage.
data %>%
  group_by(Q1) %>%
  summarise(count = n())

# Plot a suitable bar chart that visualizes sleep quality among the 
# different groups of daily social media usage, and briefly describe the
# bar chart
ggplot(data, aes(x=Q1, fill=Q5)) +
  geom_bar(position="dodge", colour="black") +
  geom_text(aes(label=after_stat(count)), stat="count",
            vjust=2, position=position_dodge(0.9), colour="white") +
  theme(legend.position=c(0.9, 0.8))

# Discuss whether you think social media usage might have an effect on sleep
# quality. Support your analysis by suitable summary statistics.
getmode <- function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

d = data %>%
  select(age, gender, Q1, Q5) %>%
  group_by(age, gender) %>%
  summarise(mode_sm_time = getmode(Q1),
            mode_sleep_quality = getmode(Q5)) %>%
  arrange(age)

ggplot(d) +
  geom_smooth(aes(x=age, y=mode_sleep_quality, group=gender, colour=gender),
              level=0.35) +
  geom_point(aes(x=age, y=mode_sleep_quality, group=gender, colour=gender),
             alpha=0.4, size=1) +
  geom_smooth(aes(x=age, y=mode_sm_time, group=gender, colour=gender),
              level=0.35) +
  geom_point(aes(x=age, y=mode_sm_time, group=gender, colour=gender),
             alpha=0.4, size=1) +
  labs(x="Age (years)",
       y="Mode daily time on SM (Hours)                 Mode sleep quality") +
  theme(legend.position=c(0.9, 0.5))


##  3 Social media usage and mental health

# Is there evidence that some social media platforms are more engaging
# or “addictive” than others?
data %>%
  group_by(Q2) %>%
  summarise(no_r = n(),
            mode_t = getmode(Q1)) %>%
  arrange(desc(no_r))

data %>%
  group_by(Q1) %>%
  summarise(no_r = n(),
            mode_sm = getmode(Q2))

data %>%
  group_by(gender) %>%
  summarise(no_r = n(),
            mode_t = getmode(Q1),
            mode_sm = getmode(Q2))







