library(statsr)
library(dplyr)
library(ggplot2)

data(nycflights)

names(nycflights)

str(nycflights)

ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 150)

rdu_flights <- nycflights %>%
  filter(dest == "RDU")
ggplot(data = rdu_flights, aes(x = dep_delay)) +
  geom_histogram()

rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)

ggplot(data = sfo_feb_flights, aes(x = arr_delay)) +
  geom_histogram()
sfo_feb_flights %>%
  summarise(median = median(arr_delay))

rdu_flights %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(IQR = IQR(arr_delay))

nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))

nycflights %>%
  group_by(month) %>%
  summarise(median = median(dep_delay)) %>%
  arrange(desc(median))

ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()

nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))

ggplot(data = nycflights, aes(x = origin, fill = dep_type)) +
  geom_bar()

nycflights <- nycflights %>%
  mutate(avg_speed = distance / (air_time / 60)) %>%
  arrange(desc(avg_speed))

ggplot(data = nycflights, aes(x = distance, y = avg_speed))+
  geom_point()

nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed"))

nycflights %>%
  group_by(dep_type) %>%
  summarise(sum(arr_type == "on time") / n())


