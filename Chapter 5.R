library(tidyverse)
install.packages("nycflights13")
nycflights13::flights

# 5.24 Exercises

# 1.
  # 1) 
    filter(flights, arr_delay >= 120)
  # 2) 
    filter(flights, dest == 'IAH' | dest == 'HOU')
  # 3)  
    filter(flights, carrier %in% c('UA', 'AA', 'DL')) 
  # 4)
    filter(flights, month %in% c(7, 8, 9))
  # 5)
    filter(flights, arr_delay > 120 & dep_delay <= 0)       
  # 6)
    filter(flights, dep_delay >= 60 & arr_delay < dep_delay - 30) 
  # 7)
    filter(flights,dep_time >= 000 & dep_time <= 600)

# 2. between() is a shortcut for x >= left & x <= right. It could have been used 
  # to find flights that departed in the summer, and flights that departed between 12am and 6am.
filter(flights, between(month, 7,9))
filter(flights, between(dep_time, 000, 600))

# 3.
sum(is.na(flights$dep_time))
filter(flights, is.na(dep_time))
view(filter(flights, is.na(dep_time)))       
  #8225 flights have a missing departure time. Departure delay, arrival time, arrival delay, 
  # and airtime are also missing. It looks like the flights were cancelled.

# 5.3.1 Exercises

# 1. Use the arrange() function with a desc() function as an argument
arrange(flights,desc(is.na(dep_time)))

# 2.
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

# 3. 
arrange(flights, air_time)

# 4.
arrange(flights, desc(distance))
arrange(flights, distance)

# 5.5.2 Exercises

# 1. 
flights <- mutate(flights, mins_dep_time = dep_time %/% 100 * 60 + dep_time %% 100,
mins_sched_dep_time = sched_dep_time %/% 100 *60 + sched_dep_time %% 100)
select(flights, dep_time, mins_dep_time, sched_dep_time, mins_sched_dep_time)  

# 2.
flights <- mutate(flights, diff_air_time = arr_time - dep_time)
select(flights, air_time, diff_air_time)
view(flights)
  # Departure and arrival times are in 24 hour time, not in minutes air time.
  # By converting arrival and departure times to minutes then air time can be calculated.
flights <- mutate(flights, mins_arr_time = arr_time %/% 100 * 60 + arr_time %% 100,
                  mins_dep_time = dep_time %/% 100 * 60 + dep_time %% 100)
flights <- mutate(flights, diff_air_time = mins_arr_time - mins_dep_time)
select(flights, air_time, diff_air_time)
  # flight times and the difference between arrival times and departure times are still wrong.
  # it might be because the variable air_time does not include taxing time.

# 3. 
select(flights, mins_dep_time, mins_sched_dep_time, dep_delay)
flights <- mutate(flights, diff_dep_delay = mins_dep_time - mins_sched_dep_time)
select(flights, dep_delay, diff_dep_delay)
  # Departure delay is equal to the difference between departure time and scheduled departure time.

#4.
head(arrange(flights, min_rank(desc(dep_delay))),10)

# 5.6.7 Exercises

# 4.
flights <- mutate(flights, cancelled = is.na(dep_time))

flights %>% group_by(month, day) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            avg_cancelled = mean(cancelled)) %>%
  ggplot(mapping = aes(x = avg_dep_delay, y = avg_cancelled)) +
  geom_point() +  geom_smooth(method = 'lm', se = FALSE)
  # There is a positive linear relationship between the average departure delay and the proportion
  #  of cancelled flights

# 5.
delays <- flights %>% group_by(carrier) %>% 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE), 
            avg_dep_delay = mean(dep_delay, na.rm = TRUE))
  arrange(delays, desc(avg_arr_delay))
  arrange(delays, desc(avg_dep_delay))
  # Carrier F9 has the worst delays overall

delays <- flights %>% group_by(origin) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE))
  arrange(delays, desc(avg_dep_delay))
  arrange(delays, desc(avg_arr_delay))
  # The worst airport for delays is EWR
  
# 5.7.1 Exercises
  
# 2.
flights %>% group_by(tailnum) %>%
  filter(arr_delay > 0) %>%
  summarize(avg_arr_delay = mean(arr_delay)) %>%
  arrange(desc(avg_arr_delay))
 # The plane with the worst on-time record has tail number N844MH

# 3.
flights %>% filter (arr_delay > 0) %>% 
  group_by(hour) %>%
  summarize(avg_arr_delay = mean(arr_delay)) %>%
  arrange(avg_arr_delay)
  # the best time to fly to avoid delays is early in the morning

# 4. 
flights %>% select(dest, arr_delay) %>% group_by(dest) %>%
  filter(arr_delay > 0) %>%
  mutate(total_delay = sum(arr_delay, na.rm = TRUE),
         prop_delay = arr_delay / total_delay)
# 5. 
flights %>% filter(!is.na(dep_delay)) %>% filter(dest == 'IAH') %>% 
  mutate(prev_dep_delay = lag(dep_delay, default = 0)) %>% 
  ggplot(mapping = aes(x = dep_delay, y= prev_dep_delay)) +
  geom_point(alpha = .5)

  # ERROR
  #flights <- flights %>% filter(!is.na(dep_delay)) %>% filter(dest == 'IAH') %>% 
  #  mutate(prev_dep_delay = lag(dep_delay, default = 0))
  #  cor(flights$dep_delay, flights$prev_dep_delay) 

# 6.
flights %>% filter(!is.na(air_time)) %>% group_by(dest) %>%
  mutate(mean_air_time = mean(air_time),
         sd_air_time = sd(air_time),
         z_score = (air_time - mean_air_time) / sd_air_time) %>%
  select(z_score, mean_air_time, dest) %>%
  arrange(desc(z_score))
  #BOS has the highest variability in air time, with a z-score of 14.8.

# 7. 
  #ERROR
  # flights %>% group_by(dest) %>%
  #   summarise(mult_carrier = unique(carrier)) %>%
  #   filter(mult_carrier >= 2) %>%
  #   arrange(desc(mult_carrier))
   

# 8.
flights %>% filter(!is.na(dep_delay)) %>% group_by(tailnum) %>%
  mutate(less_than_1hour = dep_delay <= 60) %>%
  summarize(count = sum(less_than_1hour)) %>%
  arrange(desc(count))


                                                    