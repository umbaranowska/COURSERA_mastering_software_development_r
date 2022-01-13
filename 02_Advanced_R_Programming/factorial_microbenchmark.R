library(tidyverse)
library(microbenchmark)

# loading factorial functions from script
source('./factorial_code.R')

# note:
# all 4 factorial functions should return appropriate errors in cases such as these:
factorial_loop(-1) # n must be greater than or equal to 0
factorial_loop(5.63) # n must be an integer!
factorial_loop('is it a boulder or is it a big rock?') # n must be an integer!

##### SIMPLE BARPLOT COMPARING MEAN TIMES FOR DIFFERENT INPUTS #####

time_comparison = microbenchmark(factorial_loop(0),
                                 factorial_func(0),
                                 factorial_reduce(0),
                                 factorial_mem(0),
                                 factorial_loop(2),
                                 factorial_func(2),
                                 factorial_reduce(2),
                                 factorial_mem(2),
                                 factorial_loop(5),
                                 factorial_func(5),
                                 factorial_reduce(5),
                                 factorial_mem(5),
                                 factorial_loop(7),
                                 factorial_func(7),
                                 factorial_reduce(7),
                                 factorial_mem(7),
                                 factorial_loop(10),
                                 factorial_func(10),
                                 factorial_reduce(10),
                                 factorial_mem(10)) %>% as.data.frame()

time_comparison = time_comparison %>% 
  group_by(expr) %>%
  summarize(mean_time = mean(time)) %>%
  mutate(expr = str_replace(expr, "\\(", " "),
         expr = str_remove(expr, "\\)")) %>%
  separate(expr, c('expr', 'value'), sep = " ", extra = "merge")

ggplot(time_comparison, aes(x = value, y = mean_time, fill = expr)) +
  geom_bar(stat = 'identity', position = 'dodge')


##### .TXT FILE REQUIRED IN THE ASSIGNMENT #####
sink('factorial_output.txt')
  microbenchmark(factorial_loop(0),
                 factorial_func(0),
                 factorial_reduce(0),
                 factorial_mem(0),
                 factorial_loop(2),
                 factorial_func(2),
                 factorial_reduce(2),
                 factorial_mem(2),
                 factorial_loop(5),
                 factorial_func(5),
                 factorial_reduce(5),
                 factorial_mem(5),
                 factorial_loop(7),
                 factorial_func(7),
                 factorial_reduce(7),
                 factorial_mem(7),
                 factorial_loop(10),
                 factorial_func(10),
                 factorial_reduce(10),
                 factorial_mem(10))
sink()
