

source("R/brute_force_knapsack.R")
source("R/knapsack_dynamic.R")
source("R/greedy_knapsack.R")

run_time <- data.frame()

for (i in 8:20) {
  print (i)
  run_time <- rbind(run_time, 
  data.frame(rows = i,
    brute_force_knapsack = system.time(brute_force_knapsack(x = knapsack_objects[1:i,], W = 3500))[3],
    knapsack_dynamic = system.time(knapsack_dynamic(x = knapsack_objects[1:i,], W = 3500))[3],
    greedy_knapsack = system.time(greedy_knapsack(x = knapsack_objects[1:i,], W = 3500))[3])
  )
}

ggplot(run_time, aes(x=rows)) + 
  geom_line(aes(y = brute_force_knapsack, colour = "blue"), size = 1.1) +
  geom_line(aes(y = knapsack_dynamic, colour = "red"), size = 1.1) +
  geom_line(aes(y = greedy_knapsack, colour = "green"), size = 1.1) +
  scale_colour_discrete(labels = c("Brute Force", "Greedy", "Dynamic Programming")) + 
  theme_classic() + 
  scale_x_continuous(breaks = run_time$rows, labels = run_time$rows) + 
  labs(x = "Rows", y = "Time(s)", title = "Brute Force vs Greedy vs Dynamic Programming", color = "Algorithm") +
  ggsave("vignettes/3_algorithm_comparison.png")
  





run_time2 <- data.frame()

for (i in 30:100) {
  print (i)
  run_time2 <- rbind(run_time2, 
    data.frame(rows = i,
               knapsack_dynamic = system.time(knapsack_dynamic(x = knapsack_objects[1:i,], W = 3500))[3],
               greedy_knapsack = system.time(greedy_knapsack(x = knapsack_objects[1:i,], W = 3500))[3])
  )
}

ggplot(run_time2, aes(x=rows)) + 
  geom_line(aes(y = knapsack_dynamic, colour = "red"), size = 1.2) +
  geom_line(aes(y = greedy_knapsack, colour = "green"), size = 1.2) +
  scale_colour_discrete(labels = c("Greedy", "Dynamic Programming")) + 
  theme_classic() + 
  labs(x = "Rows", y = "Time(s)", title = "Greedy vs Dynamic Programming", color = "Algorithm") +
  ggsave("vignettes/2_algorithm_comparison.png")

