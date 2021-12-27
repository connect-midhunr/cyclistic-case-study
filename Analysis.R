install.packages(c("ggplot2", "ggmap", "dplyr", "readr", 
                   "RColorBrewer", "stringr", "ggpubr"))

library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(stringr)
library(ggmap)
library(ggpubr)

cyclistic_data <- read_csv("final_data.csv")

cyc_count <- table(as.factor(cyclistic_data$type_of_user))
percent_count <- round(100 * cyc_count / nrow(cyclistic_data), 2)
lbl_count <- paste(str_to_title(names(cyc_count)), cyc_count, 
             paste(percent_count, "%", sep = ""), 
             sep = "\n")
pie(cyc_count, 
    main = "Number of Rides by User Type",
    init.angle = 90,
    labels = lbl_count, 
    border = "white", 
    col = brewer.pal(3, "Set2"))

cyc_dur <- tapply(cyclistic_data$trip_duration_in_mins, 
                  cyclistic_data$type_of_user, FUN = sum)
cyc_dur
percent_dur <- round(100 * cyc_dur / sum(cyclistic_data$trip_duration_in_mins), 2)
lbl_dur <- paste(str_to_title(names(cyc_dur)), cyc_dur,
                 paste(percent_dur, "%", sep = ""), 
                 sep = "\n")
pie(cyc_dur,
    main = "Duration of Rides by User Type",
    init.angle = 60,
    labels = lbl_dur,
    border = "white",
    col = brewer.pal(3, "Set2"))

cyc_mean <- cyclistic_data %>% 
  group_by(type_of_user) %>% 
  summarise_at(vars(trip_duration_in_mins), list(mean_duration = mean))
cyc_mean
ggplot(cyc_mean, aes(x = str_to_title(type_of_user), y = mean_duration, fill = type_of_user)) +
  geom_col(width = 0.5) +
  ggtitle("Average Duration of Ride by User Type\n") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Type of user", y = "Duration (min)") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey90"))

hour_count <- cyclistic_data %>% 
  group_by(type_of_user, hour_of_day) %>% 
  summarize(num_of_rides = n())
ggplot(hour_count, aes(x = hour_of_day, y = num_of_rides, color = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", x = 8, y = 1.72e+05) +
  geom_label(label = "Casual", x = 11, y = 0.9e+05, color = "Red") +
  geom_point(size = 2) +
  ggtitle("Total Number of Rides for each Hour of the Day by User Type",
          subtitle = "Hour of the Day vs No. of Rides\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Hour of the Day", y = "No. of Rides") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey90"))

hour_dur <- cyclistic_data %>% 
  group_by(type_of_user, hour_of_day) %>% 
  summarize(total_dur = sum(trip_duration_in_mins))
ggplot(hour_dur, 
       aes(x = hour_of_day, 
           y = total_dur/60,
           color = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", x = 11, y = 22500) +
  geom_label(label = "Casual", x = 11, y = 51500, 
             color = "Red") +
  geom_point(size = 2) +
  ggtitle("Total Time Duration of Rides for each Hour of the Day by User Type",
          subtitle = "Hour of the Day vs Time Duration (hr)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Hour of the Day", 
       y = "Time Duration (hr)") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))

hour_avg <- cyclistic_data %>% 
  group_by(type_of_user, hour_of_day) %>% 
  summarize(avg_dur = mean(trip_duration_in_mins))
ggplot(hour_avg, aes(x = hour_of_day, y = avg_dur,
           color = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", x = 11, y = 15) +
  geom_label(label = "Casual", x = 11, y = 31.5, color = "Red") +
  geom_point(size = 2) +
  ggtitle("Average Time Duration of Rides for each Hour of the Day by User Type",
          subtitle = "Hour of the Day vs Average Time Duration (min)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Hour of the Day", y = "Average Time Duration (min)") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white", size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))

day_count <- cyclistic_data %>% 
  group_by(day_of_week, type_of_user) %>% 
  summarise(num_of_rides = n()) 
day_count$day_of_week <- factor(day_count$day_of_week, ordered = TRUE,
                                levels = c("Monday", "Tuesday", 
                                           "Wednesday", "Thursday",
                                           "Friday", "Saturday", 
                                           "Sunday"))
day_count <- day_count[order(day_count$day_of_week), ]
ggplot(day_count, 
       aes(x = day_of_week, 
           y = num_of_rides, 
           color = type_of_user, 
           group = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", 
             x = 4.7, y = 4.4e+05) +
  geom_label(label = "Casual", 
             x = 4.7, y = 3.65e+05, 
             color = "Red") +
  geom_point(size = 2) +
  ggtitle("Total Number of Rides for each Day of the Week by User Type",
          subtitle = "Day of the Week vs No. of Rides\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Day of the Week", y = "No. of Rides") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))

day_dur <- cyclistic_data %>% 
  group_by(day_of_week, type_of_user) %>% 
  summarise(total_dur = sum(trip_duration_in_mins)) 
day_dur$day_of_week <- factor(day_dur$day_of_week, ordered = TRUE,
                              levels = c("Monday", "Tuesday",
                                         "Wednesday", "Thursday", 
                                         "Friday", "Saturday", 
                                         "Sunday"))
day_dur <- day_dur[order(day_dur$day_of_week), ]
ggplot(day_dur, 
       aes(x = day_of_week, 
           y = total_dur/60, 
           color = type_of_user, 
           group = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", 
             x = 5.7, y = 115000) +
  geom_label(label = "Casual", 
             x = 5.3, y = 165000, 
             color = "Red") +
  geom_point(size = 2) +
  ggtitle("Total Time Duration of Rides for each Day of the Week by User Type",
          subtitle = "Day of the Week vs Time Duration (hr)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Day of the Week", y = "Time Duration (hr)") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))

day_avg <- cyclistic_data %>% 
  group_by(day_of_week, type_of_user) %>% 
  summarise(avg_dur = mean(trip_duration_in_mins)) 
day_avg$day_of_week <- factor(day_avg$day_of_week, ordered = TRUE,
                              levels = c("Monday", "Tuesday",
                                         "Wednesday", "Thursday", 
                                         "Friday", "Saturday", 
                                         "Sunday"))
day_avg <- day_avg[order(day_avg$day_of_week), ]
ggplot(day_avg, aes(x = day_of_week, 
                    y = avg_dur,
                    color = type_of_user, 
                    group = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", x = 5.7, y = 16) +
  geom_label(label = "Casual", x = 5.3, y = 28.5, color = "Red") +
  geom_point(size = 2) +
  ggtitle("Average Time Duration of Rides for each Day of the Week by User Type",
          subtitle = "Day of the Week vs Average Time Duration (min)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Day of the Week", y = "Average Time Duration (min)") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white", 
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))

month_count <- cyclistic_data %>% 
  group_by(type_of_user, month) %>% 
  summarise(num_of_rides = n())
month_count$month <- factor(month_count$month, 
                            ordered = TRUE,
                            levels = c("November", "December", 
                                       "January", "February", 
                                       "March", "April", "May", 
                                       "June", "July", "August",
                                       "September", "October"))
month_count <- month_count[order(month_count$month), ]
ggplot(month_count, 
       aes(x = month, 
           y = num_of_rides, 
           color = type_of_user, 
           group = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", 
             x = 5.5, y = 2.2e+05) +
  geom_label(label = "Casual", 
             x = 6.5, y = 1.2e+05, 
             color = "Red") +
  geom_point(size = 2) +
  ggtitle("Total Number of Rides for each Month of the Year by User Type",
          subtitle = "Month of the Year vs No. of Rides\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Month of the Year", y = "No. of Rides") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))

month_dur <- cyclistic_data %>% 
  group_by(type_of_user, month) %>% 
  summarise(total_dur = sum(trip_duration_in_mins))
month_dur$month <- factor(month_dur$month, 
                            ordered = TRUE,
                            levels = c("November", "December", 
                                       "January", "February", 
                                       "March", "April", "May", 
                                       "June", "July", "August",
                                       "September", "October"))
month_dur <- month_dur[order(month_dur$month), ]
ggplot(month_dur, 
       aes(x = month, 
           y = total_dur/60, 
           color = type_of_user, 
           group = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", 
             x = 6.5, y = 37500) +
  geom_label(label = "Casual", 
             x = 4.5, y = 60000, 
             color = "Red") +
  geom_point(size = 2) +
  ggtitle("Total Time Duration of Rides for each Month of the Year by User Type",
          subtitle = "Month of the Year vs Time Duration (hr)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Month of the Year", y = "Time Duration (hr)") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))

month_avg <- cyclistic_data %>% 
  group_by(type_of_user, month) %>% 
  summarise(avg_dur = mean(trip_duration_in_mins))
month_avg$month <- factor(month_avg$month, 
                          ordered = TRUE,
                          levels = c("November", "December", 
                                     "January", "February", 
                                     "March", "April", "May", 
                                     "June", "July", "August",
                                     "September", "October"))
month_avg <- month_avg[order(month_avg$month), ]
ggplot(month_avg, aes(x = month,
                      y = avg_dur,
                      color = type_of_user, 
                      group = type_of_user)) +
  geom_line(size = 1.2) +
  geom_label(label = "Member", 
             x = 4.5, y = 19) +
  geom_label(label = "Casual", 
             x = 3.5, y = 24, 
             color = "Red") +
  geom_point(size = 2) +
  ggtitle("Average Time Duration of Rides for each Month of the Year by User Type",
          subtitle = "Month of the Year vs Average Time Duration (min)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "Month of the Year", y = "Average Time Duration (min)") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white", 
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))


ride_map <- get_stamenmap(
  bbox = c(
    left = -87.78,
    right = -87.52,
    top = 42.07,
    bottom = 41.65
  ),
  maptype = "terrain"
)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sf <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 20000))

start_coord_mem <- cyclistic_data %>% 
  filter(type_of_user == "member") %>% 
  select(start_lat, start_lng) %>% 
  na.omit() %>% 
  group_by(start_lat, start_lng) %>% 
  summarize(frequency = n())
sm <- ggmap(ride_map) + 
  geom_point(data = start_coord_mem, aes(x = start_lng,
                                         y = start_lat,
                                         size = frequency)) +
  ggtitle("Ride Starting Station of\nAnnual Members") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "", colour = "Frequency\n") + 
  sf

start_coord_cas <- cyclistic_data %>% 
  filter(type_of_user == "casual") %>% 
  select(start_lat, start_lng) %>% 
  na.omit() %>% 
  group_by(start_lat, start_lng) %>% 
  summarize(frequency = n())
sc <- ggmap(ride_map) + 
  geom_point(data = start_coord_cas, aes(x = start_lng,
                                         y = start_lat,
                                         color = frequency)) +
  ggtitle("Ride Starting Station of\nCasual Riders") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "", colour = "Frequency\n") + 
  sf

end_coord_mem <- cyclistic_data %>% 
  filter(type_of_user == "member") %>% 
  select(end_lat, end_lng) %>% 
  na.omit() %>% 
  group_by(end_lat, end_lng) %>% 
  summarize(frequency = n())
em <- ggmap(ride_map) + 
  geom_point(data = end_coord_mem, aes(x = end_lng,
                                         y = end_lat,
                                         color = frequency)) +
  ggtitle("Ride Ending Station of\nAnnual Members") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "", colour = "Frequency\n") + 
  sf

end_coord_cas <- cyclistic_data %>% 
  filter(type_of_user == "casual") %>% 
  select(end_lat, end_lng) %>% 
  na.omit() %>% 
  group_by(end_lat, end_lng) %>% 
  summarize(frequency = n())
ec <- ggmap(ride_map) + 
  geom_point(data = end_coord_mem, aes(x = end_lng,
                                       y = end_lat,
                                       color = frequency)) +
  ggtitle("Ride Ending Station of\nCasual Riders") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "", colour = "Frequency\n") + 
  sf

ggarrange(sm, sc)
ggarrange(em, ec)

bike_pop <- cyclistic_data %>% 
  filter(type_of_user == "casual") %>% 
  group_by(bike_type, month) %>% 
  summarise(popularity = n() * sum(trip_duration_in_mins))
bike_pop$month <- factor(bike_pop$month, 
                            ordered = TRUE,
                            levels = c("November", "December", 
                                       "January", "February", 
                                       "March", "April", "May", 
                                       "June", "July", "August",
                                       "September", "October"))
bike_pop <- bike_pop[order(bike_pop$month), ]
ggplot(bike_pop, 
       aes(x = month, 
           y = popularity, 
           color = bike_type, 
           group = bike_type)) +
  geom_line(size = 1.2) +
  geom_label(label = "Electric Bike", 
             x = 7.25, y = 2.2e+11) +
  geom_label(label = "Classic Bike", 
             x = 7.73, y = 4.5e+11, 
             color = "Red") +
  geom_label(label = "Docked Bike", 
             x = 1.8, y = 1.9e+11, 
             color = "SpringGreen4") +
  geom_point(size = 2) +
  ggtitle("Popularity of each type of Bike by Casual Riders for each Month of the Year",
          subtitle = "Month of the Year vs Popularity\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "\nMonth of the Year", y = "Popularity\n") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, 
                                    linetype = 'solid',
                                    colour = "grey90"), 
    panel.grid.minor = element_line(size = 0.25, 
                                    linetype = 'solid',
                                    colour = "grey90"))
