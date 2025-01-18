library(tidyverse)
library(ratdat)

### exploration de donnees
?complete_old
summary(complete_old)
head(complete_old)
str(complete_old)


### c'est le temps d'utiliser ggplot !
library(ggplot2)

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length)) + 
  geom_point(alpha = 0.1)### alpha changed the transparency of the points

complete_old <- filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length)) + 
  geom_point(alpha = 0.1, color = "blue")

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) + 
  geom_point(alpha = 0.1)

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, shape = sex)) + 
  geom_point(alpha = 0.1)

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = year)) + 
  geom_point(alpha = 0.1)

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) + 
  geom_point(alpha = 0.1) + 
  scale_color_viridis_d()

ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) + 
  geom_point(alpha = 0.1) + 
  scale_color_viridis_d() +
  scale_x_log10()

ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length, color = plot_type)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))

ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))

ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))

ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))
  
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_violin(fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10))

ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Plot type", y = "Hindfoot length (mm)")

 ### making panels to seggregate the sexes
ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  facet_wrap(vars(sex), nrow = 1) +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Plot type", y = "Hindfoot length (mm)")


### making this plot as an object and saving the object as a file.
plot_final<- ggplot(complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  facet_wrap(vars(sex), nrow = 1) +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Plot type", y = "Hindfoot length (mm)")

plot_final

ggsave(filename = "figures/plot_final.png", 
       plot = plot_final,
       height = 6,
       width = 8)

### importing data with Tidyverse (use Tab key for file name)

surveys <- read_csv("data/raw/surveys_complete_77_89.csv")
View(surveys)

str(surveys)

### select : better practice to go by col names than col numbers
select(surveys, plot_id, species_id)

select(surveys, c(3, 4)) 

select(surveys, -plot_id)

select(surveys, where(is.numeric))

select(surveys, where(anyNA))

### filter

filter(surveys, year == 1988)

filter(surveys, species_id %in% c("RM", "DO"))

filter(surveys, year == 1988 & species_id %in% c("RM", "DO"))

### filter challenge

select(surveys, year, month, species_id, plot_id) +
  filter(surveys, year == 1980, 1981, 1982, 1983, 1984, 1985) ### doesn't work

select(surveys, year, month, species_id, plot_id) +
  filter(surveys, year == 1980:1985) ### doesn't work either

select(surveys, year, month, species_id, plot_id) &
  filter(surveys, year == 1980:1985) ### nope

### 3 ways to select columns AND filter lines AT THE SAME TIME
## option 1: create an object that first filters
surveys_80_85 <- filter(surveys, year >= 1980 & year <= 1985)
surveys_80_85

select(surveys_80_85, year, month, species_id, plot_id)

## option 2: nest the filter in the selection
select(filter(surveys, year >= 1980 & year <= 1985), year, month, species_id, plot_id)

## option 3: using a pipeline (ctrl+shift+m)

surveys %>% 
  filter(year == 1980:1985) %>% 
  select(year, month, species_id, plot_id)

### challenge4

surveys %>% 
  filter(year == 1988) %>% 
  select(record_id, month, species_id)

### mutate ! ("new name" = "old name")

surveys %>% 
  mutate(weight_kg = weight / 1000) %>% 
  relocate(weight_kg, .after = record_id)
  
surveys %>% 
    filter(!is.na(weight)) %>% 
    mutate(weight_kg = weight / 1000,
           weight_lbs = weight_kg * 2.2) %>% 
  relocate(weight_kg, .after = record_id) %>% 
  relocate(weight_lbs, .after = weight_kg)

surveys %>% 
  mutate(date = paste(year, month, day, sep = "-")) %>% 
  relocate(date, .after = year)

## something new

library(lubridate)

surveys %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  relocate(date, .after = year)

### group_by and summarize !
surveys %>% 
  group_by(sex) %>% 
  summarize(mean.weight = mean(weight))
## result is one data frame with 3 lines for each category of sex

surveys %>% 
  group_by(sex) %>% 
  summarize(mean.weight = mean(weight, na.rm = TRUE),
            count = n())

## challenge5
surveys %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  group_by(sex, date) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = date, y = count, color = sex)) +
  geom_line()

surveys %>% 
  filter(!is.na(sex)) %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  group_by(sex, date) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = date, y = count, color = sex)) +
  geom_line()

### WORKSHOP PART 2 : GIT


