#Sample data for single group

response <- sample_tribble$___

normality_data <- sample_tribble |> 
  mutate(z_score = scale(response))
  
ggplot(normality_data, aes(sample = z_score)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  coord_flip() +
  theme_bw()


#Scaling sample data for multiple groups

explanatory <- sym("")

explanatory_level_one <- ""

explanatory_level_two <- ""

response <- sample_tribble$


normality_data <- sample_tribble |> 
  group_by(!!explanatory) |> 
  mutate(z_score = as.vector(scale())) |> 
  ungroup()

ggplot(normality_data, aes(sample = z_score)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  coord_flip() +
  theme_bw() +
  facet_wrap(explanatory)

shapiro.test(normality_data$sample_data)

normality_data
