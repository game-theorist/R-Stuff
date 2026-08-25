chisq_table <- chisq_sample |> 
  summarize(
    chisq_statistic = sum(chisq_fraction),
    df = (levels_treatment - 1) * (levels_response - 1),
    p_value = pchisq(chisq_statistic, df, lower.tail = FALSE)
  )

#Plots

bar_plot <- chisq_sample |> 
  ggplot(aes(x = treatment, y = count, fill = response)) +
  geom_col()

stacked_bar_plot <- chisq_sample |> 
  ggplot(aes(x = treatment, y = count, fill = response)) +
  geom_col(position = "fill")

chisq_distribution <- chisq_table |> 
  ggplot() +
  stat_function(fun = dchisq, args = list(df = eval(parse(text = "chisq_table$df"))),
                xlim = c(eval(parse(text = "chisq_table$chisq_statistic")), 30),
                geom = "area", 
                fill = "tomato", 
                alpha = 0.5
                ) +
  stat_function(fun = dchisq, args = list(df = eval(parse(text = "chisq_table$df"))), linewidth = 1, color = "blue") +
  geom_vline(aes(xintercept = chisq_statistic), color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = c(0, 30))



(bar_plot + stacked_bar_plot + chisq_distribution) + plot_layout(guides = "collect") + theme_bw()

#Results

chisq_sample

chisq_table
