#Dotplot

sample_tribble |> 
  ggplot(aes(x = score, fill = Duration, g_id = Duration)) +
  geom_dotplot(binwidth = 1, stackgroups = TRUE, binpositions = "all", dotsize = 0.5) +
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0), breaks = 1:10) +
  scale_y_continuous(NULL, breaks = NULL)