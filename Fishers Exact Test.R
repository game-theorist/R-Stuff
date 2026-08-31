#Fisher's Exact Test


fisher_tribble <- raw_tribble |> 
  pivot_wider(
    names_from = c(treatment, response),
    names_glue = "{treatment}_{response}",
    values_from = count
  ) |> 
  select(!c(treatment_total, response_total, proportion, expected_count)
  ) |> 
  summarize(
    exposure_success = mean(exposure_success, na.rm = TRUE),
    exposure_failure= mean(exposure_failure , na.rm = TRUE),
    non_exposure_success  = mean(non_exposure_success , na.rm = TRUE),
    non_exposure_failure = mean(non_exposure_failure, na.rm = TRUE),
    n = sum(across(everything(), ~ .x))
  )


tibble(
  extreme_ps = dhyper(x = 0:(fisher_tribble$exposure_success + fisher_tribble$exposure_failure),
       m = (fisher_tribble$exposure_success + fisher_tribble$non_exposure_success),
       n = fisher_tribble$exposure_failure + fisher_tribble$non_exposure_failure,
       k = (fisher_tribble$exposure_success + fisher_tribble$exposure_failure)
       ),
  reference_p = dhyper(x = fisher_tribble$exposure_success,
                       m = (fisher_tribble$exposure_success + fisher_tribble$non_exposure_success),
                       n = fisher_tribble$exposure_failure + fisher_tribble$non_exposure_failure,
                       k = (fisher_tribble$exposure_success + fisher_tribble$exposure_failure)
                       )
  ) |> 
  filter(many_ps <= mean(reference_p)) |> 
  summarize(
    p_value = sum(extreme_ps)
  )


mat <- sample_data |> 
  column_to_rownames("treatment") |> 
  as.matrix()

# 2. Run the test directly
fisher.test(mat)
