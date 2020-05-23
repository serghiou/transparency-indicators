eval_metrics <- function(true = true_rates,
                         false = false_rates,
                         pred_label = pred_label_vars,
                         with_prauc = F,
                         type = "any") {

  rates <- bind_rows(true[[type]], false[[type]])
  preds <- pred_label[1]
  label <- pred_label[2]
  out <- list()

  out[["TP"]] <-
    rates %>%
    filter(!!! preds & !!! label) %>%
    pull(est_freq) %>%
    sum(na.rm = T)

  out[["FP"]] <-
    rates %>%
    filter(!!! preds & !(!!! label)) %>%
    pull(est_freq) %>%
    sum(na.rm = T)

  out[["TN"]] <-
    rates %>%
    filter(!(!!! preds) & !(!!! label)) %>%
    pull(est_freq) %>%
    sum(na.rm = T)


  out[["FN"]] <-
    rates %>%
    filter(!(!!! preds) & !!! label) %>%
    pull(est_freq) %>%
    sum(na.rm = T)

  out[["Sensitivity"]] <- with(out, TP / (TP + FN) * 100)
  out[["Specificity"]] <- with(out, TN / (TN + FP) * 100)
  out[["PPV"]] <- with(out, TP / (TP + FP) * 100)
  out[["NPV"]] <- with(out, TN / (TN + FN) * 100)
  out[["Accuracy"]] <- with(out, (TP + TN) / (TP + FP + TN + FN) * 100)
  out[["AUROC"]] <- with(out, mean(c(Sensitivity, Specificity)))
  out[["P_true"]] <- with(out, (TP + FN) / (TP + FP + TN + FN) * 100)
  out[["P_pred"]] <- with(out, (TP + FP) / (TP + FP + TN + FN) * 100)
  out[["P_error"]] <- with(out, (FN - FP) / (TP + FP + TN + FN) * 100)

  # This is meaningless in our case b/c we do not vary the threshold
  if (with_prauc) {

    probs  <- with(out, rep(c(1, 1, 0, 0), c(TP, FP, TN, FN)))
    labels <- with(out, rep(c(1, 0, 0, 1), c(TP, FP, TN, FN)))
    out[["PRAUC"]] <-
      tibble(probs, labels) %>%
      mutate(labels = factor(labels)) %>%
      mutate(probs = probs) %>%
      yardstick::pr_auc(labels, probs) %>%
      pull(.estimate)

  }

  tibble::as_tibble(out)
}


eval_boot <- function(t_tr = df_true,
                      t_fs = df_false,
                      filters = filtering_vars[0],
                      grouping_tr = grouping_vars_true,
                      grouping_fs = grouping_vars_false,
                      pred_label = pred_label_vars,
                      is_old = FALSE,
                      negate_filters = FALSE,
                      with_prauc = FALSE,
                      type = "any") {

  true_rates <- list()
  false_rates <- list()

  # e.g. turn isResearch into !isResearch
  if (negate_filters) {
    filters = expr(!(!!! filters))
  }

  s_tr <- t_tr %>% filter(is_test)
  s_fs <- t_fs %>% filter(is_test)

  a <- t_tr %>% select(!!! grouping_tr, !!! grouping_fs) %>% mutate(origin = T)
  b <- t_fs %>% select(!!! grouping_tr, !!! grouping_fs) %>% mutate(origin = F)
  obs <- bind_rows(a, b)

  n <- nrow(obs)
  n_true <- nrow(s_tr)
  n_false <- nrow(s_fs)

  i <- sample(1:n, n, replace = T)
  i_true <- sample(1:n_true, n_true, replace = T)
  i_false <- sample(1:n_false, n_false, replace = T)

  t_tr_i <- obs[i, ] %>% filter(origin)
  t_fs_i <- obs[i, ] %>% filter(!origin)

  s_tr_i <- s_tr[i_true, ]
  s_fs_i <- s_fs[i_false, ]

  true_total_counts  <- t_tr_i %>% count(!!! grouping_tr, name = "n_t")
  true_sample_counts <- s_tr_i %>% count(!!! grouping_tr, name = "n_s")

  false_total_counts  <- t_fs_i %>% count(!!! grouping_fs, name = "n_t")
  false_sample_counts <- s_fs_i %>% count(!!! grouping_fs, name = "n_s")

  true_rates[[type]] <-
    s_tr_i %>%
    filter(!!! filters) %>%
    count(!!! grouping_tr, !!! pred_label) %>%
    merge(true_sample_counts) %>%
    merge(true_total_counts) %>%
    mutate(est_freq = n / n_s * n_t) %>%
    as_tibble()

  false_rates[[type]] <-
    s_fs_i %>%
    filter(!!! filters) %>%
    count(!!! grouping_fs, !!! pred_label) %>%
    merge(false_sample_counts) %>%
    merge(false_total_counts) %>%
    mutate(est_freq = n / n_s * n_t) %>%
    as_tibble()

  if (is_old) {

    true_rates[[type]]  %<>% mutate_at(vars(!!! pred_label[1]), `=`, T)
    false_rates[[type]] %<>% mutate_at(vars(!!! pred_label[1]), `=`, F)

  }

  eval_metrics(true_rates, false_rates, pred_label, with_prauc, type)
}


eval_summarize <- function(eval_boot_df) {

  eval_boot_df %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    summarize_at(vars(value),
                 list(median = median,
                      lo = ~quantile(., probs = 0.025),
                      hi = ~quantile(., probs = 0.975)))

}
