fitMods <- function(df_os,
                    df_pfs,
                    dist) {
  mod_os <- flexsurv::flexsurvreg(
    formula = survival::Surv(time = df_os$eventtime, event = df_os$status)  ~ trt,
    data = df_os,
    dist = dist,
  )

  mod_pfs <- flexsurv::flexsurvreg(
    formula = survival::Surv(time = df_pfs$eventtime, event = df_pfs$status)  ~ trt,
    data = df_pfs,
    dist = dist
  )

  # can check the AIC of each very easily
  mod_pfs$AIC
  mod_os$AIC

  # we can also store these together:
  surv_mods <- list(
    "os" = mod_os,
    "pfs" = mod_pfs,
    "AIC" = c("os" = mod_os$AIC, "pfs" = mod_pfs$AIC),
    "dist" = dist
  )
  return(surv_mods)
}



predictSurv <- function(mod_os,
                        mod_pfs,
                        v_times,
                        trts = c("SOC", "Supimab")) {
  df_pred_os <- summary(
    object = mod_os,
    newdata = data.frame(trt = trts),
    type = "survival",
    t = v_times,
    tidy = TRUE,
    ci = F
  ) %>%
    mutate(endp = "os")

  # and progression free survival
  df_pred_pfs <- summary(
    object = mod_pfs,
    newdata = data.frame(trt = trts),
    type = "survival",
    t = v_times,
    tidy = TRUE,
    ci = F
  ) %>%
    mutate(endp = "pfs")

  # bind the dataframes together.
  df_pred <- rbind(df_pred_os,
                   df_pred_pfs)

  return(df_pred)

}

calcTR <- function(df_pred){

  df_pred %>%
    tidyr::pivot_wider(
      id_cols = c("time", "trt"),
      names_from = "endp",
      values_from = "est"
    ) %>%
    mutate(pps = os - pfs,
           D = 1 - os) %>%
    select(time,
           trt,
           pfs,
           pps,
           D)

}

MM_match <- function(mat, vec) {
  mat[, c("pfs", "pps", "D")] %*% vec[c("pfs", "pps", "D")]

}

subsetTrace <- function(df_TR,
                        trt){
  as.matrix(df_TR[df_TR$trt == trt, c("pfs", "pps", "D")])
}

calcPartitionResults <- function(df_TR,
                                 v_C_supi,
                                 v_C_soc,
                                 v_Q,
                                 cycles_per_year) {
  # subset the trace for each treatment...
  m_TR_soc <- subsetTrace(df_TR = df_TR,
                          trt = "SOC")
  m_TR_supi <- subsetTrace(df_TR = df_TR,
                           trt = "Supimab")

  # matrix multiplication of trace by cost per cycle - gives total cost by cycle
  v_costs_soc  <- MM_match(mat = m_TR_soc, vec = v_C_soc/cycles_per_year)
  v_costs_supi <- MM_match(mat = m_TR_supi, vec = v_C_supi/cycles_per_year)

  # matrix multiplication of trace by utility by cycle - gives total utility by cycle
  v_qalys_soc  <- MM_match(mat = m_TR_soc, vec = v_Q/cycles_per_year)
  v_qalys_supi <- MM_match(mat = m_TR_supi, vec = v_Q/cycles_per_year)

  # combine the columns together into a single matrix.
  m_res <- cbind(
    "c_soc" = v_costs_soc,
    "c_supi" = v_costs_supi,
    "q_soc" = v_qalys_soc,
    "q_supi" = v_qalys_supi
  )

  # the column names of the matrix need to be informative...
  colnames(m_res) <- c("c_soc", "c_supi", "q_soc", "q_supi")

  return(m_res)
}

discMat <- function(dr,
                    v_times,
                    m_res){
  # construct a vector of discount weights - to be applied to costs & qalys
  v_dw <- 1 /(1 + dr) ^ v_times
  # apply discount weight vector to all columns of matrix simultaneously
  # this gives total discounted costs and qalys over whole period.
  m_disc_res <- v_dw %*% m_res
  return(m_disc_res)
}

calcICER <- function(cost_base,
                     cost_int,
                     effect_base,
                     effect_int){
  # calculate incremental costs and qalys
  inc_cost <- (cost_int - cost_base)
  inc_qaly <- (effect_int - effect_base)
  return(as.numeric(inc_cost / inc_qaly))
}


runModel <- function(df_os,
                     df_pfs,
                     dist,
                     disc_rate,
                     horizon,
                     cycles_per_year,
                     v_C_supi,
                     v_C_soc,
                     v_Q,
                     output
) {

  v_times = seq(from = 0, to = horizon, by = 1/cycles_per_year)

  surv_mods <- fitMods(df_os = df_os,
                       df_pfs = df_pfs,
                       dist = dist)

  df_pred <- predictSurv(
    mod_os = surv_mods$os,
    mod_pfs = surv_mods$pfs,
    v_times = v_times,
    trts = c("SOC", "Supimab")
  )

  df_TR <- calcTR(df_pred)

  # calculate partitioned survival over time...
  m_res <- calcPartitionResults(df_TR = df_TR,
                                v_C_supi = v_C_supi,
                                v_C_soc = v_C_soc,
                                v_Q = v_Q,
                                cycles_per_year = cycles_per_year)

  m_disc_res <- discMat(
    dr = disc_rate,
    v_times = v_times,
    m_res = m_res
  )

  ICER <- calcICER(
    cost_base = m_disc_res[, "c_soc"],
    cost_int = m_disc_res[, "c_supi"],
    effect_base = m_disc_res[, "q_soc"],
    effect_int = m_disc_res[, "q_supi"]
  )

  switch(output,
         "ICER" = ICER,
         "models" = surv_mods)

}
