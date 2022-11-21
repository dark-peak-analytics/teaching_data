#' Cost-effectiveness plane plot
#'
#' Simple function for use as an example here.
#'
#' @param results a dataframe with a row for each PSA run with the following columns:
#'         Cost_Trt, Cost_NoTrt, QALY_Trt, QALY_NoTrt
#'
#' @return a ggplot2 object containing a cost-effectiveness plane plot
#' @examples plotCEplane(results = f_wrapper(c_Trt = 200, n_age_init = 25, n_sim = 1000, d_r = 0.035, n_age_max = 60))
plotCEplane <- function(results = df_model_res){

  # calculate incremental costs and qalys from results data-frame in function input.
  df_plot <- data.frame(inc_C = results$Cost_Trt - results$Cost_NoTrt,
                        inc_Q = results$QALY_Trt - results$QALY_NoTrt)

  # now use this plotting data-frame to create a very simple ggplot.
  plot <- ggplot(data = df_plot,
                 aes(x = inc_Q,  # x axis incremental QALYS
                     y = inc_C)  # y axis incremental Costs
  ) +
    theme_bw()+

    geom_point(alpha = 0.5, col = "black", fill = "grey") +

    # titles
    labs(
      title = "",
      subtitle = "",
      x = "Incremental QALYs",
      y = "Incremental Costs"
    ) +

    # set xlimits and ylimits for plot.
    xlim(c(
      min(df_plot$inc_Q, df_plot$inc_Q * -1),
      max(df_plot$inc_Q, df_plot$inc_Q * -1)
    )) +

    ylim(c(
      min(df_plot$inc_C, df_plot$inc_C * -1),
      max(df_plot$inc_C, df_plot$inc_C * -1)
    ))+

    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 0)

  return(plot) # output the plot from the function.

}
