#' Function to plot efficiency over time
#'
#' @param data - Output from 'efficiency_over_time' function
#' @return - Plot of True and Optimal A and D efficiency
#' @export
#'
#' @examples
efficiency_over_time_plot<- function(data){
  eff_over_time = data
  eff_by_yr_true = as.data.frame(eff_over_time$eff_by_yr_true) #true efficiency
  eff_by_yr_opt = as.data.frame(eff_over_time$eff_by_yr_opt)   #optimal efficiency
  ##PLOT
  #STACK DATA
  eff_ov_time_true_a = cbind(eff_by_yr_true[c("End Year", "A-Efficiency")], "A-Efficiency: True")
  eff_ov_time_true_d = cbind(eff_by_yr_true[c("End Year", "D-Efficiency")], "D-Efficiency: True")
  eff_ov_time_opt_a = cbind(eff_by_yr_opt[c("End Year", "A-Efficiency")], "A-Efficiency: Optimal")
  eff_ov_time_opt_d = cbind(eff_by_yr_opt[c("End Year", "D-Efficiency")], "D-Efficiency: Optimal")
  colnames(eff_ov_time_true_a) = colnames(eff_ov_time_true_d) = colnames(eff_ov_time_opt_a) = colnames(eff_ov_time_opt_d) =
    c("End Year", "Efficiency", "Label")
  eff_ov_time = rbind(eff_ov_time_true_a, eff_ov_time_opt_a[2:nrow(eff_ov_time_opt_a), ], eff_ov_time_true_d, eff_ov_time_opt_d[2:nrow(eff_ov_time_opt_d), ])
  #PLOT
  ggplot(eff_ov_time, aes(x = `End Year`, y = Efficiency,
                          linetype = factor(Label), shape = factor(Label))) +
    geom_line() +
    geom_point() +
    geom_text(aes(label = round(Efficiency,2)), size = 3, vjust = -1) +
    scale_linetype_manual("",
                          values = c("solid", "longdash", "solid", "longdash"),
                          breaks = c("A-Efficiency: True", "A-Efficiency: Optimal", "D-Efficiency: True", "D-Efficiency: Optimal")) +
    scale_shape_manual("",
                       values = c(15, 15, 17, 17),
                       breaks = c("A-Efficiency: True", "A-Efficiency: Optimal", "D-Efficiency: True", "D-Efficiency: Optimal")) +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = yrs)

}
