#' Function to calculate efficiency after adding a new study
#'
#' @param data - input data
#' @param variables - list of variables to include in the design matrix (should match with the column names of data)
#' @param labels - list of labels for variables
#' @param corrvar - name of the column corresponding to correlation coefficients
#' @param naval - value of variable indicating the variable cannot be identified in the study
#' @return -  a_efficiency, d_efficiency
#' @export
#'
#' @examples
efficiency_by_adding_one_full_fact <- function(data, variables, labels, corrvar, naval){
  ##INPUTS
  #data = input data
  #variables = list of variables to include in the design matrix (should match with the column names of data)
  #labels = list of labels for variables
  #corrvar = name of the column corresponding to correlation coefficients
  #naval = value of variable indicating the variable cannot be identified in the study

  ##CREATE DESIGN MATRIX
  Z <- lapply(data[variables], factor)  #dummy code variables in the data
  Z <- data.frame(Z)  #convert Z to a matrix with factor variables

  ##FULL-FACTORIAL DESIGN
  #GENERATE FULL FACTORIAL (I.E., ALL POSSIBLE COMBINATIONS OF Z)
  nfactor = ncol(Z)
  eval(parse(text = paste("hyper_grid <- expand.grid(", paste(paste("levels(Z[, ", c(1:nfactor), "])", sep = ""), collapse = ", "), ")", sep = "")))
  names(hyper_grid) <- variables
  #ADD COLUMN OF CORRELATIONS SO THAT THE FUNCTION "EFFICIENCY" CAN IDENTIFY SINGULARITY THROUGH LM
  eval(parse(text = paste("hyper_grid$", corrvar, "<- 0", sep = "")))
  #REMOVE LEVELS THAT DO NOT MAKE SENSE--I.E., THERE WILL BE NO STUDY THAT A FACTOR == naval
  hyper_grid <- hyper_grid %>% filter_all(all_vars(. != naval))

  ##ADD ONE STUDY AT A TIME AND CALCULATE EFFICIENCY
  study_eff <- data.frame()
  for (j in 1:nrow(hyper_grid)){
    data_new <- dplyr::bind_rows(data, hyper_grid[j, ])
    add_eff <- efficiency(data = data_new, variables = variables, labels = labels, corrvar = corrvar)
    study_eff <- rbind(study_eff, cbind(j, add_eff))
  }

  output <- cbind(hyper_grid[variables], study_eff[c("a_efficiency", "d_efficiency")])
  return(output)
}
