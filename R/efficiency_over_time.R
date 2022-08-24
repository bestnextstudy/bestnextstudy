#' Function to get efficiency over time
#'
#' @param data - input data
#' @param variables - list of variables to include in the design matrix (should match with the column names of data)
#' @param labels - list of labels for variables
#' @param corrvar - name of the column corresponding to correlation coefficients
#' @param naval - value of variable indicating the variable cannot be identified in the study
#' @param yrvar -  name of the column corresponding to the year
#' @param yrs - vector of years to create the block of studies
#' @return - List of two data frames (one for true efficiency and another for optimal efficiency)
#' @export
#' @examples
efficiency_over_time <- function(data, variables, labels, corrvar, naval, yrvar, yrs){
  ##INPUTS
  #data = input data
  #variables = list of variables to include in the design matrix (should match with the column names of data)
  #labels = list of labels for variables
  #corrvar = name of the column corresponding to correlation coefficients
  #naval = value of variable indicating the variable cannot be identified in the study
  #yrvar = name of the column corresponding to the year
  #yrs = vector of years to create the block of studies


  ##TRUE EFFICIENCY
  #SETTINGS
  eff_by_yr_true = matrix(0, nrow = length(yrs), ncol = 4)
  colnames(eff_by_yr_true) = c("End Year", "N", "A-Efficiency", "D-Efficiency")
  #EFFICIENCIES FOR EACH BLOCK OF STUDIES
  for(i in 1:length(yrs)){
    eff_by_yr_true[i, 1] = yrs[i]
    Matrix_Input_Yr = filter(data, Year <= yrs[i])
    eff = as.data.frame(efficiency(Matrix_Input_Yr, variables, labels, corrvar))
    eff_by_yr_true[i, 2] = eff$N
    eff_by_yr_true[i, 3] = eff$a_efficiency
    eff_by_yr_true[i, 4] = eff$d_efficiency
  }

  ##OPTIMAL EFFICIENCY (WHEN STARTING FROM 1981-2005 AND ADDING STUDIES TO EACH BLOCK OF EXISTING STUDIES--I.E., 1981-2005, 1981-2008, ...)
  #SETTINGS
  eff_by_yr_blk_opt = matrix(0, length(yrs), ncol = 4)
  colnames(eff_by_yr_blk_opt) = c("End Year", "N", "A-Efficiency", "D-Efficiency")
  #EFFICIENCIES FOR THE FIRST BLOCK OF STUDIES
  eff_by_yr_blk_opt[1, 1] = yrs[1]
  eval(parse(text = paste("eff_by_yr_blk_opt[1, 2] = as.data.frame(efficiency(filter(data, ", yrvar, " <= yrs[1]), variables, labels, corrvar))$N", sep = "")))
  eval(parse(text = paste("eff_by_yr_blk_opt[1, 3] = as.data.frame(efficiency(filter(data, ", yrvar, " <= yrs[1]), variables, labels, corrvar))$a_efficiency", sep = "")))
  eval(parse(text = paste("eff_by_yr_blk_opt[1, 4] = as.data.frame(efficiency(filter(data, ", yrvar, " <= yrs[1]), variables, labels, corrvar))$d_efficiency", sep = "")))
  #ITERATE THROUGH BLOCKS OF YEARS
  for(i in 1:(length(yrs) - 1)){
    j = i + 1
    eval(parse(text = paste("Matrix_Input_Init = filter(data, ", yrvar, " <= yrs[i])", sep = "")))  #initial set of studies
    eval(parse(text = paste("Matrix_Input_Next = filter(data, ", yrvar, " <= yrs[j])", sep = "")))  #set of studies in the next block of years
    Matrix_Input_A = Matrix_Input_D = Matrix_Input_Init #output matrix for each A- and D-efficiency
    nstudies_init = nrow(Matrix_Input_Init)  #number of initial set of studies
    nstudies_add = nrow(Matrix_Input_Next) - nstudies_init #number of studies to add (number of studies actually added in the next time frame)
    #ITERATE THE PROCESS WITH THE NUMBER OF STUDIES TO ADD
    for(k in 1:nstudies_add){
      #IN EACH ITERATION, EXAMINE THE DESIGN THAT MAXIMIZES EFFICIENCY AND ADD IT TO THE SET OF STUDIES
      #A-EFFICIENCY
      eff_all_A = efficiency_by_adding_one_full_fact(Matrix_Input_A, variables, labels, corrvar, naval)
      design_add_A = eff_all_A %>% top_n(n = 1, wt = a_efficiency)
      eval(parse(text = paste("design_add_A$", corrvar, " = 0", sep = ""))) #need to check what this line is for...
      Matrix_Input_A = bind_rows(Matrix_Input_A, design_add_A)
      #D-EFFICIENCY
      eff_all_D = efficiency_by_adding_one_full_fact(Matrix_Input_D, variables, labels, corrvar, naval)
      design_add_D = eff_all_D %>% top_n(n = 1, wt = d_efficiency)
      eval(parse(text = paste("design_add_D$", corrvar, " = 0", sep = ""))) #need to check what this line is for...
      Matrix_Input_D = bind_rows(Matrix_Input_D, design_add_D)
      cat("End year =", yrs[j], ":", k, "out of", nstudies_add, "studies added \n")
    }
    #CALCULATE EFFICIENCIES
    eff_by_yr_blk_opt[j, 1] = yrs[j]
    eff_by_yr_blk_opt[j, 2] = as.data.frame(efficiency(Matrix_Input_A, variables, labels, corrvar))$N
    eff_by_yr_blk_opt[j, 3] = as.data.frame(efficiency(Matrix_Input_A, variables, labels, corrvar))$a_efficiency
    eff_by_yr_blk_opt[j, 4] = as.data.frame(efficiency(Matrix_Input_D, variables, labels, corrvar))$d_efficiency
  }

  output = list(eff_by_yr_true = eff_by_yr_true, eff_by_yr_opt = eff_by_yr_blk_opt)
  return(output)
}
