#' Function to get the "Best Next Studies"
#'
#' @param data - input data
#' @param variables - list of variables to include in the design matrix (should match with the column names of data)
#' @param labels - list of labels for variables
#' @param corrvar - name of the column corresponding to correlation coefficients
#' @param naval - value of variable indicating the variable cannot be identified in the study
#' @param nstudies_add - number of next studies
#' @return - List of two data frames (one based on A-efficiency and another based on D-efficiency)
#' @export
#'
#' @examples
efficiency_next_best_studies <- function(data, variables, labels, corrvar, naval, nstudies_add){
  ##INPUTS
  #data = input data
  #variables = list of variables to include in the design matrix (should match with the column names of data)
  #labels = list of labels for variables
  #corrvar = name of the column corresponding to correlation coefficients
  #naval = value of variable indicating the variable cannot be identified in the study

  ##SETTINGS
  next_best_studies_A = next_best_studies_D = data.frame()
  Matrix_Input_A = Matrix_Input_D = data

  ##ITERATE THE PROCESS WITH THE NUMBER OF STUDIES TO ADD
  for(i in 1:nstudies_add){ #loop through the number of studies to add
    #A-EFFICIENCY
    eff_all_A = efficiency_by_adding_one_full_fact(Matrix_Input_A, variables, labels, corrvar, naval)
    design_add_A = eff_all_A %>% top_n(n = 1, wt = a_efficiency)
    next_best_studies_A = bind_rows(next_best_studies_A, design_add_A)
    eval(parse(text = paste("design_add_A$", corrvar, " = 0", sep = ""))) #need to check what this line is for...
    Matrix_Input_A = bind_rows(Matrix_Input_A, design_add_A)
    #D-EFFICIENCY
    eff_all_D = efficiency_by_adding_one_full_fact(Matrix_Input_D, variables, labels, corrvar, naval)
    design_add_D = eff_all_D %>% top_n(n = 1, wt = d_efficiency)
    next_best_studies_D = bind_rows(next_best_studies_D, design_add_D)
    eval(parse(text = paste("design_add_D$", corrvar, " = 0", sep = ""))) #need to check what this line is for...
    Matrix_Input_D = bind_rows(Matrix_Input_D, design_add_D)
    cat(i, "out of", nstudies_add, "studies added \n")
  }
  next_best_studies_A = next_best_studies_A[c(variables, "a_efficiency")]
  next_best_studies_D = next_best_studies_D[c(variables, "d_efficiency")]
  colnames(next_best_studies_A) = c(variables, "A-efficiency")
  colnames(next_best_studies_D) = c(variables, "D-efficiency")
  next_best_studies_A = next_best_studies_A %>% mutate(`Study No.` = row_number()+nrow(data))%>% relocate(`Study No.`)
  next_best_studies_D = next_best_studies_D %>% mutate(`Study No.` = row_number()+nrow(data))%>% relocate(`Study No.`)

  output = list(next_best_studies_A = next_best_studies_A, next_best_studies_D = next_best_studies_D)
  return(output)
}
