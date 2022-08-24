#' Function to Calculate Efficiency (D- AND A-EFFICIENCY)
#'
#' @param data - input data
#' @param variables - list of variables to include in the design matrix (should match with the column names of data)
#' @param labels - list of labels for variables
#' @param corrvar - name of the column corresponding to correlation coefficients in the input data
#'
#' @return -  Number of studies, a_efficiency, d_efficiency
#' @export
#'
#' @examples
efficiency <- function(data, variables, labels, corrvar){
  ##INPUTS
  #data = input data
  #variables = list of variables to include in the design matrix (should match with the column names of data)
  #labels = list of labels for variables
  #corrvar = name of the column corresponding to correlation coefficients

  ##CREATE DESIGN MATRIX
  Z <- lapply(data[variables], factor)  #dummy code variables in the data
  Z <- data.frame(Z)  #convert Z to a matrix with factor variables

  ##STANDARDIZED ORTHOGONAL CONTRAST CODE THE DESIGN MATRIX
  new_Z <- list() #standardized orthogonal contrast coding of Z
  for(i in 1:length(variables)){
    variable_name <- variables[i]
    # print(variable_name)
    subset_data <- as.data.frame(Z[i])
    levels <- levels(subset_data[, 1])
    ##STANDARDIZED ORTHOGONAL CONTRAST CODING
    EC <- cbind(rep(1, each = length(levels)), contr.sum(levels(subset_data[,1])))
    SOCC <- orthonormalization(EC)*(length(levels))^(1/2)
    SOCC <- SOCC[, (2:(length(levels)))]
    contrasts(subset_data[, 1]) <- SOCC
    new_Z[[i]] <- subset_data
  }

  ##UPDATE DESIGN MATRIX TO AVOID SINGULARITY OF (Z'Z); Z IS NOT OF FULL RANK
  new_Z <-  cbind(data[corrvar] , data.frame(new_Z))  #first column is the correlation (dependent variable)
  frmla <- as.formula(paste(colnames(new_Z)[1], paste(colnames(new_Z)[2:(ncol(new_Z))], sep = "", collapse = " + "), sep = " ~ "))
  reg <- lm(frmla, data = new_Z)
  coeff <- as.data.frame(reg$coefficients)
  additional_drop <- as.list(names(which(rowSums(is.na(coeff)) > 0)))
  occ_design <- as.data.frame(model.matrix(reg))
  occ_design <- occ_design[, !(names(occ_design) %in% additional_drop)]
  Z <- as.matrix(occ_design)

  ##CALCULATE D- AND A-EFFICIENCY
  ZtZ <- t(Z) %*% Z
  inv_ZtZ <- matrix.inverse(ZtZ)
  Np <- dim(Z)[1]
  p <- dim(Z)[2]
  trace <- sum(diag(inv_ZtZ))
  det <- det(inv_ZtZ)
  a_efficiency <- (1/(Np*(trace/p)))*100
  d_efficiency <- (1/(Np*(det^(1/p))))*100
  output <- cbind(N = nrow(Z), a_efficiency, d_efficiency)

  return(output)
}
