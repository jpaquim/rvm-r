rvm <- function(x, ...)
    UseMethod("rvm")

rvm.default <- function(x, y, scale = TRUE, type = NULL, kernel = "radial", gamma = NULL, degree = NULL,
                        coef = NULL, class.weights = NULL, cachesize = 40, max_iter = 2000,
                        epsilon = .001){
  
  # NULL parameters?
  if(is.null(x)) stop(sQuote("x"), " must not be NULL!")
  if(is.null(y)) stop(sQuote("y"), " must not be NULL!")
  if(is.null(kernel)) stop(sQuote("kernel"), " must not be NULL!")
  if(is.null(cachesize)) stop(sQuote("cachesize"), " must not be NULL!")
  if(is.null(max_iter)) stop(sQuote("tolerance"), " must not be NULL!")
  if(is.null(epsilon)) stop(sQuote("epsilon"), " must not be NULL!")
  
  # check if x and y match
  if (nrow(x) != nrow(y))
    stop("x and y don't match")
  
  # is kernel valid
  if (!(kernel %in% c("radial", "linear", "polynomial", "sigmoid", "histogram")))
    top(sQuote("kernel"), " type is not valid!")
  
  # determine type
  if (is.null(type)) {
    if (is.factor(y))
      type = "classification"     
    else
      type = "regression"     
  }
  
  # convert factors to integers
  if (type == "classification") {
    y <- as.integer(levels(y))
  }
  
  #set defaults
  if (kernel == "polynomial") {
    if (is.null(gamma))
      gamma <- 1   
    if(is.null(coef))
      coef <- 1
    if(is.null(degree))
      coef <- 1
  } else {
    if (is.null(gamma))
      gamma <- .1
    if(is.null(coef))
      coef <- -1
  }
  
  
  
}
    

    
  
    