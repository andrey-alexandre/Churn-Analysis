se <- function(y, y_hat){
  e <- y - y_hat
  se <- e^2
  
  return(se)
}
pe <- function(y, y_hat){
  e <- y - y_hat
  pe <- e/y
  
  return(pe)
}