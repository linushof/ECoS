rwp <- function(o1_O, o1_P, o2_O, o2_P, direction='larger') {
  # check lengths
  if(length(o1_O) != length(o1_P) || length(o2_O) != length(o2_P))
    stop("Option input vectors not of same length")
  
  if(direction=='larger'){
    # compute the result
    rwp <- sum( outer(o1_O, o2_O, `>`) * outer(o1_P, o2_P, `*`) )
  }else{
    # compute the result
    rwp <- sum( outer(o1_O, o2_O, `<`) * outer(o1_P, o2_P, `*`) )
  }
  return(rwp)
}



