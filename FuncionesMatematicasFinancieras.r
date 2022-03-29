

VF <- function(vp = NULL, i = NULL, n = NULL, type = "compound"){
  # Future value
  stopifnot(is.double(vp) | is.double(vp))
  stopifnot(is.double(i) | is.double(i))
  stopifnot(is.double(n) | is.double(n))
  
  type <- match.arg(type, c("compound", "simple")) 
  value <- vp * ((1 + i)^n)
  if(type == "simple") value <- vp * (1 + n * i)
  return(value)

}

# Example
# VF(vp = 1000000, i = 0.01, n = 60) # 1816697 
# VF(vp = 1000000, i = 0.01, n = 60, type = "simple" ) # # 1600000





VP <- function(vf = NULL, i = NULL, n = NULL, type = "compound"){
  # Present value
  stopifnot(is.double(vf) | is.double(vf))
  stopifnot(is.double(i) | is.double(i))
  stopifnot(is.double(n) | is.double(n))
  
  type <- match.arg(type, c("compound", "simple")) 
  value <- vf / ((1 + i)^n)
  if(type == "simple") value <- vf / (1 + n*i)
  return(value)
  
}

# Ejemplo
# VP(vf = 1816697, i = 0.01, n = 60) 
# VP(vf = 1600000, i = 0.01, n = 60, type = "simple" )




IR  <- function(vp = NULL, vf = NULL, n = NULL, type = "compound"){
  # Present value
  stopifnot(is.double(vp) | is.double(vp))
  stopifnot(is.double(vf) | is.double(vf))
  stopifnot(is.double(n) | is.double(n))
  
  type <- match.arg(type, c("compound", "simple")) 
  value <- ((vf / vp)^(1/n)) - 1
  
  if(type == "simple"){
    value <- (vf - vp) / (n*vp)
  } 
  
  return(value)
}


# Example
# IR(vp = 1000000, vf = 1816697,  n = 60) 
# IR(vp = 1000000, vf = 1600000,  n = 60, type = "simple" )



nper <- function(vp = NULL, vf = NULL, i = NULL, type = "compound"){
  # Present value
  stopifnot(is.double(vp) | is.double(vp))
  stopifnot(is.double(vf) | is.double(vf))
  stopifnot(is.double(i) | is.double(i))
  
  type <- match.arg(type, c("compound", "simple")) 
  value <- log(vf/vp) / log(1+i)
  
  if(type == "simple"){
    value <- (vf - vp) / (vp*i)
  } 
  
  return(value)
  
}

# Example
nper(vp = 1000000, vf = 1816697,  i = 0.01) 
nper(vp = 1000000, vf = 1600000,  i = 0.01, type = "simple" )


