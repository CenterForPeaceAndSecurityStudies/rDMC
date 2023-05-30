parentchild <- function(x) {

  x <- x[!is.na(x[ , 1]), , drop = FALSE]

  if(nrow(x) == 0)
    return(NULL)

  if(ncol(x) == 1)
    return(lapply(x[ , 1] , function(v) list(name = v)))

  s <- split(x[ , -1, drop = FALSE], x[ , 1])

  unname(mapply(function(v, n){

    if(!is.null(v)) list(name = n, children = v)

    else

      list(name = n)
  },

  lapply(s, parentchild), names(s), SIMPLIFY = FALSE))

}
