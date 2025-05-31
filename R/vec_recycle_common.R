vec_recycle_common <- function(x, y) {

  nx <- length(x)
  ny <- length(y)

  if (identical(nx, ny)) {
    return(list(x = x, y = y))
  }

  if (identical(nx, 1L) && ny > 1L) {
    return(list(x = rep(x, times = ny), y = y))
  }

  if (nx > 1L && identical(ny, 1L)) {
    return(list(x = x, y = rep(y, times = nx)))
  }

  stop("Can't recycle `x` (size " ,
       nx,
       ") ",
       "to match `y` (size ",
       ny,
       ").",
       call. = FALSE)

}
