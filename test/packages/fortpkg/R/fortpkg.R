scale <- function(x, a) {
  .Fortran(F_scale, x = as.double(x), as.integer(length(x)), as.double(a), ok = FALSE)$x
}

count <- function(x) {
  .Fortran(F_count, as.double(x), as.integer(length(x)), k = integer(1))$k
}

cmplx <- function(z) .Fortran(F_cmplx, as.complex(z))
