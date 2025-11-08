## HW5 Class/Methods

## Class Definition
setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)


## Validity Method
setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("Lengths of 'value' and 'pos' must match.")
  }
  if (any(object@pos < 1L | object@pos > object@length)) {
    return("Positions in 'pos' must be within 1 and 'length'.")
  }
  if (any(duplicated(object@pos))) {
    return("Positions in 'pos' must be unique.")
  }
  TRUE
})


## Coercion Methods
setAs("numeric", "sparse_numeric", function(from) {
  pos <- which(from != 0)
  new("sparse_numeric", value = from[pos], pos = as.integer(pos), length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  out[from@pos] <- from@value
  out
})


## Arithmetic Helper Function
.merge_sparse <- function(x, y, op) {
  if (x@length != y@length)
    stop("Vectors must be the same length.")
  
  all_pos <- union(x@pos, y@pos)
  x_vals <- setNames(x@value, x@pos)
  y_vals <- setNames(y@value, y@pos)
  
  vals <- mapply(function(p) {
    xv <- ifelse(p %in% names(x_vals), x_vals[as.character(p)], 0)
    yv <- ifelse(p %in% names(y_vals), y_vals[as.character(p)], 0)
    op(xv, yv)
  }, all_pos)
  
  nz <- vals != 0
  new("sparse_numeric", value = vals[nz], pos = as.integer(all_pos[nz]), length = x@length)
}



## Generic and Method Definitions
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length)
              stop("Vectors must be the same length.")
            
            # Get all unique positions, sorted
            all_pos <- sort(unique(c(x@pos, y@pos)))
            
            # Initialize value placeholders
            x_vals <- rep(0, length(all_pos))
            y_vals <- rep(0, length(all_pos))
            
            # Fill known positions
            x_vals[match(x@pos, all_pos)] <- x@value
            y_vals[match(y@pos, all_pos)] <- y@value
            
            # Add values
            res_vals <- x_vals + y_vals
            
            # Keep only nonzero results
            keep <- res_vals != 0
            
            # Return sparse_numeric
            new("sparse_numeric",
                value = res_vals[keep],
                pos = as.integer(all_pos[keep]),
                length = x@length)
          })

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) .merge_sparse(x, y, `*`))

setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) .merge_sparse(x, y, `-`))

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length)
              stop("Vectors must be same length.")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })


## Operator Overloads
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))


## Show Method
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero positions:", object@pos, "\n")
  cat("Values:", object@value, "\n")
})


## Plot Method
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  plot(x@pos, x@value, pch = 19, col = "blue", main = "Sparse Vector Comparison",
       xlab = "Position", ylab = "Value", ...)
  points(y@pos, y@value, pch = 19, col = "red")
  legend("topright", legend = c("x", "y"), col = c("blue", "red"), pch = 19)
})


## Additional Method
# Compute the L2 norm of a sparse vector
setGeneric("sparse_norm", function(x) standardGeneric("sparse_norm"))
setMethod("sparse_norm", "sparse_numeric", function(x) sqrt(sum(x@value^2)))


