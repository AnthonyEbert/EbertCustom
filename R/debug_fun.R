


#' put arguments into global environment
#' @param expr R expression
#' @examples
#' arg_list <- debug_fun(expression(lm(x ~ y)))
#' attach(arg_list)
#' @export
debug_fun <- function(expr){



  if(!is.expression(expr)){
    expr <- substitute(expr)
  }

  function_to_get <- get(all.names(expr)[1])

  new_f <- function(...){return(as.list(environment(), all=TRUE))}

  formals(new_f) <- formals(function_to_get)

  # substitute(expression, list(function_to_get = new_f))
  #
  # e <- new.env()
  # e$function_to_get <- new_f

  outp <- gsub(all.names(expr)[1], "new_f", as.character(expr))

  outj <- eval(parse(text = outp))

  return(outj)
}
