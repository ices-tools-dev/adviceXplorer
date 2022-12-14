# #' Programmatically create a Shiny input
# #' 
# #' @param FUN function to create the input
# #' @param n number of inputs to be created
# #' @param id ID prefix for each input
# shinyInput <- function(FUN, n, id, ...) {

#   # for each of n, create a new input using the FUN function and convert
#   # to a character
#   vapply(seq_len(n), function(i){
#     as.character(FUN(paste0(id, i), ...))
#   }, character(1))
  
# }
