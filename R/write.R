
#' @title Write ID objects into a text file that can be used by GLOBIOM
#' @description Write 10 ID objects per line, plus a header and a line with slash,
#' according to GAMS requisites.
#' @param tokens A vector of IDs.
#' @param header A string.
#' @param outfile Name of the file to be saved.
#' @export
writeAllColRow <- function(tokens, header, outfile){
  result <- paste0(header, "\n/\n")
  nlines <- ceiling(length(tokens) / 10)
  token.list <- split(tokens, rep(1:nlines,
                                  each=10, len=length(tokens)))
  tokens <- paste(lapply(token.list,paste,collapse=", "), collapse = ",\n")
  result <- paste0(result, tokens)

  f <- file(outfile)
  write(result, f)
  close(f)
}
