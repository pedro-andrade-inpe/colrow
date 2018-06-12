
#' @title Rename shapefile
#' @description Rename a shapefile and all other files with the same name
#' @param filename The file to be renamed, with .shp
#' @param newname The new name of the file, without .shp
#' @export
#' @examples
#' \donttest{
#' renameShape("myshape.shp", "my_new_shape")
#' }
renameShape <- function(filename, newname){
  oldname <- basename(filename) %>% tools::file_path_sans_ext()
  dir<- dirname(filename)

  extensions <- c("shp", "shx", "prj", "dbf")

  extension <- extensions[1]

  for(extension in extensions){
    from <- paste0(dir, "/", oldname, ".", extension)

    if(file.exists(from)){
      file.rename(
        from = from,
        to   = paste0(dir, "/", newname, ".", extension)
      )
    }
  }
}





