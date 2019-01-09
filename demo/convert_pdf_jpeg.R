
require(pdftools)

files = list.files(".")

for(myfile in files){
  output <- paste0(tools::file_path_sans_ext(myfile), ".jpg")
  cat(paste0("Creating "), output, "\n")
  pdf_convert(myfile, format = "jpeg", pages = 1, filenames = output,
            dpi =500, antialias = TRUE, opw = "", upw = "", verbose = TRUE)
}
