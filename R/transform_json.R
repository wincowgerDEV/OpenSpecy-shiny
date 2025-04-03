transform_library <- function (library){
  
  if (is_OpenSpecy(library) != TRUE) {
    
  list(
    spectra = as.data.table(lapply(rds$spectra, unlist)),
    metadata = as.data.table(lapply(rds$metadata, unlist)),
    wavenumber = as.double(rds$wavenumber)
  ) |> as_OpenSpecy()
}

}
