filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/soil4.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/test_library.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/testdata2.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/test1520.csv"

test <- read_formatted_spectrum(filename = filename, share = F, id = "dad")
test$coords$filename

test <- read_any(filename = filename, share = F, id = "dad", std_wavenumbers = std_wavenumbers)

files <- unzip(zipfile = filename, list = TRUE)
unzip(filename, exdir = tempdir())
file <- bind_cols(lapply(paste0(tempdir(), "/", files$Name), read_spectrum, share = F, id = "sdfad"))

list(
    "wavenumber" = file$wavenumber...1,
    "spectra" = file %>%
        select(-starts_with("wave")), 
    "coords" = generate_grid(nrow(files))[,filename := files$Name])

generate_grid <- function(x) {
    base <- sqrt(x)
    as.data.table(expand.grid(x = 1:round_any(base, 1, ceiling), y = 1:round_any(base, 1, ceiling))[1:x,])
}

round_any <- function(x, accuracy, f = round){
    f(x / accuracy) * accuracy
}

read_map <- function(filename, share, id, std_wavenumbers){
    files <- unzip(zipfile = filename, list = TRUE)
    unzip(filename, exdir = tempdir())
    if(nrow(files) == 2 & any(grepl("\\.dat$", ignore.case = T, files$Name)) & any(grepl("\\.hdr$", ignore.case = T, files$Name))){
        hs_envi <- hyperSpec::read.ENVI.Nicolet(file = paste0(tempdir(), "/", files$Name[grepl("\\.dat$", ignore.case = T, files$Name)]),
                                                headerfile = paste0(tempdir(), "/", files$Name[grepl("\\.hdr$", ignore.case = T, files$Name)]))
        
        list(
            "wavenumber" = hs_envi@wavelength,
             "spectra" = transpose(as.data.table(hs_envi@data$spc)), 
             "coords" = data.table(x = hs_envi@data$x, y = hs_envi@data$y, filename = gsub(".*/", "", hs_envi@data$filename))
        )
    }
    else if(nrow(files) == 1 & any(grepl("\\.RData$", ignore.case = T, files$Name))){
        assign("file", base::get(load(paste0(tempdir(), "/", files$Name))))
        dt <- generate_grid(x = ncol(file))
        list(
            "wavenumber" =  std_wavenumbers,
            "spectra" = file, 
             "coords" = generate_grid(x = ncol(file))[,filename := files$Name])
    }
    
    else{
        
        file <- bind_cols(lapply(paste0(tempdir(), "/", files$Name), read_spectrum, share = F, id = "sdfad"))

        list(
            "wavenumber" = file$wavenumber...1,
            "spectra" = file %>%
                 select(-starts_with("wave")), 
             "coords" = generate_grid(nrow(files))[,filename := files$Name])
    }
}

read_any <- function(filename, share, id, std_wavenumbers){
    if(grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)", ignore.case = T, filename)){
        read_formatted_spectrum(filename = filename, share = share, id = id)
        #single_data$data <- TRUE
    }
    
    else if(grepl("\\.zip$", ignore.case = T, filename)) {
        read_map(filename = filename, share = share, id = id, std_wavenumbers = std_wavenumbers)
        
    }
}

read_spectrum <- function(filename, share, id) {
    
    as.data.table(
        if(grepl("\\.csv$", ignore.case = T, filename)) {
            tryCatch(fread(filename),
                     error = function(e) {e})
        }
        else if(grepl("\\.[0-9]$", ignore.case = T, filename)) {
            tryCatch(read_0(filename, share = share, id = id),
                     error = function(e) {e})
        }
        
        else {
            ex <- strsplit(basename(filename), split="\\.")[[1]]
            
            tryCatch(do.call(paste0("read_", tolower(ex[-1])),
                             list(filename, share = share, id = id)),
                     error = function(e) {e})
        }
    )
}
    
read_formatted_spectrum <- function(filename, share, id){
    spectra <- read_spectrum(filename = filename, share = share, id = id)
    list("wavenumber" =     
             spectra$wavenumber,
        "spectra" =     
             data.table(spectra$intensity),
         "coords" = as.data.table(expand.grid(x = 1, y = 1, filename = gsub(".*/", "", filename), stringsAsFactors = F))
    )
}
