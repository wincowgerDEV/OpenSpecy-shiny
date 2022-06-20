filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/soil4.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/test_library.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/testdata2.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/LDPE (1).csv"

#test <- read_formatted_spectrum(filename = filename, share = F, id = "dad")
read.csv(filename)
test <- read_any(filename = filename, share = F, id = "dad", std_wavenumbers = std_wavenumbers)
test$coords$y

test_conformed <- conform_spectra(test$spectra, wavenumber = test$wavenumber, correction = "none")

load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/library.RData")
load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/metadata.RData")

cor <- correlate_spectra(data = test_conformed, search_wavenumbers = conform_wavenumber(test$wavenumber), std_wavenumbers = std_wavenumbers, library = library)

#Correlate functions ----
correlate_intensity <- function(intensity, search_wavenumbers, std_wavenumbers, lib){
    c(cor(intensity, lib, use = "pairwise.complete.obs"))
}

correlate_spectra <- function(data, search_wavenumbers, std_wavenumbers, library){
    data[search_wavenumbers %in% std_wavenumbers,][,lapply(.SD, correlate_intensity, search_wavenumbers = search_wavenumbers, std_wavenumbers = std_wavenumbers, lib = library[std_wavenumbers %in% search_wavenumbers,])]
}


