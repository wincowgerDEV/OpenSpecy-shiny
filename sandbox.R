filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/soil4.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/test_library.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/testdata2.zip"
filename <- "C:/Users/winco/OneDrive/Documents/zipped_file_test/LDPE (1).csv"

#test <- read_formatted_spectrum(filename = filename, share = F, id = "dad")
read.csv(filename)
test <- read_any(filename = filename, share = F, id = "dad", std_wavenumbers = std_wavenumbers)
test$coords$y

test_conformed <- conform_spectra(test$spectra, wavenumber = test$wavenumber, correction = "none")
test_preprocessed <- process_spectra(df = test_conformed, 
                                     wavenumber = conform_wavenumber(test$wavenumber)
                                    )
cor <- correlate_spectra(data = test_preprocessed, search_wavenumbers = conform_wavenumber(test$wavenumber), std_wavenumbers = std_wavenumbers, library = library)

load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/library.RData")
load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/metadata.RData")
load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/library_deriv.RData")

library <- fread("C:/Users/winco/Downloads/data-processed-20220622-125248.csv") %>%
    select(-wavenumber)

save(library,file =  "G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/library_deriv.RData")

#signal to noise ratio
snr <- function(x) {
    y = adj_neg(x[!is.na(x)])
    if(length(y) < 20){
        NA
    }
    else{
        sd  = runSD(y, n = 20) 
        sd[(length(sd) - 19):length(sd)] <- NA
        signal = max(y, na.rm = T)
        noise = min(sd[sd != 0], na.rm = T)
        ifelse(is.finite(signal/noise) & is.finite(noise), log10(signal/noise), 0)
        
    }
}

lib <- library

for(col in 1:ncol(lib)) {
    print(col)
    print(snr(lib[[col]]))
}

x <- lib$`9261082428c40db8bcc4b13dcb8d3486`

ggplot() +
    geom_line(aes(x = std_wavenumbers, y = x))

y = adj_neg(x[!is.na(x)])

sd  = runSD(y, n = 20) 
sd[(length(sd) - 19):length(sd)] <- NA
signal = max(y, na.rm = T)
noise = min(sd[sd != 0], na.rm = T)
ifelse(is.finite(signal/noise) & is.finite(noise), log10(signal/noise), 0)


snr(lib$`1082d26f2c35bd3be0d1e335735b91f1`)

hist(unlist(lapply(lib, snr)))

signal_test <- unlist(lapply(lib, snr))
signal_test[!is.finite(signal_test)]
hist(signal_test)
signal_test <- ifelse(unlist(lapply(library, snr)) <= 0, 0, log10(unlist(lapply(library, snr))))


