conform_spectra <- function(df, wavenumber, std_wavenumbers, correction){
    setcolorder(df[,2:ncol(df)][,lapply(.SD, conform_intensity, wavenumber = wavenumber, correction = correction, std_wavenumbers = std_wavenumbers)][,wavenumber := std_wavenumbers], "wavenumber")
}

intensity = file$spectra$intensity...2
wavenumber = file$spectra$wavenumber
correction = "none"

conform_intensity <- function(intensity, wavenumber, correction, std_wavenumbers){
    test <- std_wavenumbers %in% conform_wavenumber(wavenumber)
    new_wavenumbers <- std_wavenumbers[test]
    place <- rep(NA, length.out= length(std_wavenumbers))
    vec <- adjust_intensity(x = new_wavenumbers,
                            y = clean_spec(x = wavenumber, y = intensity, out = new_wavenumbers),
                            type = correction,
                            na.rm = T)[,"intensity"]
    place[test] <- vec
    place
}

adjust_intensity <- function(x, y, type = "none", make_rel = F, ...) {
    yadj <- switch(type,
                   "reflectance" = (1 - y/100)^2 / (2 * y/100),
                   "transmittance" = log10(1/adj_neg(y, ...)),
                   "none" = adj_neg(y, ...)
    )
    if (make_rel) yout <- make_rel(yadj) else yout <- yadj
    
    data.frame(wavenumber = x, intensity = yout)
}


conform_wavenumber <- function(wavenumber){
    seq(round_any(min(wavenumber), 5, ceiling), round_any(max(wavenumber), 5, floor), by = 5)
}

clean_spec <- function(x, y, out){
    c(
        approx(x = x, y = y, xout = out)$y
    )
}

read_map <- function(filename, share, id, std_wavenumbers){
    files <- unzip(zipfile = filename, list = TRUE)
    unzip(filename, exdir = tempdir())
    if(nrow(files) == 2 & any(grepl("\\.dat$", ignore.case = T, files$Name)) & any(grepl("\\.hdr$", ignore.case = T, files$Name))){
        hs_envi <- hyperSpec::read.ENVI.Nicolet(file = paste0(tempdir(), "/", files$Name[grepl("\\.dat$", ignore.case = T, files$Name)]),
                                                headerfile = paste0(tempdir(), "/", files$Name[grepl("\\.hdr$", ignore.case = T, files$Name)]))@data
        
        list("spectra" = transpose(as.data.table(hs_envi$spc), keep.names = "wavenumber") %>%
                 mutate(wavenumber = as.numeric(wavenumber)), 
             "coords" = data.table(x = hs_envi$x, y = hs_envi$y))
    }
    else if(nrow(files) == 1 & any(grepl("\\.RData$", ignore.case = T, files$Name))){
        assign("file", base::get(load(paste0(tempdir(), "/", files$Name))))
        base <- sqrt(ncol(file)-1)
        file$wavenumber <- std_wavenumbers
        list("spectra" = file %>%
                 select(wavenumber, everything()), 
             "coords" = expand.grid(x = 1:round_any(base, 1, ceiling), y = 1:round_any(base, 1, ceiling))[1:(ncol(file)-1),])
    }
    
    else{
        base <- sqrt(nrow(files))
        
        list("spectra" = bind_cols(lapply(paste0(tempdir(), "/", files$Name), read_spectrum, share = F, id = "sdfad")) %>%
                 select(wavenumber...1, starts_with("intens")) %>%
                 rename(wavenumber = wavenumber...1), 
             "coords" = expand.grid(x = 1:round_any(base, 1, ceiling), y = 1:round_any(base, 1, ceiling))[1:nrow(files),])
    }
}

read_any <- function(filename, share, id, std_wavenumbers){
    if(grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)", ignore.case = T, filename)){
        read_spectrum(filename = filename, share = share, id = id)
        #single_data$data <- TRUE
    }
    
    else if(grepl("\\.zip$", ignore.case = T, filename)) {
        read_map(filename = filename, share = share, id = id, std_wavenumbers = std_wavenumbers)
        
    }
}

read_spectrum <- function(filename, share, id) {
    
    list("spectra" =     
             as.data.table(
                 if(grepl("\\.csv$", ignore.case = T, filename)) {
                     tryCatch(read_text(filename, method = "fread",
                                        share = share,
                                        id = id),
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
             ),
         "coords" = data.table(x = 1, y = 1)
    )
}


process_cor_os <- function(x){
    abs(
        c(
            scale( 
                signal::sgolayfilt(x,
                                   p = 3, n = 11, m = 1
                )
            ) 
        )
    )
}

if(!length(data | !active_preprocessing) {
    data.table(intensity = numeric(), wavenumber = numeric(), SpectrumIdentity = factor())
}

process_intensity <- function(intensity, wavenumber, active_preprocessing, range_decision, min_range, max_range, smooth_decision, smoother, baseline_decision, baseline_selection, baseline, derivative_decision, trace, std_wavenumbers) {

    test <- std_wavenumbers %in% std_wavenumbers[!is.na(intensity)]
    place <- rep(NA, length.out= length(std_wavenumbers))
    
    #set innitial conditions
    intensity_cor <- intensity[!is.na(intensity)]
    wavenumber_cor <- wavenumber[!is.na(intensity)]
    test2 <-  length(wavenumber_cor[wavenumber_cor > min_range & wavenumber_cor < max_range]) > 11
    
    #Range criteria   
    if(range_decision & test2) {
        #assumes that all the wavenumbers exist, but they don't 
        intensity_cor <- intensity_cor[wavenumber_cor >= min_range & wavenumber_cor <= max_range]
        wavenumber_cor <- wavenumber_cor[wavenumber_cor >= min_range & wavenumber_cor <= max_range]
        test <- std_wavenumbers %in% std_wavenumbers[std_wavenumbers >= min(wavenumber_cor) & std_wavenumbers <= max(wavenumber_cor)]
        
        } 
    
    #Smooth criteria
    if(smooth_decision) {
        intensity_cor <- smooth_intens(wavenumber_cor, intensity_cor, p = smoother)$intensity
            }
    #Baseline criteria
    if(baseline_decision & baseline_selection == "Polynomial") {
        intensity_cor <- subtr_bg(wavenumber_cor, intensity_cor, degree = baseline)$intensity
            }
    else if(baseline_decision & baseline_selection == "Manual" & !is.null(trace$data)){
        intensity_cor <-  intensity_cor - approx(trace$data$wavenumber, trace$data$intensity, xout = wavenumber_cor, rule = 2, method = "linear", ties = mean)$y
    }
    
    #Derivative
    if(derivative_decision) {
        intensity_cor <-  process_cor_os(intensity_cor)
    }
    
    place[test] <- intensity_cor#try using this for other function
    
    place
    
}

process_spectra <- function(df, wavenumber, active_preprocessing, range_decision, min_range, max_range, smooth_decision, smoother, baseline_decision, baseline_selection, baseline, derivative_decision, trace, std_wavenumbers){
    setcolorder(df[,2:ncol(df)][,lapply(.SD, process_intensity, wavenumber = wavenumber, active_preprocessing = active_preprocessing, range_decision = range_decision, min_range = min_range, max_range = max_range, smooth_decision = smooth_decision, smoother = smoother, baseline_decision = baseline_decision, baseline_selection = baseline_selection, baseline = baseline, derivative_decision = derivative_decision, trace = trace, std_wavenumbers = std_wavenumbers)][,wavenumber := std_wavenumbers], "wavenumber")
}


identify_spectra <- function(df, library, metadata){
    identified <- df[,2:ncol(df)][,lapply(.SD, identify_similarity, library = library)]
    left_join(data.table(identified_name = colnames(identified), sample_name = as.vector(as.matrix(identified[1])), rsq = as.vector(as.matrix(identified[2]))),
                        metadata)
}


identify_similarity <- function(spectrum, library){
    correlation <- cor(spectrum[!is.na(spectrum)], 
                       library[!is.na(spectrum),], 
                       use = "everything")
    c(colnames(library)[
        which.max(
            correlation
        )
    ],
    max(correlation, na.rm = T)
    )
}  


intensity = conformed[[10]]


std_wavenumbers <- seq(405, 3995, by = 5)

setwd("C:/Users/winco/OneDrive/Documents/zipped_file_test")

file <- read_any("test_library.zip", share = T, id = "sdafd", std_wavenumbers = std_wavenumbers)

load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/library_deriv.RData")
load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/OpenSpecy-shiny/OpenSpecy-shiny/data/metadata.RData")

conformed <- conform_spectra(df = file$spectra, 
                             wavenumber = file$spectra$wavenumber, 
                             correction = "none", 
                             std_wavenumbers = std_wavenumbers)

conformed[[40]]

preprocessed <- process_intensity(intensity = conformed$`018064006c850b41296c0ff94848b797`, 
                                     wavenumber = conformed$wavenumber, 
                                     active_preprocessing = T, 
                                     range_decision = T, 
                                     min_range = 1000, 
                                     max_range = 2000, 
                                     smooth_decision = T, 
                                     smoother = 3, 
                                     baseline_decision = T, 
                                     baseline_selection = "Polynomial", 
                                     baseline = 8, 
                                     derivative_decision = T,
                                     std_wavenumbers = std_wavenumbers)
                                     #trace, 
                                     #std_wavenumbers)



preprocessed_df <-   process_spectra(df = conformed, 
                                     wavenumber = conformed$wavenumber, 
                                     active_preprocessing = T, 
                                     range_decision = F, 
                                     min_range = 1000, 
                                     max_range = 2000, 
                                     smooth_decision = F, 
                                     smoother = 3, 
                                     baseline_decision = F, 
                                     baseline_selection = "Polynomial", 
                                     baseline = 8, 
                                     derivative_decision = T,
                                     trace = NULL,
                                     std_wavenumbers = std_wavenumbers)

test <- unlist(lapply(preprocessed_df[,2:ncol(preprocessed_df)], snr))
preprocessed_df[[4]]

intensity = conformed[[49]]

conformed$`018064006c850b41296c0ff94848b797`
spectrum <- preprocessed_df$intensity...2

library(TTR)


x = rnorm(n = 400)

snr <- function(x) {
    max  = runMax(x[!is.na(x)], n = 20) 
    max[(length(max) - 19):length(max)] <- NA
    #mean = runMean(x[!is.na(x)], n = 10)
    #mean[(length(mean) - 9):length(mean)] <- NA
    signal = max[which.max(max)]#/mean(x, na.rm = T)
    noise = max[which.min(max[max != 0])]
    signal/noise
}

snr(rnorm(n = 400))

ggplot() + 
    geom_line(aes(x = 1:400, y = x)) +
    geom_line(aes(x = 1:400, y = max))


hist(rnorm(n = 400))

cortest <- cor(preprocessed_df[,2:ncol(preprocessed_df)][!is.na(preprocessed_df[[2]]),], library[!is.na(preprocessed_df[[2]]),], use = "pairwise.complete.obs")

rowmax <- apply(cortest, 1, function(x) max(x, na.rm = T))

whatmax <- colnames(cortest)[apply(cortest, 1, function(x) which.max(x))]


cols <- sample(1:ncol(library), 100, replace = F)
test <- library[, ..cols]

row1 <- cortest[1,]
joined <- left_join(data.table(sample_name = names(cortest[1,]), rsq = cortest[1,]), meta)

identified <- identify_spectra(df = preprocessed_df, library = library, metadata = meta)

formatted <- data.table(identified_name = colnames(identified), sample_name = as.vector(as.matrix(identified[1])), rsq = as.vector(as.matrix(identified[2])))

joined <- left_join(formatted, meta)

row1 <- as.vector(as.matrix(identified[1]))




check <- library[!is.na(preprocessed_df$`0003cda3af6ecb4b60eca8336f4e34e9`),]

preprocessed_df$`0003cda3af6ecb4b60eca8336f4e34e9`[!is.na(preprocessed_df$`0003cda3af6ecb4b60eca8336f4e34e9`)]

ggplot() +
    geom_line(aes(x = preprocessed_df$wavenumber, y = make_rel(spectrum, na.rm = T))) #+ 
    #geom_line(aes(x = std_wavenumbers, y = make_rel(preprocessed, na.rm = T))) + 
    #geom_line(aes(x = conformed$wavenumber, y = make_rel(conformed$`intensity...2`, na.rm = T)), color = "red") #+
    #geom_line(aes(x = vec$wavenumber, y = make_rel(vec$intensity, na.rm = T)), color = "blue")
