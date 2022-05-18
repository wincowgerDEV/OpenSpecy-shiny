conform_spectra <- function(df, wavenumber, std_wavenumbers, correction){
    setcolorder(df[,2:ncol(df)][,lapply(.SD, conform_intensity, wavenumber = wavenumber, correction = correction, std_wavenumbers = std_wavenumbers)][,wavenumber := std_wavenumbers], "wavenumber")
}

conform_intensity <- function(intensity, wavenumber, correction, std_wavenumbers){
    test <- std_wavenumbers %in% conform_wavenumber(wavenumber)
    place <- rep(NA, length.out= length(std_wavenumbers))
    vec <- adjust_intensity(x = conform_wavenumber(wavenumber),
                            y = clean_spec(x = wavenumber, y = intensity),
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

clean_spec <- function(x, y){
    c(
        approx(x = x, y = y, xout = conform_wavenumber(x))$y
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

if(!length(data | !active_preprocessing) {
    data.table(intensity = numeric(), wavenumber = numeric(), SpectrumIdentity = factor())
}

process_intensity <- function(intensity, wavenumber, active_preprocessing, range_decision, min_range, max_range, smooth_decision, smoother, baseline_decision, baseline_selection, baseline, trace, std_wavenumbers) {

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
    
    place[test] <- intensity_cor#try using this for other function
    
    place
    
}

process_spectra <- function(df, wavenumber, active_preprocessing, range_decision, min_range, max_range, smooth_decision, smoother, baseline_decision, baseline_selection, baseline, trace, std_wavenumbers){
    setcolorder(df[,2:ncol(df)][,lapply(.SD, process_intensity, wavenumber = wavenumber, active_preprocessing = active_preprocessing, range_decision = range_decision, min_range = min_range, max_range = max_range, smooth_decision = smooth_decision, smoother = smoother, baseline_decision = baseline_decision, baseline_selection = baseline_selection, baseline = baseline, trace = trace, std_wavenumbers = std_wavenumbers)][,wavenumber := std_wavenumbers], "wavenumber")
}


intensity = conformed[[10]]


std_wavenumbers <- seq(405, 3995, by = 5)

setwd("C:/Users/winco/OneDrive/Documents/zipped_file_test")

file <- read_any("test_library.zip", share = T, id = "sdafd", std_wavenumbers = std_wavenumbers)

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
                                     std_wavenumbers = std_wavenumbers)
                                     #trace, 
                                     #std_wavenumbers)

for(column in 1:ncol(conformed)){
    print(column)
    process_intensity(intensity = conformed[[column]], 
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
                      std_wavenumbers = std_wavenumbers)
}


preprocessed_df <-   process_spectra(df = conformed, 
                                     wavenumber = conformed$wavenumber, 
                                     active_preprocessing = T, 
                                     range_decision = T, 
                                     min_range = 1000, 
                                     max_range = 2000, 
                                     smooth_decision = F, 
                                     smoother = 3, 
                                     baseline_decision = F, 
                                     baseline_selection = "Polynomial", 
                                     baseline = 8, 
                                     trace = NULL,
                                     std_wavenumbers = std_wavenumbers)
preprocessed_df[[4]]

intensity = conformed[[49]]

conformed$`018064006c850b41296c0ff94848b797`
preprocessed_df$`018064006c850b41296c0ff94848b797`


ggplot() +
    geom_line(aes(x = std_wavenumbers, y = make_rel(preprocessed_df[[49]], na.rm = T)), size = 2) + 
    #geom_line(aes(x = std_wavenumbers, y = make_rel(preprocessed, na.rm = T))) + 
    geom_line(aes(x = std_wavenumbers, y = make_rel(conformed[[49]], na.rm = T)), color = "red")
