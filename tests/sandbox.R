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

read_spectrum <- function(filename, share = F, id = "test") {
    
    as.data.table(
        if(grepl("\\.csv$", ignore.case = T, filename)) {
            tryCatch(read_text_2(file = filename, 
                                 method = "fread",
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
    )
}

read_text_2 <- function(file = ".", cols = NULL, method = "read.csv",
                        share = NULL, id = paste(digest(Sys.info()),
                                                 digest(sessionInfo()),
                                                 sep = "/"), ...) {
    df <- do.call(method, list(file, ...)) %>%
        data.frame()
    
    if (all(grepl("^X[0-9]*", names(df)))) stop("missing header: ",
                                                "use 'header = FALSE' or an ",
                                                "alternative read method")
    
    # Try to guess column names
    if (is.null(cols)) {
        #if (all(grepl("^V[0-9]*", names(df)))) {
        #    cols <- 1:2
        #    warning("missing header: guessing 'wavenumber' and 'intensity' data ",
        #            "from the first two columns; use 'cols' to supply user-defined ",
        #            "columns")
        #} else {
        cols <- c(names(df)[grep("(wav*)", ignore.case = T, names(df))][1L])#,
        #names(df)[grep("(transmit*)|(reflect*)|(abs*)|(intens*)",
        #              ignore.case = T, names(df))][1L])
        #}
    }
    if (any(is.na(cols))) stop("undefined columns selected; columns should be ",
                               "named 'wavenumber' and 'intensity'")
    #if (cols[1] == cols[2]) stop("inconsistent input format")
    
    #df <- df[cols]
    
    # Check if columns are numeric
    if (!all(sapply(df, is.numeric))) stop("input not numeric")
    
    #names(df) <- c("wavenumber", "intensity")
    
    #if (!is.null(share)) share_spec_2(df, file = file, share = share, id = id)
    
    return(df)
}

read_formatted_spectrum <- function(filename, share, id){
    spectra <- read_spectrum(filename = filename, share = share, id = id)
    list("wavenumber" =     
             spectra$wavenumber,
         "spectra" =     
             spectra %>% select(-wavenumber),
         "coords" = generate_grid(x = ncol(spectra) - 1)[,filename := gsub(".*/", "", filename)]
    )
}

read.csv("C:/Users/winco/OneDrive/Documents/zipped_file_test/testdata (10).csv")

read_formatted_spectrum(filename = "C:/Users/winco/OneDrive/Documents/zipped_file_test/testdata (10).csv")


if(grepl("\\.csv$", ignore.case = T, filename)) {
    tryCatch(read_text_2(file = filename, 
                         method = "read.csv",
                         share = share,
                         id = id),
             error = function(e) {e})
}


method = "read.csv"

df <- do.call(method, list(file)) %>%
    data.frame()



read_text_2 <- function(file = ".", cols = NULL, method = "read.csv",
                        share = NULL, id = paste(digest(Sys.info()),
                                                 digest(sessionInfo()),
                                                 sep = "/"), ...) {
    df <- do.call(method, list(file, ...)) 
    
    if (all(grepl("^X[0-9]*", names(df)))) stop("missing header: ",
                                                "use 'header = FALSE' or an ",
                                                "alternative read method")
    
    # Try to guess column names
    if (is.null(cols)) {
        #if (all(grepl("^V[0-9]*", names(df)))) {
        #    cols <- 1:2
        #    warning("missing header: guessing 'wavenumber' and 'intensity' data ",
        #            "from the first two columns; use 'cols' to supply user-defined ",
        #            "columns")
        #} else {
        cols <- c(names(df)[grep("(wav*)", ignore.case = T, names(df))][1L])#,
        #names(df)[grep("(transmit*)|(reflect*)|(abs*)|(intens*)",
        #              ignore.case = T, names(df))][1L])
        #}
    }
    if (any(is.na(cols))){
        cols <- c(names(df)[1L])#,
        warning("undefined columns selected; one column should be named wavenumber; guessing first.")  
    } 
    #if (cols[1] == cols[2]) stop("inconsistent input format")
    
    df <- df %>%
        rename("wavenumber" = cols)
    
    # Check if columns are numeric
    if (!all(sapply(df, is.numeric))){
        df <- df[,c(sapply(df, is.numeric)), with = F]
        warning("Dropping non-numeric columns")  
    } 
    
    #names(df) <- c("wavenumber", "intensity")
    
    #if (!is.null(share)) share_spec_2(df, file = file, share = share, id = id)
    
    return(df)
}
    
df <- fread("C:/Users/winco/OneDrive/Documents/zipped_file_test/LDPE (1).csv")
    list <- sapply(df, is.numeric)

    read_text_2(file = "C:/Users/winco/OneDrive/Documents/zipped_file_test/LDPE (1).csv", 
            method = "fread",
            share = NULL,
            id = "test")
    
    
load("data/library.RData")
load("data/metadata.RData")
library(data.table)
library(dplyr)

fwrite(library %>% mutate(wavenumber = seq(405, 3995, by = 5)), "C:/Users/winco/OneDrive/Documents/Keith/library.csv")
fwrite(meta, "C:/Users/winco/OneDrive/Documents/Keith/metadata.csv")
