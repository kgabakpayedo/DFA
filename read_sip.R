read_sip <- function(csv_file, keep_var = NULL, name_out) {
    if (! file.exists(csv_file)) stop('Le fichier n\'existe pas')
    csv_data <- data.table::fread(csv_file, sep = ';',   colClasses = c('numeric', 'character', 'Date', 'Date', 'Date', 'character', 'Date', 'character', 'numeric', 'character', 'numeric', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'character', 'character', 'character', 'numeric', 'character', 'character', 'character', 'numeric'),  encoding = 'UTF-8', data.table = FALSE,  select = keep_var) 

    for(col in colnames(csv_data)) if(col %in% c('SAL_QPV'))  csv_data[[col]] <- as.logical(csv_data[[col]])
    .GlobalEnv[[name_out]] <- csv_data
    }

