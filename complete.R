complete  <- function(directory, id = 1:332) {
    file_Dir <- getwd()
    file_Dir <- paste(file_Dir, directory, sep = "/")  
    files <- list.files(file_Dir)
    
    com_data <- data.frame()
    com_data_tmp <- data.frame()
    i <- numeric(0)
    for (i in id) {
        files[i] <- paste(file_Dir,files[i], sep = "/")
        data_Sum <- read.csv(files[i])

        data_NNA <- complete.cases(data_Sum[["nitrate"]],data_Sum[["sulfate"]])
        
        nobs <- length(data_Sum$ID[data_NNA])
        if (nobs > 0) {
            id <- i
            com_data_tmp <- cbind(id,nobs)
            com_data <- rbind(com_data,com_data_tmp)
        }        
    }
    com_data
}
