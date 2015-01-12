pollutantmean <- function(directory, pollutant, id = 1:332) {
    file_Dir <- getwd()
    file_Dir <- paste(file_Dir, directory, sep = "/")       #file_Dir is the Dir of specdata
    files <- list.files(file_Dir)

    merge_data <- data.frame()
        
    for (i in id) {
        files[i] <- paste(file_Dir,files[i], sep = "/")
        data_Sum <- read.csv(files[i])
        
        data_Poll <- data_Sum[pollutant]
        data_Poll_NNA <- data_Poll[!is.na(data_Poll)]
       
        merge_data <- rbind(merge_data,data.frame(data_Poll_NNA))
    }
    mu <- mean(merge_data$data_Poll_NNA)
    mu
}
