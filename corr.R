corr   <- function(directory, threshold = 0) {
    com_data <- complete(directory)
    b_tmp <- com_data$nobs > threshold
    newid <- com_data$id[b_tmp]
    
    i_cor <- vector(mode = "numeric")
    if (length(newid) > 0) {
        file_Dir <- getwd()
        file_Dir <- paste(file_Dir, directory, sep = "/")
        files <- list.files(file_Dir)
        
        v_cor <- vector()
        
        for (i in newid) {
            files[i] <- paste(file_Dir,files[i], sep = "/")
            data_Sum <- read.csv(files[i])
            
            data_NNA <- complete.cases(data_Sum[["nitrate"]],data_Sum[["sulfate"]])
            
            i_tmpcor <- cor(data_Sum$nitrate[data_NNA],data_Sum$sulfate[data_NNA])  
            v_cor <- cbind(v_cor,i_tmpcor, deparse.level = 0)
        }
        i_cor <- v_cor[1,] 
    }
    i_cor
}
