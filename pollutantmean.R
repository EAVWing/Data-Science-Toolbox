pollutantmean <- function(directory, pollutant, id = 1:332){
        fileList <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        i <- 1
        for (i in 1:length(id)){
                dat <- rbind(dat, read.csv(fileList[id[i]]))
                i <- i + 1
        }
        p <- if(pollutant == "sulfate"){
                2
        }else{
                3
        }
        mean(dat[,p], na.rm = TRUE)
}

