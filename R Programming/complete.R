complete <- function(directory, id = 1:332){
        fileList <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        i <- 1
        for (i in 1:length(id)){
                a <- read.csv(fileList[id[i]])
                f <- table(complete.cases(a))
                nob <- unname(f[2])
                dat <- rbind(dat, c(id[i],nob))
                i <- i + 1
        }
        colnames(dat) <- c("id", "nobs")
        dat
}