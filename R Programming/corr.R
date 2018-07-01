corr <- function(directory, threshold = 0){
        ##Makes a list of the files above threshold
        id = 1:332
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
        dat2 <- subset.data.frame(dat, nobs > threshold)
        goodFileList <- dat2[,1]
        ##Loop through the list of good files and add cor to list unless there are none
        if (length(goodFileList) == 0){
                y = numeric()
                y
        }else{
                j <- 1
                x = numeric()
                for (j in 1:length(goodFileList)){
                        b <- read.csv(fileList[goodFileList[j]])
                        x <- append(x, cor(x = b[,2], y = b[,3], use = "complete.obs"))
                        j <= j + 1
                }
        x
        }
}