setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
# set.seed(11)


# barplot of 
barPlotOfRanking <- function(rankingDataFrame, valuesToRank, featuresCol, positionCol, exe_num, x_label, y_label, x_upper_lim) 
{
        library("ggplot2")

        dotSize <- 3
        pdfHeight <- 10 # inches
        pdfWidth <- 20 # inches
        textSize <- 30
        
        mkDirResultsCommand <- "mkdir -p ../results_LucaOneto/"
        system(mkDirResultsCommand)
        cat("just run the command: ", mkDirResultsCommand, "\n", sep="")
        
        # thisPdfFile <-  paste("../results_LucaOneto/", y_label, "_features_", exe_num, ".pdf", sep="")
        # pdf(thisPdfFile)
        thisPngFile <-  paste("../results_LucaOneto/", y_label, "_features_", exe_num, ".png", sep="")
        png(thisPngFile)
        p <- ggplot(data=rankingDataFrame, aes(x=reorder(featuresCol, -positionCol), y=valuesToRank)) +  geom_bar(stat="identity", fill="steelblue")  + labs(title = paste("Feature ranking on ", y_label, sep=""), y = y_label, x = x_label)        
        
        if ( x_upper_lim !=-1 ) {
            p <- p + coord_flip(ylim = c(0, x_upper_lim))
            
        } else {
            p <- p + coord_flip()
        }
        
        
        plot(p)
        cat("saved plot in file ", thisPngFile, "\n", sep="")
        # cat("saved plot in file ", thisPdfFile, "\n", sep="")
        dev.off()
        
        return(p)
}


fileName <- "../results_LucaOneto/MDA_RF.csv"
aggregateRankings <- read.table(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
print(cat("fileName = ", fileName, "\n", sep=""))

cat("colnames(aggregateRankings)\n")
cat(colnames(aggregateRankings))

upper_num_limit_here <- 10000
num_to_return <- 1
exe_num <- sample(1:upper_num_limit_here, num_to_return)


FEATURE_RANKING_PLOT_DEPICTION <- TRUE

if (FEATURE_RANKING_PLOT_DEPICTION == TRUE) {
    
        # print(colnames(dd_sorted_IncNodePurity_only))

          upper_num_limit <- 20
         accuracyP <- barPlotOfRanking(aggregateRankings, aggregateRankings$MDAvalue, aggregateRankings$feature, aggregateRankings$MDApos, exe_num, "features", "MDA", upper_num_limit)

            
}
        
