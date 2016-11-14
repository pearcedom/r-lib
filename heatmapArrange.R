## A function to take a matrix/dataframe (designed using expression data) and make it suitable for plotting with ggplot2
# data_in is a data frame
# class_by is a vector of strings or regular expression that are used to assign a class column. These must reflect some element common to the colnames of the original input matrix
# scale allows scaling
# by_row determines whether scaling is performed on rows or columns
heatmapArrange <- function(data_in, class_by = "", cluster_row = FALSE, cluster_column = FALSE, scale = TRUE, by_row = TRUE){
    library(reshape2)
    #clustering : if all samples have the same value, clustering fails so remove those which do
    data_in <- data_in[sapply(1:nrow(data_in), function(x) sd(data_in[x,])) != 0,]
    #scale data by row or column (or neither)
    if(scale == TRUE){
        if(by_row == TRUE){
            dfr <- data.frame(t(scale(t(data_in))))
            } else {
                dfr <- data.frame(scale(data_in))
            }
        } else {
            dfr <- data_in
        }
    #melt
    dfr$row_value <- row.names(dfr)
    dfr_mlt <- melt(dfr)
    #cluster by row
    if(cluster_row == TRUE){
        r_clst <- hclust(as.dist(1-cor(t(data_in), method = "pearson")), method = "complete", members = NULL)
        dfr_mlt$row_value <- factor(dfr_mlt$row_value, levels = r_clst$labels[r_clst$order])
    }
    #cluster by column
    if(cluster_column == TRUE){
        c_clst <- hclust(as.dist(1-cor(data_in, method = "pearson")), method = "complete", members = NULL)
        dfr_mlt$variable <- factor(dfr_mlt$variable, levels = c_clst$labels[c_clst$order])
    }
    #assign a sample type if wanted
    if(!identical(class_by, "")){
        for(x in 1:length(class_by)){
            class_vec <- c()
            for(i in class_by[[x]]){
                class_vec[grep(i, droplevels(dfr_mlt$variable))] <- i
            }
            dfr_mlt[[names(class_by)[x]]] <- class_vec
        }
    }
    print("scale_fill_gradient2 reminder : scale_fill_gradient2(high = #d73027, mid = black, low = #1a9850)")
    dfr_mlt
}