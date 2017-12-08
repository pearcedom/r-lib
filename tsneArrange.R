#Given a data frame/matrix, calculate tsne coordinates and arrange as a data frame
tsneArrange <- function(data, perplexity = 30){
        library(Rtsne)
        tsne_out <- Rtsne(data, 
                          perplexity = perplexity,
                          max_iter = 5000)$Y 
        data.frame(tsne_out, xpr_id = row.names(data))
}
