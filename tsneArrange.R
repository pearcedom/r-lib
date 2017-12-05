#Given a data frame/matrix, calculate tsne coordinates and arrange as a data frame
tsneArrange <- functin(data){
        library(Rtsne)
        tsne_out <- Rtsne(data)$Y 
        data.frame(tsne_out, xpr_id = row.names(data))
}
