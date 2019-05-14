#' Fold Change Analysis for Gene Expression Profiling

#' Conduct the differential expression analysis using fold change method and find the (significantly) differential expressed genes(DEGs)

#' @param exprSet The gene expression profiling with each row represents a gene, each column represents an sample
#' @param sig.level The threshold for screening the significantly differential expressed gene, 2 (by default) is the typical value used in fold change methods.
#' @param col.g1 A numeric vector indicates the columns that contain samples from the first group
#' @param col.g2 A numeric vector indicates the columns that contain samples from the second group
#' @param writeout A logical value indicating whether a data frame contains name of the DEGS and their fold change result should output as a .csv file in to the working directory
#' @param q A number less than 1 indicate the quantile you want to set for clustering the fold change result several levels
#' @param log.tran A logical value indicating whether the data needs log transformation, which is what usually done in gene expression analysis.
#' @return A list that contains a factor that contains the fold change results of all the , name of the DEGS, their fold change result
#' @importFrom dplyr "%>%"
#' @export FCgene
FCgene <- function(exprSet, sig.level = 2, col.g1, col.g2, writeout = FALSE, q = 0.2, log.tran = FALSE){
  if(log.tran)
    exprSet <- logtrans_fc(exprSet)
  else
  fc.result <- apply(exprSet[,col.g1], 1, mean) - apply(exprSet[,col.g2], 1, mean)

  fc.DEgene <- fc.result %>% as.matrix(ncol = 2) %>% subset(. > sig.level)
  fc.DEG <- fc.DEgene %>% order(.[,1]) %>% fc.DEgene[.,]
  fc.name <- names(fc.DEG)
  fc.value <- as.numeric(fc.DEG)

  ## writeout
  fc.df <- data.frame("DEG" = fc.name, "FC" = fc.value)
  if(writeout){
    write.csv(fc.df, "./FCgene_result.csv",  quote = F, row.names = F)
  }
  else

  ## Order
  fc.abs <- fc.result %>% abs() %>% sort()
  fc.quan <- fc.abs %>% quantile(probs = seq(0,1,q)) %>% as.numeric()
  fc.abs.fac <- cut(fc.abs, breaks = fc.quan, include.lowest = T)

  return(list(DEG.name = fc.name, DEG.value = fc.value, fc.factor = fc.abs.fac))
}

