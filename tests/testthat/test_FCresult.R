library(FCgene)
library(dplyr)
context("Test if the FCgene function does output a list that contains the name of the DEGs,
        the fold change values that the DEGs have,
        and a factor that clusters the fold change results into several groups as requested")

set.seed(513)
gene_name <- paste("gene", LETTERS, sep = "")
sample_name <- paste("sample", c(1:8), sep = "")

MyexprSet <- lapply(gene_name, FUN = function(x) x = c(rnorm(4, 0, 6), rnorm(4, 0, 60))) %>%
  as.data.frame(row.names = sample_name, col.names = gene_name) %>% t()

MyFCresult <- FCgene(exprSet = MyexprSet, col.g1 = 1:4, col.g2 = 5:8)

test_that("The FCgene outputs the right format of results",{
   expect_is(MyFCresult, "list")
   expect_length(MyFCresult, 3)
   })
