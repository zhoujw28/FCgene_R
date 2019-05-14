#' Visualization of FCgene Object

#' Visualizing of the fold change result using a ggplot style pie chart

#' @param x An output object from FCgene function in the FCgene package
#' @import ggplot2
#' @importFrom dplyr "%>%"
#' @examples
#' data(mydata)
#' MyexprSet <- mydata
#' MyFCgene <- FCgene(MyexprSet, col.g1 = c(1:4), col.g2 = c(5:8))
#' FCplot(MyFCgene)
#' @export FCplot
FCplot <- function(x){
  numgroup <- x$fc.factor %>% levels() %>% length()
  type <- x$factor.factor %>% levels() %>% paste(LETTERS[1:numgroup], . , sep = "")
  nums <- x$fc.factor %>% table() %>% as.numeric()
  df <- data.frame(type = type, nums = nums)
  ## hist first
  p <- ggplot(data = df, mapping = aes(x = 'Content',y = nums, fill = type)) +
    geom_bar(stat = "identity", position = 'stack', width = 1)

  FCprop <- df$nums/sum(df$nums)*100
  label <- FCprop %>% round(., 2) %>% paste('(', . , '%)', sep = '') %>% paste(df$type, . , sep = '')

  p + coord_polar(theta = 'y') + labs(x = '', y = '', title = 'Results of Fold Change') +
    theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) +
    scale_fill_discrete(labels = label)
}
