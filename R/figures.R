# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   04:04 PM Friday, 25 October 2013
# * Copyright: AS IS
# *



#' Default of ggplot2
#'
#' @param n the number of colours generated
#' @export
myCols <- function(n)
{
    hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65)
}

