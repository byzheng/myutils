# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   12:03 PM Thursday, 28 August 2014
# * Copyright: AS IS
# *


# function about publications

#' Print figures into files including pdfs and pngs
#' @param p the figure
#' @param filename the output filename without extension
#' @param height Figure height
#' @param width Figure width
#' @param format The output formats
#' @param fontsize The font size in the figures
#' @param fun The function to export figures
#' @export
myPrintFigures <- function(p, filename = NULL, height = NULL, width = NULL, format = c('pdf', 'png'),
    fontsize = 10, fun = NULL)
{
    p_class <- class(p)

    o_args <- NULL
    fun_call <- print
    if ('ggplot' %in% p_class)
    {
        p <- p + theme(text = element_text(size = fontsize)) 
    } else if ('gtable' %in% p_class)
    {        
        for (i in seq(along = p$grobs))
        { 
            class_i <- class(p$grobs[[i]])
            if ('text' %in% class_i)
            {
                p$grobs[[i]] <- modifyList(p$grobs[[i]], list(gp = list(fontsize = fontsize, cex = 1)))
            } else if ('absoluteGrob' %in% class(p$grobs[[i]]))
            {
                # str(p$grobs[[i]])
                # names(p$grobs[[i]])
                for (j in seq(along = p$grobs[[i]]$children))
                {
                    class_j <- class(p$grobs[[i]]$children[[j]])
                    if ('text' %in% class_j)
                    {
                        p$grobs[[i]]$children[[j]] <- modifyList(p$grobs[[i]]$children[[j]], 
                            list(gp = list(fontsize = fontsize, cex = 1)))
                    } else if ('gtable' %in% class_j)
                    {
                        for (k in seq(along = p$grobs[[i]]$children[[j]]$grobs))
                        {
                            class_k <- class(p$grobs[[i]]$children[[j]]$grobs[[k]])
                            if ('text' %in% class_k)
                            {
                                p$grobs[[i]]$children[[j]]$grobs[[k]] <- 
                                    modifyList(p$grobs[[i]]$children[[j]]$grobs[[k]], 
                                        list(gp = list(fontsize = fontsize, cex = 1)))
                            }
                        }
                    }
                }
            } else if ('gtable' %in% class_i)
            {
                for (j in seq(along = p$grobs[[i]]$grobs))
                {
                    class_j <- class(p$grobs[[i]]$grobs[[j]])
                    if ('text' %in% class_j)
                    {
                        p$grobs[[i]]$grobs[[j]] <- 
                            modifyList(p$grobs[[i]]$grobs[[j]], 
                                list(gp = list(fontsize = fontsize, cex = 1)))
                    } else if ('gtable' %in% class_j)
                    {
                        for (k in seq(along = p$grobs[[i]]$grobs[[j]]$grobs))
                        {
                            class_k <- class(p$grobs[[i]]$grobs[[j]]$grobs[[k]])
                            if ('text' %in% class_k)
                            {
                                p$grobs[[i]]$grobs[[j]]$grobs[[k]] <- 
                                    modifyList(p$grobs[[i]]$grobs[[j]]$grobs[[k]], 
                                        list(gp = list(fontsize = fontsize, cex = 1)))
                            }
                        }
                    }
                }
            }
        }
        fun_call <- grid.draw
    } else if ('list' %in% p_class)
    {
        fun_call <- fun
    } else if ('trellis' %in% p_class)
    {
        fun_call <- print
    } else
    {
        stop('NOT IMPLEMENTED')
    }
    
    if ('pdf' %in% format)
    {
        pdf(paste0(filename, '.pdf'), height = height, width = width)
        do.call(fun_call, c(list(p), o_args))
        dev.off()
    }
    if ('png' %in% format)
    {
        png(paste0(filename, '.png'), height = height, width = width, units = 'in', res = 300)
        do.call(fun_call, c(list(p), o_args))
        dev.off()
    }
    if (is.null(format)) {
        do.call(fun_call, c(list(p), o_args))
    }
}

