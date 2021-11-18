# # * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# # * Created:   06/04/2010
# # *
#
#
# systemCommand <- function(command, res = TRUE)
# {
# 	tryCatch({
# 		bat_name <- tempfile(tmpdir=".", fileext=".bat")
#
# 		if (res)
# 		{
# 			writeLines(paste(command, ' > temp.txt\n', sep = ''), bat_name)
# 		} else
# 		{
# 			writeLines(command, bat_name)
# 		}
# 		system(bat_name, show.output.on.console = TRUE)
# 		result <- NULL
# 		if (res)
# 		{
# 			try({
# 				result <- readLines('temp.txt')
# 				file.remove('temp.txt')
# 			}, silent = TRUE)
# 		}
# 		file.remove(bat_name)
# 		return(result)
# 	}, error = function(e) {
# 		return(NULL)
# 	})
# }
