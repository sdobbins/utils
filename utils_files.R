# @author Scott Dobbins
# @version 0.9.9.6
# @date 2018-01-11 19:00


read_list_of_words <- function(file) {
  return (read.csv(file, header = FALSE, stringsAsFactors = FALSE)[["V1"]])
}
