# @author Scott Dobbins
# @version 0.9.9.6
# @date 2018-01-11 21:00


### Load from utils folder --------------------------------------------------

utils_in_order <- c('utils_general.R', 
                    'utils_vectors.R', 
                    'utils_vectors.R', 
                    'utils_numerics.R', 
                    'utils_logicals.R', 
                    'utils_regex.R', 
                    'utils_files.R', 
                    'utils_characters.R', 
                    'utils_factors.R')

# finds directory name of currently running script (utils_standard.R)
this_file <- rprojroot::thisfile_source()
utils_folder <- substr(this_file, start = 1L, stop = nchar(this_file) - 16L)

for (util in utils_in_order) {
  source(paste0(utils_folder, util))
}
