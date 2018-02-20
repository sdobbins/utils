# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Extras ------------------------------------------------------------------

`%><%` <- function(a, b) xor(a, b)
`%!&%` <- function(a, b) !(a & b)
`%!|%` <- function(a, b) !(a | b)
none <- function(...) !any(...)
not_all <- function(...) !all(...)


### Bits --------------------------------------------------------------------

# bitwise logical operators
`%&%` <- function(a, b) bitwAnd(a, b)
`%|%` <- function(a, b) bitwOr(a, b)
`%^%` <- function(a, b) bitwXor(a, b)

# bitshifts
`%<<%` <- function(a, n) bitwShiftL(a, n)
`%>>%` <- function(a, n) bitwShiftR(a, n)


### Mapped ------------------------------------------------------------------

# standard
map_and <- function(a, b) Map(`&`, a, b)
map_and_vec <- function(a, b) unlist(Map(`&`, a, b), use.names = FALSE)
`%map_and%` <- function(a, b) Map(`&`, a, b)
`%map_and_vec%` <- function(a, b) unlist(Map(`&`, a, b), use.names = FALSE)

map_or <- function(a, b) Map(`|`, a, b)
map_or_vec  <- function(a, b) unlist(Map(`|`, a, b), use.names = FALSE)
`%map_or%`  <- function(a, b) Map(`|`, a, b)
`%map_or_vec%`  <- function(a, b) unlist(Map(`|`, a, b), use.names = FALSE)

map_e <- function(a, b) Map(`==`, a, b)
map_e_vec <- function(a, b) unlist(Map(`==`, a, b), use.names = FALSE)
`%map_e%` <- function(a, b) Map(`==`, a, b)
`%map_e_vec%` <- function(a, b) unlist(Map(`==`, a, b), use.names = FALSE)

map_ne <- function(a, b) Map(`!=`, a, b)
map_ne_vec <- function(a, b) unlist(Map(`!=`, a, b), use.names = FALSE)
`%map_ne%` <- function(a, b) Map(`!=`, a, b)
`%map_ne_vec%` <- function(a, b) unlist(Map(`!=`, a, b), use.names = FALSE)

map_gt <- function(a, b) Map(`>`, a, b)
map_gt_vec <- function(a, b) unlist(Map(`>`, a, b), use.names = FALSE)
`%map_gt%` <- function(a, b) Map(`>`, a, b)
`%map_gt_vec%` <- function(a, b) unlist(Map(`>`, a, b), use.names = FALSE)

map_gte <- function(a, b) Map(`>=`, a, b)
map_gte_vec <- function(a, b) unlist(Map(`>=`, a, b), use.names = FALSE)
`%map_gte%` <- function(a, b) Map(`>=`, a, b)
`%map_gte_vec%` <- function(a, b) unlist(Map(`>=`, a, b), use.names = FALSE)

map_lt <- function(a, b) Map(`<`, a, b)
map_lt_vec <- function(a, b) unlist(Map(`<`, a, b), use.names = FALSE)
`%map_lt%` <- function(a, b) Map(`<`, a, b)
`%map_lt_vec%` <- function(a, b) unlist(Map(`<`, a, b), use.names = FALSE)

map_lte <- function(a, b) Map(`<=`, a, b)
map_lte_vec <- function(a, b) unlist(Map(`<=`, a, b), use.names = FALSE)
`%map_lte%` <- function(a, b) Map(`<=`, a, b)
`%map_lte_vec%` <- function(a, b) unlist(Map(`<=`, a, b), use.names = FALSE)

map_not <- function(a) lapply(a, `!`)
map_not_vec <- function(a) sapply(a, `!`)

# extras
map_nand <- function(a, b) Map(`%!&%`, a, b)
map_nand_vec <- function(a, b) unlist(Map(`%!&%`, a, b), use.names = FALSE)
`%map_nand%` <- function(a, b) Map(`%!&%`, a, b)
`%map_nand_vec%` <- function(a, b) unlist(Map(`%!&%`, a, b), use.names = FALSE)

map_nor  <- function(a, b) Map(`%!|%`, a, b)
map_nor_vec  <- function(a, b) unlist(Map(`%!|%`, a, b), use.names = FALSE)
`%map_nor%`  <- function(a, b) Map(`%!|%`, a, b)
`%map_nor_vec%`  <- function(a, b) unlist(Map(`%!|%`, a, b), use.names = FALSE)

map_all <- function(...) sapply(list(...), all)
map_all_l <- function(l) lapply(l, all)

map_any <- function(...) sapply(list(...), any)
map_any_l <- function(l) lapply(l, any)

map_none <- function(...) sapply(list(...), none)
map_none_l <- function(l) lapply(l, none)


### Map-Reduce --------------------------------------------------------------

reduce_and <- function(...) unlist(Reduce(map_and, l), use.names = FALSE)
reduce_and_l <- function(l) Reduce(map_and, l)

reduce_nand <- function(...) map_not_vec(Reduce(map_and, list(...)))
reduce_nand_l <- function(l) map_not(Reduce(map_and, l))

reduce_or <- function(...) unlist(Reduce(map_or, list(...)), use.names = FALSE)
reduce_or_l <- function(l) Reduce(map_or, l)

reduce_nor <- function(...) map_not_vec(Reduce(map_or, list(...)))
reduce_nor_l <- function(l) map_not(Reduce(map_or, l))

