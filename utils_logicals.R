# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Extras ------------------------------------------------------------------

`%><%` <- function(a, b) xor(a, b)
`%!&%` <- function(a, b) !(a & b)
`%!|%` <- function(a, b) !(a | b)
none <- function(...) !any(...)


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
`%map_and%` <- function(a, b) purrr::map2(a, b, `&`)
`%map_and_vec%` <- function(a, b) purrr::map2_lgl(a, b, `&`)

`%map_or%`  <- function(a, b) purrr::map2(a, b, `|`)
`%map_or_vec%`  <- function(a, b) purrr::map2_lgl(a, b, `|`)

`%map_e%` <- function(a, b) purrr::map2(a, b, `==`)
`%map_e_vec%` <- function(a, b) purrr::map2_lgl(a, b, `==`)

`%map_ne%` <- function(a, b) purrr::map2(a, b, `!=`)
`%map_ne_vec%` <- function(a, b) purrr::map2_lgl(a, b, `!=`)

`%map_gt%` <- function(a, b) purrr::map2(a, b, `>`)
`%map_gt_vec%` <- function(a, b) purrr::map2_lgl(a, b, `>`)

`%map_gte%` <- function(a, b) purrr::map2(a, b, `>=`)
`%map_gte_vec%` <- function(a, b) purrr::map2_lgl(a, b, `>=`)

`%map_lt%` <- function(a, b) purrr::map2(a, b, `<`)
`%map_lt_vec%` <- function(a, b) purrr::map2_lgl(a, b, `<`)

`%map_lte%` <- function(a, b) purrr::map2(a, b, `<=`)
`%map_lte_vec%` <- function(a, b) purrr::map2_lgl(a, b, `<=`)

map_not <- function(a) sapply(a, `!`)

# extras
`%map_nand%` <- function(a, b) purrr::map2(a, b, `%!&%`)
`%map_nand_vec%` <- function(a, b) purrr::map2_lgl(a, b, `%!&%`)

`%map_nor%`  <- function(a, b) purrr::map2(a, b, `%!|%`)
`%map_or_vec%`  <- function(a, b) purrr::map2_lgl(a, b, `%!|%`)

map_all <- function(...) sapply(list(...), all)
map_all_l <- function(l) lapply(l, all)

map_any <- function(...) sapply(list(...), any)
map_any_l <- function(l) lapply(l, any)

map_none <- function(...) sapply(list(...), none)
map_none_l <- function(l) lapply(l, none)


### Map-Reduce --------------------------------------------------------------

reduce_and <- function(...) unlist(purrr::reduce(list(...), `%map_and%`), use.names = FALSE)
reduce_and_l <- function(l) reduce(l, `%map_and%`)

reduce_nand <- function(...) unlist(map_not(purrr::reduce(list(...), `%map_and%`)), use.names = FALSE)
reduce_nand_l <- function(l) map_not(reduce(l, `%map_and%`))

reduce_or <- function(...) unlist(purrr::reduce(list(...), `%map_or%`), use.names = FALSE)
reduce_or_l <- function(l) reduce(l, `%map_or%`)

reduce_nor <- function(...) unlist(map_not(purrr::reduce(list(...), `%map_or%`)), use.names = FALSE)
reduce_nor_l <- function(l) map_not(reduce(l, `%map_or%`))
