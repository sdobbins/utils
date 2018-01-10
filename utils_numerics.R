# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Types -------------------------------------------------------------------

is_int <- function(double, eps = 1e-9) {
  return (near(double, round_to_int(double), eps))
}

round_to_int <- function(numbers, digits = 0L) {
  return (as.integer(round(numbers, digits)))
}

is_NA_or_0L <- function(ints) {
  if (!is.numeric(ints)) {
    stop("This method is for integers; you supplied some non-numeric type")
  }
  if (is.double(ints)) {
    warning("This method is for integers; you supplied doubles")
  }
  return (is.na(ints) | ints == 0L)
}

is_NA_or_0 <- function(dbls, tol = NULL) {
  if (!is.numeric(dbls)) {
    stop("This method is for doubles; you supplied some non-numeric type")
  }
  if (is.integer(dbls)) {
    warning("This method is for doubles; you supplied integers")
  }
  if (is.null(tol)) {
    return (is.na(dbls) | dbls == 0)
  } else {
    return (is.na(dbls) | near(dbls, 0, tol = tol))
  }
}


### Statistics --------------------------------------------------------------

mode_num <- function(nums) {
  unique_nums <- unique(nums)
  return (unique_nums[which.max(sapply(unique_nums, function(x) sum(x == nums)))])
}

mode_and_others_num <- function(nums) {
  unique_nums <- unique(nums)
  tabs <- mode_num(nums)
  max_index <- which.max(tabs)
  mode_level <- unique_nums[max_index]
  non_mode_levels <- unique_nums[-max_index]
  return (list("mode" = mode_level, "others" = non_mode_levels))
}


### Rankings ----------------------------------------------------------------

ranking <- function(dt, sort_col, ref_col, ref, descending = FALSE, ties = TRUE) {
  if (is.na(ref)) {
    return (NA_integer_)
  } else {
    which_is_ref <- which(dt[[ref_col]] == ref)
    if (is_empty(which_is_ref)) {
      return (NA_integer_)
    } else {
      if (descending) { # higher/later is better
        return (min(frankv(dt, cols = sort_col, order = -1L, na.last = NA, ties.method = 'min')[which_is_ref]))
      } else { # lower/earlier is better
        return (min(frankv(dt, cols = sort_col, order = 1L, na.last = NA, ties.method = 'min')[which_is_ref]))
      }
    }
  }
}

rankings <- function(dt, sort_col, ref_col, refs, descending = FALSE, ties = TRUE) {
  results <- rep(NA_integer_, length(refs))
  are_present <- dt[[ref_col]] %in% refs
  if (descending) { # higher/later is better
    results[are_present] <- frankv(dt, cols = sort_col, order = -1L, na.last = NA, ties.method = 'min')[which(are_present)]
  } else { # lower/earlier is better
    results[are_present] <- frankv(dt, cols = sort_col, order = 1L, na.last = NA, ties.method = 'min')[which(are_present)]
  }
  return (results)
}

# only sends back one answer (always)
which_has_ranking <- function(dt, sort_col, ref_col, rank, descending = FALSE, ties = TRUE) {
  if (is.na(rank)) {
    return (NA_integer_)
  } else {
    if (descending) { # higher/later is better
      return (dt[[ref_col]][max(which(frankv(dt, cols = sort_col, order = -1L, na.last = NA, ties.method = 'min') == rank))])
    } else { # lower/earlier is better
      return (dt[[ref_col]][min(which(frankv(dt, cols = sort_col, order = 1L, na.last = NA, ties.method = 'min') == rank))])
    }
  }
}

# sends back multiple answers, even duplicate rows will all show up
which_have_rankings <- function(dt, sort_col, ref_col, ranks, descending = FALSE, ties = TRUE) {#*** make sure ranks are in order?
  if (descending) { # higher/later is better
    return (dt[[ref_col]][which(frankv(dt, cols = sort_col, order = -1L, na.last = NA, ties.method = 'min') %in% ranks)])
  } else { # lower/earlier is better
    return (dt[[ref_col]][which(frankv(dt, cols = sort_col, order = 1L, na.last = NA, ties.method = 'min') %in% ranks)])
  }
}

top_n_ranks <- function(dt, sort_col, ref_col, n, descending = FALSE, ties = TRUE) {#*** make sure ranks are in order
  assert_that(!is.na(n), 
              msg = "Can't find top <NA> ranks.")
  if (descending) { # higher/later is better
    return (dt[[ref_col]][which(frankv(dt, cols = sort_col, order = -1L, na.last = NA, ties.method = 'min') <= n)])
  } else { # lower/earlier is better
    return (dt[[ref_col]][which(frankv(dt, cols = sort_col, order = 1L, na.last = NA, ties.method = 'min') <= n)])
  }
}

  

### Distance ----------------------------------------------------------------

distance <- function(numbers, ref) {
  return (abs(numbers - ref))
}

distance_2D <- function(numbers_x, numbers_y, ref) {
  x <- ref[[1]]
  y <- ref[[2]]
  return (sqrt(((numbers_x - x) ** 2) + ((numbers_y - y) ** 2)))
}

distance_3D <- function(numbers_x, numbers_y, numbers_z, ref) {
  x <- ref[[1]]
  y <- ref[[2]]
  z <- ref[[3]]
  return (sqrt(((numbers_x - x) ** 2) + ((numbers_y - y) ** 2) + ((numbers_z - z) ** 2)))
}

bounded <- function(number, lower, upper) {
  if (number > upper) {
    return (upper)
  } else if (number < lower) {
    return (lower)
  } else {
    return (number)
  }
}

if (is_package_installed("dplyr")) {
  # best version of this function using the faster dplyr::if_else
  bounded_vectorized <- function(numbers, lowers, uppers) {
    return (dplyr::if_else(numbers > uppers, uppers, dplyr::if_else(numbers < lowers, lowers, numbers)))
  }
} else {
  # if dplyr isn't loaded, then this function is still supported with base::ifelse
  bounded_vectorized <- function(numbers, lowers, uppers) {
    return (ifelse(numbers > uppers, uppers, ifelse(numbers < lowers, lowers, numbers)))
  }
}

buffer <- function(numbers, buf, lower_first = TRUE) {
  if (lower_first) {
    return (c(numbers - buf, numbers + buf))
  } else {
    return (c(numbers + buf, numbers - buf))
  }
}

close_to <- function(numbers, ref, buf) {
  return (abs(numbers - ref) <= buf)
}

close_to_2D <- function(numbers_x, numbers_y, ref, buf) {
  x_length <- length(numbers_x)
  assert_that(x_length == length(numbers_y), 
              msg = "You provided vectors unequal in length.")
  x <- ref[[1]]
  y <- ref[[2]]
  if (x_length > 1000L) {
    is_close <- close_to(numbers_x, x, buf)
    is_close[is_close] <- close_to(numbers_y[is_close], ref, buf)
    is_close[is_close] <- sqrt(((numbers_x[is_close] - x) ** 2) + ((numbers_y[is_close] - y) ** 2)) <= buf
    return (is_close)
  } else {
    return (sqrt(((numbers_x - x) ** 2) + ((numbers_y - y) ** 2)) <= buf)
  }
}

close_to_3D <- function(numbers_x, numbers_y, numbers_z, ref, buf) {
  x_length <- length(numbers_x)
  assert_that(x_length == length(numbers_y) && x_length == length(numbers_z), 
              msg = "You provided vectors unequal in length.")
  x <- ref[[1]]
  y <- ref[[2]]
  z <- ref[[3]]
  if (x_length > 1000L) {
    is_close <- close_to(numbers_x, x, buf)
    is_close[is_close] <- close_to(numbers_y[is_close], y, buf)
    is_close[is_close] <- close_to(numbers_z[is_close], z, buf)
    is_close[is_close] <- sqrt(((numbers_x[is_close] - x) ** 2) + ((numbers_y[is_close] - y) ** 2) + ((numbers_z[is_close] - z) ** 2)) <= buf
    return (is_close)
  } else {
    return (sqrt(((numbers_x - x) ** 2) + ((numbers_y - y) ** 2) + ((numbers_z - z) ** 2)) <= buf)
  }
}

halfway_int <- function(lower, upper, round_up = FALSE) {
  if (round_up) {
    return (round_to_int(ceiling((upper - lower) / 2) + lower))
  } else {
    return (round_to_int(floor((upper - lower) / 2) + lower))
  }
}

closest_to <- function(numbers, ref, but_less_than = FALSE, but_greater_than = FALSE, or_equal_to = FALSE, sorted = FALSE, descending = FALSE) {
  # currently only ascending mode is implemented
  if (is.null(ref) || is.null(numbers) || is_empty(ref) || is_empty(numbers)) {
    message("The numbers and/or reference are null or empty. Returning NULL.")
    return (NULL)
  }
  if (but_less_than && but_greater_than) {
    if (or_equal_to) {
      message("You asked for a number both (less than or equal to) *and* (greater than or equal to) your reference number. This is asking for numbers that equal your reference number.")
      return (which(numbers == ref))
    } else {
      message("You asked for a number both less than and greater than (but not equal to) your reference number. Returning empty result (or NA if ref was NA).")
      return (ref %[!=]% ref)
    }
  } else if (is.na(ref)) {
    message("You provided a NA reference. Returning NA.")
    return (NA_of_same_type(ref))
  } else {
    if (sorted) {
      upper_index <- length(numbers)
      if (upper_index == 0L) {
        message("Can't find the number closest to an empty set (numbers is empty). Returning empty result.")
        return (ref %[!=]% ref)
      } else if (upper_index == 1L) {
        if (numbers[[upper_index]] > ref) {
          if (or_less_than) {
            return (ref %[!=]% ref)
          } else {
            return (numbers[[upper_index]])
          }
        } else if (numbers[[upper_index]] < ref) {
          if (or_greater_than) {
            return (ref %[!=]% ref)
          } else {
            return (numbers[[upper_index]])
          }
        } else {
          if (or_equal_to) {
            return (numbers[[upper_index]])
          } else {
            return (ref %[!=]% ref)
          }
        }
      } else {
        lower_index <- 1L
        halfway_index <- halfway_int(lower_index, upper_index)
        halfway_number <- numbers[[halfway_index]]
        if (descending) {
          
        } else {# ascending
          if (but_less_than) {
            if (numbers[[upper_index]] <= ref) {
              if (numbers[[upper_index]] == ref) {
                if (or_equal_to) {
                  return (numbers[[upper_index]])
                } else {
                  return (numbers[[upper_index-1L]])
                }
              } else {
                return (numbers[[upper_index]])
              }
            } else if (numbers[[lower_index]] >= ref) {
              if (numbers[[lower_index]] == ref) {
                if (or_equal_to) {
                  return (numbers[[lower_index]])
                } else {
                  return (ref %[!=]% ref)
                }
              } else {
                return (ref %[!=]% ref)
              }
            } else {
              while (upper_index - lower_index > 2L && halfway_number != ref) {
                if (halfway_number > ref) {
                  upper_index <- halfway_index - 1L
                } else {
                  lower_index <- halfway_index
                }
                halfway_index <- halfway_int(lower_index, upper_index, round_up = TRUE)
                halfway_number <- numbers[[halfway_index]]
              }
              if (halfway_number > ref) {#*** still feel like this section needs work
                if (or_equal_to) {
                  return (numbers[[lower_index]] %[<=]% ref)
                } else {
                  return (numbers[[lower_index]] %[<]% ref)
                }
              } else if (numbers[[upper_index]] <= ref) {#*** still feel like this section needs work
                if (or_equal_to) {
                  return (numbers[[upper_index]] %[<=]% ref)
                } else {
                  return (numbers[[upper_index]] %[<]% ref)
                }
              }
            }
          } else {# but_greater_than = TRUE
            if (numbers[[upper_index]] <= ref) {
              if (numbers[[upper_index]] == ref) {
                if (or_equal_to) {
                  return (numbers[[upper_index]])
                } else {
                  return (ref %[!=]% ref)
                }
              } else {
                return (numbers[[upper_index]])
              }
            } else if (numbers[[lower_index]] >= ref) {
              if (numbers[[lower_index]] == ref) {
                if (or_equal_to) {
                  return (numbers[[lower_index]])
                } else {
                  return (numbers[[lower_index + 1L]])
                }
              } else {
                return (numbers[[lower_index]])
              }
            } else {
              while (upper_index - lower_index > 2L && halfway_number != ref) {
                if (halfway_number > ref) {
                  upper_index <- halfway_index
                } else {
                  lower_index <- halfway_index + 1L
                }
                halfway_index <- halfway_int(lower_index, upper_index, round_up = FALSE)
                halfway_number <- numbers[[halfway_index]]
              }
              if (halfway_number < ref) {#*** still feel like this section needs work
                if (or_equal_to) {
                  return (numbers[[upper_index]] %[>=]% ref)
                } else {
                  return (numbers[[upper_index]] %[>]% ref)
                }
              } else if (numbers[[lower_index]] <= ref) {#*** still feel like this section needs work
                if (or_equal_to) {
                  return (numbers[[lower_index]] %[>=]% ref)
                } else {
                  return (numbers[[lower_index]] %[>]% ref)
                }
              }
            }
          }
        }
        return (halfway_number)
      }
    } else {
      if (!or_equal_to) {
        numbers <- numbers %[!=]% ref
      }
      if (but_less_than) {
        numbers <- numbers %[<]% ref
      }
      if (but_greater_than) {
        numbers <- numbers %[>]% ref
      }
      return (numbers[[which.min(numbers - ref)]])
    }
  }
}
