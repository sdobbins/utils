# @author Scott Dobbins
# @version 0.9.9.6
# @date 2018-01-08 12:00


### Constants ---------------------------------------------------------------

vowels <- c("a", "e", "i", "o", "u")
vowels_y <- c(vowels, "y")

consonants_y <- letters %d% vowels
consonants <- consonants_y %d% "y"

English_definite_articles <- c("the")
English_indefinite_articles <- c("a", "an")
English_articles <- c(English_definite_articles, English_indefinite_articles)

English_coordinating_conjunctions <- c("and", "but", "for", "or", "so", "yet")
English_subordinating_conjunctions <- c("after", "although", "as", "because", "before", "even", "if", "lest", "nor", "once", "since", "though", "till", "unless", "until", "what", "when", "whenever", "wherever", "whether", "while")
English_conjunctions <- c(English_coordinating_conjunctions, English_subordinating_conjunctions)

English_demonstrative_pronouns <- c("that", "these", "this", "those")
English_indefinite_pronouns <- c("any", "anybody", "anyone", "anything", "none", "nobody", "no one", "nothing", "every", "everybody", "everyone", "everything", "some", "somebody", "someone", "something", "each", "either", "neither", "one", "both", "few", "many", "several", "all", "most")
English_interrogative_pronouns <- c("who", "what", "when", "where", "why", "how")
English_objective_pronouns <- c("me", "you", "him", "her", "it", "them", "us", "you all", "y'all")
English_subjective_pronouns <- c("I", "you", "he", "she", "it", "they", "we", "you all", "y'all")
English_reflexive_pronouns <- c("myself", "yourself", "himself", "herself", "itself", "themself", "ourselves", "yourselves", "themselves")
English_relative_pronouns <- c("here", "there", "that", "which", "whichever", "who", "whom", "whoever", "whomever", "whose")
English_pronouns <- unique(c(English_demonstrative_pronouns, English_indefinite_pronouns, English_interrogative_pronouns, English_objective_pronouns, English_subjective_pronouns, English_reflexive_pronouns, English_relative_pronouns))

English_negations <- c("no", "not")

English_prepositions <- c("above", "around", "at", "away", "behind", "between", "close", "far", "from", "in", "inside", "into", "near", "of", "off", "on", "outside", "to", "top", "toward", "towards", "under", "underneath")

articles <- c(English_articles, "el", "l", "la", "le", "les", "un", "una", "unas", "unos")
conjunctions <- English_conjunctions
negations <- English_negations

English_prepositions_upper <- toupper(English_prepositions)

foreign_prepositions <- list()
foreign_prepositions[["Germany"]] <- c("ab", "am", "an", "ans", "anstatt", "auf", "aufs", "aus", "ausser", "ausserhalb", "bei", "beim", "bis", "diesseits", "durch", "durchs", "entlang", "fur", "furs", "gegen", "gegenuber", "hinter", "innerhalb", "jenseits", "mit", "nach", "neben", "im", "in", "ins", "ohne", "seit", "statt", "trotz", "uber", "ubers", "um", "ums", "umweit", "unter", "unters", "vom", "von", "vor", "vorm", "wegen", "wehrend", "zu", "zum", "zur", "zwischen")
foreign_prepositions[["France"]] <- c("a", "apres", "au", "aupres", "aux", "avant", "avec", "chez", "contre", "d", "dans", "de", "depuis", "derriere", "des", "devant", "du", "en", "entre", "envers", "environment", "hors", "jusqu", "jusque", "malgre", "moins", "moyennant", "par", "parmi", "pendant", "plus", "pour", "sans", "sauf", "selon", "sous", "sur", "vers", "versus", "via")
foreign_prepositions[["Italy"]] <- c("a", "accanto", "ad", "al", "all", "attraverso", "come", "con", "contro", "da", "dentro", "di", "dietro", "dopo", "durante", "eccetto", "fino", "fuori", "in", "intorno", "giu", "la", "ma", "nonostante", "ogni", "opposto", "per", "piu", "poi", "poiche", "prima", "prossimo", "quando", "riguardo", "senza", "sopra", "sotto", "su", "tra", "tramite", "verso", "vicino")
prepositions <- sort(unique(c(English_prepositions, unlist(foreign_prepositions, use.names = FALSE))))

stop_words <- unique(c(articles, conjunctions, negations, prepositions))

measurement_units <- c("km", "m", "cm", "mm", "mi", "in", "ft", "nm", "kg", "lb")
measurement_units_upper <- toupper(measurement_units)

ordinal_markers <- c("nd", "rd", "th") # "st" (also an abbreviation for saint/station/street)
roman_numerals <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII")
cardinal_directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

lower_case_set <- c(stop_words, measurement_units, ordinal_markers)
lower_case_set_upper <- toupper(lower_case_set)


### Helpers ----------------------------------------------------------------

is_na_or_empty <- function(strings) {
  return (is.na(strings) | strings == "")
}


### Matchers ---------------------------------------------------------------

# endsWithWord <- function(strings, ending) {
#   pos <- -(nchar(ending) + 1L)
#   return (strings == ending | (endsWith(strings, ending) & !(stri_sub(strings, from = pos, to = pos) %in% letters)))
# }

endsWithAny <- function(strings, endings) {
  results <- endsWith(strings, endings[[1]])
  for (i in seq_along(endings)[-1L]) {
    results <- results | endsWith(strings, endings[[i]])
  }
  return (results)
}

# startsWithWord <- function(strings, beginning) {
#   pos <- nchar(beginning) + 1L
#   return (strings == beginning | (startsWith(strings, beginning) & !(stri_sub(strings, from = pos, to = pos) %in% letters)))
# }

startsWithAny <- function(strings, beginnings) {
  results <- startsWith(strings, beginnings[[1]])
  for (i in seq_along(beginnings)[-1L]) {
    results <- results | startsWith(strings, beginnings[[i]])
  }
  return (results)
}



### Substrings -------------------------------------------------------------

if (is_package_loaded("stringi")) {
  first_n_chars <- function(strings, n) {
    return (stri_sub(strings, to = n))
  }
  
  last_n_chars <- function(strings, n) {
    return (stri_sub(strings, from = -n))
  }
  
  remove_first_n_chars <- function(strings, n) {
    return (stri_sub(strings, from = 1L + n))
  }
  
  remove_last_n_chars <- function(strings, n) {
    return (stri_sub(strings, to = -(n + 1L)))
  }
  
  remove_beginning <- function(strings, beginning) {
    return (stri_sub(strings, from = 1L + nchar(beginning)))
  }
  
  remove_ending <- function(strings, ending) {
    return (stri_sub(strings, to = -nchar(ending)))
  }
} else {
  first_n_chars <- function(strings, n) {
    # assert_that(all(n > 0L), 
    #             msg = "Ask for more than 0 characters")
    return (substr(strings, start = 1L, stop = n))
  }
  
  last_n_chars <- function(strings, n) {
    # assert_that(all(n > 0L), 
    #             msg = "Ask for more than 0 characters")
    string_lengths = nchar(strings)
    return (substr(strings, start = string_lengths - n + 1L, stop = string_lengths))
  }
  
  remove_first_n_chars <- function(strings, n) {
    # assert_that(all(n >= 0L), 
    #             msg = "Can't remove less than 0 characters")
    return (substr(strings, start = 1L + n, stop = nchar(strings)))
  }
  
  remove_last_n_chars <- function(strings, n) {
    # assert_that(all(n >= 0L), 
    #             msg = "Can't remove less than 0 characters")
    return (substr(strings, start = 1L, stop = nchar(strings) - n))
  }
  
  remove_beginning <- function(strings, beginning) {
    return (substr(strings, start = 1L + nchar(beginning), stop = nchar(strings)))
  }
  
  remove_ending <- function(strings, ending) {
    return (substr(strings, start = 1L, stop = nchar(strings) - nchar(ending)))
  }
}

replace_first_n_chars_with <- function(strings, n, replacements) {
  return (paste0(replacements, remove_first_n_chars(strings, n)))
}

replace_last_n_chars_with <- function(strings, n, replacements) {
  return (paste0(remove_last_n_chars(strings, n), replacements))
}

replace_beginning_with <- function(strings, beginning, replacements) {
  return (paste0(replacements, remove_beginning(strings, beginning)))
}

replace_ending_with <- function(strings, ending, replacements) {
  return (paste0(remove_ending(strings, ending), replacements))
}


### Capitalizers -----------------------------------------------------------

### Normal capitalization

capitalize <- function(words) {
  return (paste0(toupper(substring(words, 1L, 1L)), tolower(substring(words, 2L))))
}

capitalize_first_letters <- function(words) {
  return (paste0(toupper(substring(words, 1L, 1L)), substring(words, 2L)))
}

capitalize_phrase <- function(line) {
  return (paste(capitalize(strsplit(line, split = " ", fixed = TRUE)[[1]]), collapse = " "))
}

capitalize_phrases <- function(lines) {
  lines_mod <- if_else(lines == "", "`", tolower(lines))
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  lines_mod <- split(proper_nouns(unlist(lines_mod, use.names = FALSE)), rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- sapply(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", lines_mod))
}


### Proper Noun Functions ---------------------------------------------------

proper_noun <- function(word) {
  if (word %in% upper_case_set_lower) {
    return (toupper(word))
  } else if (word %in% lower_case_set) {
    return (word)
  } else {
    return (capitalize(word))
  }
}

proper_nouns <- function(words) {
  upper_case_words <- words %in% upper_case_set_lower
  capitalize_words <- !(upper_case_words | words %in% lower_case_set)
  words[upper_case_words] <- toupper(words[upper_case_words])
  words[capitalize_words] <- capitalize(words[capitalize_words])
  return (words)
}

proper_noun_phrase <- function(line) {
  line <- provide_buffers_around(tolower(line), chars = "[-/(]")
  line <- paste0(proper_nouns(strsplit(line, split = " ", fixed = TRUE)[[1]]), collapse = " ")
  return (remove_buffers_around(line, chars = "[-/(]"))
}

proper_noun_phrases <- function(lines) {
  lines_mod <- if_else(lines == "", "`", provide_buffers_around(tolower(lines), chars = "[-/(]"))
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  lines_mod <- split(proper_nouns(unlist(lines_mod, use.names = FALSE)), rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- sapply(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", remove_buffers_around(lines_mod, chars = "[-/(]")))
}


### Numbers as strings ------------------------------------------------------

### Comma adders

commas_string <- function(number) {
  if (is.finite(number)) {
    abs_number <- abs(number)
    if (abs_number > 1) {
      num_groups <- log(abs_number, base = 1000)
    } else {
      num_groups <- 0
    }
    
    if (num_groups < 1) {
      return (as.character(number))
    } else {
      num_rounds <- floor(num_groups)
      output_string <- ""
      
      for (round in 1:num_rounds) {
        this_group_int <- abs_number %% 1000
        if(this_group_int < 10) {
          output_string <- paste0(",00", as.character(this_group_int), output_string)
        } else if(this_group_int < 100) {
          output_string <- paste0(",0", as.character(this_group_int), output_string)
        } else {
          output_string <- paste0(",", as.character(this_group_int), output_string)
        }
        abs_number <- abs_number %/% 1000
      }
      
      if (number < 0) {
        return (paste0("-", as.character(abs_number), output_string))
      } else {
        return (paste0(as.character(abs_number), output_string))
      }
    }
  } else {
    return (NA_character_)
  }
}

# note: this assumes non-negative finite integers as inputs
commas_strings <- function(numbers) {
  numbers_strings <- as.character(numbers)
  
  nums_digits <- if_else(numbers < 10, 1, ceiling(log10(numbers)))
  max_digits <- max(nums_digits, na.rm = TRUE)
  num_rounds <- ceiling(max_digits / 3) - 1L
  
  head_lengths <- 3L - (-nums_digits %% 3L)
  tail_positions <- head_lengths + 1L
  results <- substr(numbers_strings, 1L, head_lengths)
  
  for (round in seq(num_rounds)) {
    needs_more <- nums_digits > (3L * round)
    results[needs_more] <- paste0(results[needs_more], ",", substr(numbers_strings[needs_more], tail_positions[needs_more] + (3L * (round - 1L)), tail_positions[needs_more] + (3L * round) - 1L))
  }
  return (results)
}


### Modifications -----------------------------------------------------------

### Articles

fix_article <- function(string, invariate_plurals) {
  l <- nchar(string)
  if (endsWith(string, "s") | startsWith(string, "a ") | string %in% invariate_plurals) {
    return (string)
  } else if (substr(string, 1, 1) %in% vowels) {
    return (paste("an", string))
  } else {
    return (paste("a", string))
  }
}

fix_articles <- function(strings, invariate_plurals) {
  lengths <- nchar(strings)
  needs_article <- !(endsWith(strings, "s") | 
    startsWith(strings, "a ") | 
    strings %in% invariate_plurals)
  starts_with_vowel <- substr(strings, 1, 1) %in% vowels
  strings[needs_article & starts_with_vowel] <- paste("an", strings[needs_article & starts_with_vowel])
  strings[needs_article & !starts_with_vowel] <- paste("a", strings[needs_article & !starts_with_vowel])
  return (strings)
}


### Dictionary Functions ----------------------------------------------------

#dictionary_10k <- read.csv('../words/10k most common English words.txt', header = FALSE, stringsAsFactors = FALSE)[["V1"]]
dictionary_20k <- read.csv('~/Developer/RStudio/words/20k most common English words.txt', header = FALSE, stringsAsFactors = FALSE)[["V1"]]

are_valid_words <- function(words, dictionary) {
  return (words %in% dictionary)
}

are_valid_word_phrases <- function(lines, dictionary) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- strsplit(lines_mod, split = "[, ()/-]")
  lines_mod <- split(are_valid_words(unlist(lines_mod, use.names = FALSE), dictionary), rep(1:length(lines), lengths(lines_mod)))
  lines_reduced <- sapply(lines_mod, all)
  return (if_else(lines == "", TRUE, lines_reduced))
}

get_invalid_words <- function(lines, dictionary) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- strsplit(lines_mod, split = "[, ()/-]")
  combined_words <- unlist(lines_mod, use.names = FALSE)
  combined_invalid_words <- if_else(are_valid_words(combined_words, dictionary), "", combined_words)
  lines_mod <- split(combined_invalid_words, rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- sapply(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", lines_mod))
}

filter_out_invalid_words <- function(lines, dictionary, exclude = character(0)) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- gsub(pattern = "([,()/-])", replacement = " \\1 ", lines_mod)
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  combined_words <- unlist(lines_mod, use.names = FALSE)
  is_valid_word <- are_valid_words(combined_words, dictionary) | combined_words %like% "[,()/-]"
  is_valid_word <- is_valid_word & combined_words %!in% exclude
  combined_valid_words <- if_else(is_valid_word, combined_words, "")
  lines_mod <- split(combined_valid_words, rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- trimws(gsubs(changes = c("\\1" = " ([,()/-]) ", 
                                        "-?\\b[a-z]\\b-?", 
                                        " "   = " +"), 
                            sapply(lines_mod, paste0, collapse = " ")))
  return (if_else(lines == "", "", lines_mod))
}

#*** fix num_lines <- length(lines) and other similar stuff here
fix_invalid_words <- function(lines, dictionary, exclude = character(0)) {
  lines_mod <- if_else(lines == "", "`", lines) %>% 
    remove_duplicates(duplicate = ",", exact = TRUE) %>% # for safety
    provide_buffers_around("[,()/-]") %>% # to conserve punctuation (will re-delete spaces at end)
    gsub(pattern = "&", replacement = " and ", fixed = TRUE) %>% # for safety; make all &s ands
    fix_spaces
  
  num_lines <- length(lines)
  
  split_result <- strsplit(lines_mod, split = " ", fixed = TRUE)
  split_lengths <- lengths(split_result)
  combined_words <- unlist(split_result, use.names = FALSE)
  is_valid_word <- are_valid_words(combined_words, dictionary) | combined_words %like% "[,()/-]" | combined_words == ""
  is_valid_word <- is_valid_word & combined_words %!in% exclude
  is_invalid_word <- !is_valid_word
  is_and <- combined_words == "and"
  is_comma <- combined_words == ","
  split_sizes <- rep(1:num_lines, split_lengths)
  
  is_valid_word_column <- split(is_valid_word, split_sizes)
  is_invalid_word_column <- split(is_invalid_word, split_sizes)
  is_and_column <- split(is_and, split_sizes)
  is_comma_column <- split(is_comma, split_sizes)
  
  is_previous_comma <- is_comma_column %map_and% lead(is_invalid_word_column, 1L, fill = FALSE)
  is_twice_previous_comma <- is_comma_column %map_and% lead(is_invalid_word_column, 2L, fill = FALSE)
  is_previous_and <- is_and_column %map_and% lead(is_invalid_word_column, 1L, fill = FALSE)
  is_comma_before_previous_and <- is_twice_previous_comma %map_and% lead(is_previous_and, 1L, fill = FALSE)
  
  is_subsequent_and <- is_and_column %map_and% lag(is_invalid_word_column, 1L, fill = FALSE)
  is_subsequent_comma <- is_comma_column %map_and% lag(is_invalid_word_column, 1L, fill = FALSE)
  is_subsequent_bridge <- is_subsequent_and %map_or% is_subsequent_comma
  
  is_previous_bridge_word <- is_previous_comma %map_or% is_previous_and
  is_previous_removed <- lag(is_previous_bridge_word, 1L, fill = FALSE)
  is_previous_not_removed <- map_not(is_previous_removed)
  is_subsequent_bridge_and_previous_not_removed <- is_subsequent_bridge %map_and% lag(is_previous_not_removed, 1L, fill = FALSE)
  
  is_removable <- reduce_or_l(list(is_invalid_word_column, is_previous_bridge_word, is_comma_before_previous_and, is_subsequent_bridge_and_previous_not_removed))
  is_keepable <- map_not(is_removable)
  
  is_remaining_before_comma_and <- is_keepable %map_and% lead(is_comma_before_previous_and, 1L, fill = FALSE)
  is_and_needing_saving <- lag(is_remaining_before_comma_and, 2L, fill = FALSE)
  which_is_and_twice_after_remaining_before_comma_and <- map_which(is_and_needing_saving)
  which_is_comma <- map_which(is_comma_column)
  which_is_remaining_before_comma_and <- map_which(is_remaining_before_comma_and)
  which_is_remaining_after_comma_yet_before_comma_and <- map2(which_is_comma, which_is_remaining_before_comma_and, ~quiet(closest_to, .x, .y, but_less_than = TRUE, sorted = TRUE)) %map_plus% 1L
  
  reinsertion_necessary <- !sapply(which_is_remaining_after_comma_yet_before_comma_and, is_empty)
  
  is_keepable <- is_keepable %map_or% is_and_needing_saving
  is_removable <- map_not(is_keepable)
  
  is_keepable_comma <- is_keepable %map_and% is_comma_column
  which_is_keepable_comma <- map_which(is_keepable_comma)
  are_commas_unneeded <- lengths(which_is_keepable_comma) < 2L
  is_removable_comma <- is_comma_column %map_and% are_commas_unneeded
  
  is_removable <- is_removable %map_or% is_removable_comma
  
  is_removable_vector <- unlist(is_removable, use.names = FALSE)
  combined_words[is_removable_vector] <- ""
  
  filtered_lines <- split(combined_words, split_sizes)
  
  filtered_lines <- pmap(list(filtered_lines, 
                              reinsertion_necessary, 
                              which_is_and_twice_after_remaining_before_comma_and, 
                              which_is_remaining_after_comma_yet_before_comma_and), 
                         ~(if.else(..2, remove_and_reinsert(..1, from = ..3, at = ..4), ..1)))
  
  filtered_lines_reduced <- sapply(filtered_lines, paste0, collapse = " ") %>% 
    remove_buffers_around("[()/-]") %>% 
    format_commas %>% 
    remove_single_letters(with_punctuation = "[()/-]") %>% 
    remove_hanging_punctuation("[()/-]") %>% 
    grems(patterns = c(ending_with(as_many_of(word(any_of(stop_words)))), 
                       beginning_with(word(any_of(English_prepositions))))) %>% 
    remove_duplicates(c(",", "/")) %>% 
    fix_spaces
  
  return (if_else(lines == "", "", filtered_lines_reduced))
}


### Anagrams ----------------------------------------------------------------

is_anagram <- function(word1, word2) {
  if (nchar(word1) == nchar(word2)) {
    letters1 <- strsplit(word1, "")[[1]]
    letters2 <- strsplit(word2, "")[[1]]
    dict1 <- list()
    dict2 <- list()
    for (i in seq_along(letters1)) {
      l1 <- letters1[[i]]
      l2 <- letters2[[i]]
      dict1[l1] <- (dict1[[l1]] %||% 0L) + 1L
      dict2[l2] <- (dict2[[l2]] %||% 0L) + 1L
    }
    return (unlist(dict1) %e% unlist(dict2))
  } else {
    return (FALSE)
  }
}

find_anagrams <- function(word, dictionary) {
  dict <- dictionary[nchar(dictionary) == nchar(word)]
  word_letters <- strsplit(word, "")[[1]]
  present_letters <- unique(word_letters)
  absent_letters <- letters %d% present_letters
  dict_letters <- strsplit(dict, "")
  
  dict_slicer <- map_lgl(dict_letters, ~(none(absent_letters %in% .)))
  dict <- dict[dict_slicer]
  dict_letters <- dict_letters[dict_slicer]
  
  dict_slicer <- map_lgl(dict_letters, ~(all(present_letters %in% .)))
  dict <- dict[dict_slicer]
  dict_letters <- dict_letters[dict_slicer]
  
  for (letter in word_letters) { #* technically could stop at length - 1L here because that's all the degrees of freedom it has
    letter_count <- sum(word_letters == letter)
    dict_slicer <- map_lgl(dict_letters, ~(sum(. == letter) == letter_count))
    dict <- dict[dict_slicer]
    dict_letters <- dict_letters[dict_slicer]
  }
  
  return (dict %[!=]% word)
}

