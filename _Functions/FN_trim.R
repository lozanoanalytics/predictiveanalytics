FN_trim <- function(x){
  # removes the leading and trailing spaces from the character variable x
  # x <- " b c "
  return(gsub('^\\s+|\\s+$','',x))
  # translation:
  # look at the beginning (^) for a space (\s but need to be \\s)
  # OR (|) at the end ($) for a space
  # and replace with '' (this is part of "gsub")
}

# FN_trim('  b c  ')
