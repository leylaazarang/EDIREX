# mysplit.R

######
# Function mysplit
# It can be used to apply strsplit to each entry of a character vector and select always the same entry of the result
# It was written to be applied with sapply
# Inputs
# xi = the element to be selected (from 1 to the length of the character vector, typically)
# char.vector = the vector containing the strings to be split
# split.by = the string to be used to split each entry in char.vector
# result.sel = the element after splitting to be selected
# Output
# a single string result after the split
#######
mysplit <- function(xi,char.vector,split.by,result.sel,fixed=FALSE){
  char.vector <- as.character(char.vector)
  mychar <- char.vector[xi]
  # If we want to have all bits of the split as a matrix/array, we can use
  # split.result <- strsplit(mychar,split=split.by)[[result.sel]]
  if(!fixed) {split.result <- strsplit(mychar,split=split.by)[[1]][result.sel]
  split.result} else {
    split.result <- strsplit(mychar,split=split.by,fixed=fixed)[[1]][result.sel]
    split.result
  }
}
