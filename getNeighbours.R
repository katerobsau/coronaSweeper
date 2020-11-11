# A function to get the neighbours
get_neighbours <- function(i, I, J){
  # indexing is by column first then rows 
  row_index = i%%I + I*(i%%I == 0); 
  col_index = ceiling(i/I)
  left_nbr   = i - I + (col_index == 1)*(I*J)
  right_nbr  = i + I - (col_index == J)*(I*J)
  top_nbr    = i - 1 + (row_index == 1)*I
  bottom_nbr = i + 1 - (row_index == I)*I
  # later part of each nbr equation addresses boundary cases
  # the board wraps like on a taurus
  nbrs <- c(left_nbr, right_nbr, top_nbr, bottom_nbr)
  return(nbrs)
}