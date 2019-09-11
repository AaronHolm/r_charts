get_scale <- function (data_col){
  val_max = max(data_col)
  scale_num = nchar(as.integer(val_max))
  if(scale_num == 5 | scale_num == 4){
    start_num = as.integer(substr(as.character(as.integer(val_max)), 1, 2))
    scale_max = 10^(scale_num-2)*(start_num)
  }
  else{
    start_num = as.integer(substr(as.character(as.integer(val_max)), 1, 1))
    scale_max = 10^(scale_num-1)*(start_num)
  }
  scale_min = scale_max / 5
  #scale_level = 10^(scale_num-1)
  scale_level = scale_max / 5
  print(paste(scale_min, scale_max, scale_level))
  return (c(scale_min, scale_max, scale_level))
}
