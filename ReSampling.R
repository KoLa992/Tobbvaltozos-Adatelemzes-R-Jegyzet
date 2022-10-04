DrawSamples <- function(current_data, sample_size, number_of_samples, random_seed) {
  set.seed(random_seed)
  Samples <- sample(current_data, size = sample_size, replace = TRUE, )
  
  for (i in 2:number_of_samples) {
    set.seed(random_seed+i-1)
    Samples <- rbind(Samples, sample(current_data, size = sample_size, replace = TRUE, ))
  }
  
  Samples <- as.data.frame(Samples)
  
  rownames(Samples) <- paste0("Sample",1:number_of_samples)
  colnames(Samples) <- paste0("Element",1:sample_size)
  
  return(Samples)
}
