"Xrow_to_Perl" <- function(x, row=1, file="", comment=NA) {
  x <- as.data.frame(x)
  nm <- names(x)
  rw <- x[1,]

  if(! is.na(comment)) {
    cat(paste0("# ",comment,"\n"), file=file)
  }
  cat("  $X = {\n", file=file)
  for(i in 1:(length(nm)-1)) {
    if(is.na(rw[i])) rw[i] <- '"infinity"'
    cat(paste0("   ",nm[i], " => ", rw[i],",\n"), file=file)
  }
  cat(paste0("   ",nm[length(nm)], " => ",
                   rw[length(nm)],"};\n"), file=file)
}
