
## extract p-value cutoff for E[fdf] < q
fdr_cut <- function(pvals, q, plotit=FALSE, ...){
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/N) ])
  
  if(plotit){
    sig <- factor(pvals<=alpha)
    o <- order(pvals)
    plot(pvals[o], col=c("grey60","red")[sig[o]], pch=20, ..., 
       ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
    lines(1:N, q*(1:N)/N)
  }
  
  return(alpha)
}

