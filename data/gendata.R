#
# Generate p by (m*n) matrix and save to a file
#
#    m : number of elements per bin
#    n : number of bins
#    p : number of rows
#
gen <- function(m, n, p) {
    X <- matrix(c(rep(1:n, each=m), sample(m*n*p)), nrow=p+1, byrow=TRUE)
    write.table(X, file=paste('m', m, 'n', n, 'p', p, '.txt', sep=''), row.names=FALSE, col.names=FALSE)
}

