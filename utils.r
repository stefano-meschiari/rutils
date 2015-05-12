# Like apply, but removes any NULLs in the return list
capply <- function(X, MARGIN, FUN, ...) {

    
    l <- apply(X=X, MARGIN=MARGIN, FUN=FUN, ...)
    l <- Filter(Negate(is.null), l)

    return(l)
}

# Like sapply, but removes any NULLs in the return list
csapply <- function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE) {    
    l <- sapply(X=X, FUN=FUN, ..., simplify=simplify, USE.NAMES=USE.NAMES)
    l <- Filter(Negate(is.null), l)

    if (!identical(simplify, FALSE) && length(l)) 
        return(simplify2array(l, higher = (simplify == "array")))
    else 
        return(l)
}

csnapply <- function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE) {
    Xn <- 1:length(X)
    return(csapply(Xn, function(i, ...) {
        return(FUN(i, X[[i]], ...))
    }, simplify=simplify, USE.NAMES=TRUE))
}

# Operator to paste strings
`%.%` <- function(str1, str2) {
    return(paste(str1, str2, sep=''))
}

# Reads an entire file
readAll <- function(file) {
    return(readChar(file, file.info(file)$size))
}

str_replacef <- function(string, pattern, replacement.fun) {
    matches <- str_match(string, pattern)

    for (m in matches[,1]) {
        rep <- replacement.fun(m)
        string <- str_replace(string, pattern, rep)
    }
    return(string)
}
