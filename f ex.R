myfunction1 <- function() {
    x <- rnorm(100)
    mean(x)
}

second1 <- function(x) {
    x + rnorm(length(x))
}

add3 <- function(x, y) {
    x+y
}
 
above <- function(x, n) {
    use <- x > n
    x[use]
}

make.power <- function(n){
    pow <- function(x) {
        x^n
    }
    pow
}

f <- function(x) {
    y <- 2
    y^2+g(x)
}

g <- function(x){
    x*y
}


make.NeglogLik <- function(data, fixed = c(FALSE, FALSE)){
    params <- fixed
    function(p){
        params[!fixed] <- p
        mu <- params[1]
        sigma <- params[2]
        a <- -0.5*length(data)*log(2*pi*sigma^2)
        b <- -0.5*sum((data-mu)^2)/(sigma^2)-(a)
    }
}

noise <- function(n, mean, sd){
    rnorm(n, mean, sd)
}

hilbert <- function(n){
    i <- 1:n
    1/outer(i-1,i,"+")
}