##' Easily Parse block-diagonal matrices with lower triangular info
##'
##' @param x list, matrix or expression, see details
##'
##' @param ... Other arguments treated as a list that will be
##'     concatenated then reapplied to this function.
##'
##' @return named symmetric matrix useful in \link[RxODE]{RxODE} simulations (and
##'     perhaps elsewhere)
##'
##' @details
##'
##'  This can take an R matrix, a list including matrices or
##'  expressions, or expressions
##'
##'  Expressions can take the form
##'
##'  name ~ estimate
##'
##'  Or the lower triangular matrix when "adding" the names
##'
##'  name1 + name2 ~ c(est1,
##'                    est2, est3)
##'
##'  The matricies are concatenated into a block diagonal matrix, like
##'  \code{\link[Matrix]{bdiag}}, but allows expressions to specify
##'  matrices easier.
##'
##'
##' @examples
##'
##' ## A few ways to specify the same matrix
##' lotri({et2 + et3 + et4 ~ c(40,
##'                            0.1, 20,
##'                            0.1, 0.1, 30)})
##'
##' ## You  do not need to enclose in {}
##' lotri(et2 + et3 + et4 ~ c(40,
##'                           0.1, 20,
##'                           0.1, 0.1, 30),
##'           et5 ~ 6)
##' ## But if you do enclose in {}, you can use multi-line matrix specifications:
##'
##' lotri({et2 + et3 + et4 ~ c(40,
##'                            0.1, 20,
##'                            0.1, 0.1, 30);
##'           et5 ~ 6;
##'           })
##'
##' ## You can also add lists or actual R matrices as in this example:
##' lotri(list(et2 + et3 + et4 ~ c(40,
##'                                0.1, 20,
##'                                0.1, 0.1, 30),
##'               matrix(1,dimnames=list("et5","et5"))))
##'
##' ## Overall this is a flexible way to specify symmetric block diagonal matrices.
##'
##' @author Matthew L Fidler
##' @seealso \code{\link[RxODE]{RxODE}}
##' @export
lotri  <- function(x, ...){
    .lst  <- list(...);
    if (is.null(x)){
        .ret  <- NULL;
    } else if (is.list(x)){
        omega  <- lapply(x, lotri);
        if (is(omega, "list")){
            .omega <- as.matrix(Matrix::bdiag(omega));
            .d <- unlist(lapply(seq_along(omega),
                                function(x){
                dimnames(omega[[x]])[2]
            }))
            dimnames(.omega) <- list(.d, .d);
            omega <- .omega;
        }
        .ret  <- omega
    } else if (is.matrix(x)) {
        .ret  <- x
    } else {
        .env  <- new.env(parent=emptyenv());
        .env$df  <- NULL;
        .env$eta1 <- 0L;
        .f  <- function(x, env){
            if (is.name(x)){
                return(character())
            } else if (is.call(x)){
                if (identical(x[[1]], quote(`~`))){
                    if (length(x[[3]]) == 1){
                        ## et1 ~ 0.2
                        env$netas <- 1;
                        env$eta1 <- env$eta1 + 1;
                        env$names  <- c(env$names, as.character(x[[2]]));
                        env$df  <- rbind(env$df,
                                         data.frame(i=env$eta1, j=env$eta1, x=as.numeric(eval(x[[3]]))));
                    } else {
                        ## et1+et2+et3~c() lower triangular matrix
                        ## Should fixed be allowed????
                        if (any(tolower(as.character(x[[3]][[1]])) == c("c", "fix", "fixed"))){
                            if (any(tolower(as.character(x[[3]][[1]])) == c("fix", "fixed"))){
                                stop("fix/fixed are not allowed with RxODE omega specifications.");
                            }
                            env$netas <- length(x[[3]]) - 1;
                            .num <- sqrt(1+env$netas*8)/2-1/2
                            if (round(.num) == .num){
                                .n <- unlist(strsplit(as.character(x[[2]]), " +[+] +"));
                                .n <- .n[.n != "+"];
                                if(length(.n) == .num){
                                    env$names  <- c(env$names, .n);
                                    .r <- x[[3]][-1];
                                    .r <- sapply(.r, function(x){
                                        return(as.numeric(eval(x)));
                                    });
                                    .i <- 0
                                    .j <- 1;
                                    for (.k in seq_along(.r)){
                                        .v <- .r[.k];
                                        .i <- .i + 1;
                                        if (.i==.j){
                                            env$df  <- rbind(env$df,
                                                             data.frame(i=env$eta1+.i, j=env$eta1+.i, x=.v));
                                            .j <- .j + 1;
                                            .i <- 0;
                                        } else {
                                            env$df  <- rbind(env$df,
                                                             data.frame(i=c(env$eta1+.i, env$eta1+.j),
                                                                        j=c(env$eta1+.j, env$eta1+.i), x=.v));
                                        }
                                    }
                                    env$eta1 <- env$eta1 + .num;
                                }  else {
                                    stop("The left handed side of the expression must match the number of items in the lower triangular matrix.");
                                }
                            }
                        } else {
                            stop("Omega expression should be 'name ~ c(lower-tri)'");
                        }
                    }
                } else if (identical(x[[1]], quote(`{`))){
                    lapply(x, .f, env=env)
                } else if (identical(x[[1]], quote(`quote`))){
                    lapply(x[[2]], .f, env=env)
                } else {
                    stop("Omega expression should be 'name ~ c(lower-tri)'")
                }
            } else if (is.pairlist(x)) {
                lapply(x, .f, env=env);
            } else if (is.atomic(x)){
                stop("Something wrong with matrix parsing, likely a number is on a line without an identifier.");
            } else {
                stop("Unknown element in matrix parsing")
            }
        }
        .sX  <- substitute(x)
        if (is.call(.sX)){
            if (identical(.sX[[1]], quote(`[[`))){
                .sX  <- x
            }
        }
        .doParse  <- TRUE
        if (is.call(.sX)){
            if (identical(.sX[[1]], quote(`matrix`))){
                return(x);
            } else if (identical(.sX[[1]], quote(`list`))){
                omega  <- lapply(x, lotri);
                if (is(omega, "list")){
                    .omega <- as.matrix(Matrix::bdiag(omega));
                    .d <- unlist(lapply(seq_along(omega), function(x){dimnames(omega[[x]])[2]}))
                    dimnames(.omega) <- list(.d, .d);
                    omega <- .omega;
                }
                .ret  <- omega
                .doParse  <- FALSE
            }
        }
        if (.doParse){
            .f(.sX,.env);
            .ret <- diag(.env$eta1);
            for (.i in seq_along(.env$df$i)){
                .ret[.env$df$i[.i], .env$df$j[.i]]  <- .env$df$x[.i];
            }
            dimnames(.ret)  <- list(.env$names,.env$names)
        }
    }
    if (length(.lst)==0) return(.ret)
    else return(lotri(c(list(.ret),.lst)))
}
