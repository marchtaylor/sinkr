#' Conduct k-fold cross validation with multiple permutations
#'
#' @param modelfun Model function to use ("lm", "glm", "gam", "gls")
#' @param k Number of folds to use (see \code{\link[sinkr]{kfold}})
#' @param nperm Number of permutations
#' @param argList List of arguments to pass to modelFun
#'
#' @return dataframe of validation (i.e. response) and prediction values
#' @export
#'
#' @examples
#' # make data
#' set.seed(1111)
#' n <- 100
#' x <- sort(rlnorm(n, meanlog = 0.25, sdlog = 1.5))
#' a <- 0
#' b <- 100
#' cv <- 0.5
#' y <- (a + x*b) * rlnorm(n, 0, cv)
#' 
#' # fit models
#' fitLM <- lm(y ~ x - 1)
#' fitLogLM <- lm(log(y) ~ log(x))
#' fitGLM <- glm(y ~ x - 1, family=Gamma(link="identity"))
#' 
#' # plot
#' plot(x, y)
#' lines(x, predict(fitLM), col=2, lty=2)
#' lines(x, exp(predict(fitLogLM)), col=3, lty=3)
#' lines(x, predict(fitGLM, type="response"), col=4, lty=4)
#' legend("bottomright", legend=c("LM", "logLM", "GLM (Gamma)"), col=2:4, lty=2:4)
#' 
#' # Compare with Aikaike Information Criterium (AIC)
#' AIC(fitLM, fitLogLM, fitGLM) # not an appropriate comparison
#' 
#' 
#' 
#' # LM
#' set.seed(1)
#' res <- cv.nperm(modelfun="lm", k=5, nperm=9,
#'   argList=list(
#'     data = data.frame(x,y), 
#'     formula = formula(y ~ x - 1)
#'   )
#' )
#' sqrt(mean((res[,1] - res[,2])^2)) # root mean squared error
#' median(abs((res[,1] - res[,2])/res[,2] * 100)) # median absolute percent error
#' 
#' 
#' # log LM
#' set.seed(1)
#' res <- cv.nperm(modelfun="lm", k=5, nperm=9,
#'   argList=list(
#'     data = data.frame(x,y), 
#'     formula = formula(log(y) ~ log(x))
#'   )
#' )
#' sqrt(mean((exp(res[,1]) - exp(res[,2]))^2)) # root mean squared error
#' median(abs((exp(res[,1]) - exp(res[,2]))/exp(res[,2]) * 100)) # median absolute percent error
#' 
#' 
#' # GLM
#' set.seed(1)
#' res <- cv.nperm(modelfun="glm", k=5, nperm=9,
#'   argList=list(
#'     data = data.frame(x,y), 
#'     formula = formula(y ~ x - 1),
#'     family = Gamma(link = "identity")
#'   )
#' )
#' sqrt(mean((res[,1] - res[,2])^2)) # root mean squared error
#' median(abs((res[,1] - res[,2])/res[,2] * 100)) # median absolute percent error
#' 
#' 
#' 
cv.nperm <- function(modelfun="lm", k=4, nperm=10, argList=NULL){
  if(is.null(argList)) stop("must provide arguments to model function in form of a list")
  res <- vector(mode="list", nperm)
  for(i in seq(nperm)){
    ks <- kfold(nrow(argList$data),k=k)
    res[[i]] <- vector(mode="list", length(ks))
    for(j in seq(ks)){
      train <- argList$data[-ks[[j]],]
      valid <- argList$data[ks[[j]],]
      argList.j <- argList
      argList.j$data <- train
      if(modelfun == "lm"){
        fit <- do.call("lm", args=argList.j)
        pred <- predict(fit, newdata = valid)
        resp.expr <- parse(text=getResponse(fit))
        res[[i]][[j]] <- cbind(data.frame(valid=eval(resp.expr, valid)), data.frame(pred))
      }
      if(modelfun == "glm"){
        fit <- do.call("glm", args=argList.j)
        pred <- predict(fit, newdata = valid)
        resp.expr <- parse(text=getResponse(fit))
        res[[i]][[j]] <- cbind(data.frame(valid=eval(resp.expr, valid)), data.frame(pred))
      }
      if(modelfun == "gam"){
        fit <- do.call("gam", args=argList.j)
        pred <- predict(fit, newdata = valid)
        resp.expr <- parse(text=getResponse(fit))
        res[[i]][[j]] <- cbind(data.frame(valid=eval(resp.expr, valid)), data.frame(pred))
      }
      if(modelfun == "gls"){
        fit <- do.call("gls", args=argList.j)
        pred <- predict(fit, newdata = valid)
        resp.expr <- parse(text=attr(nlme::getResponse(fit), "label"))
        res[[i]][[j]] <- cbind(data.frame(valid=eval(resp.expr, valid)), data.frame(pred))
      }
    }
    res[[i]] <- do.call("rbind", res[[i]])
  }
  res <- do.call("rbind", res)
  names(res) <- c("validation", "prediction")
  return(res)
}