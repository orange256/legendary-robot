\name{thx_seafood}
\alias{thx_seafood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a 2nd-order CFA model with interaction term.
}

\description{
This function can help you to construct a 2nd-order CFA model with interaction term, and return model fit & factor scores.
}

\usage{
thx_seafood(data, model, eta, ksi, method = c(full, marsh, cross), product = NULL, DV = NULL, ID = NULL, missing_value = NULL, data_format = NULL, save_data = TRUE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  your data.
}
  \item{model}{
  A 1-order CFA model formula.
}
  \item{eta}{
  First argument of 2-order CFA model.
}
  \item{ksi}{
  Second argument of 2-order CFA model.
}
  \item{method}{
  Which method you want to use to construct interaction term. 'full' for full model; 'marsh' for marsh model; 'cross' for cross-product.
}
  \item{product}{
  If you choose 'cross' method, then you need to give us which combination product you want to use. eg: product=c(f1f4,f2f4) (default=NULL)
}
  \item{DV}{
  If you choose 'lms' method, then you need to give us dependent variable in regression model. eg: DV='finalscore' (default=NULL)
}
  \item{ID}{
  ID variable. eg: ID='id' (default=NULL)
}
  \item{Missing_value}{
  Missing flag in your dataset. eg: If you set -999 as missing, then you need set Missing_value=-999 .(default=NULL)
}
  \item{data_format}{
  Mplus format setting. (only in 'lms' method) eg: data_format=(2F8.0, 5F8.2) . (default=NULL)
}
  \item{save_data}{
  Should save Mplus file which in D:/Mplus_in_R ?  (default=TRUE)
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{fit }{a model fit of CFA model (only in 'full', 'marsh', 'cross' model)}
  \item{Summaries }{Mplus summaries (only in 'lms' model)}
  \item{Parameters }{Mplus Parameters (only in 'lms' model)}
  \item{fscore_2inter }{combine your original data with factor scores from the results.}

%% ...
}
\references{
Marsh, H. W., Wen, Z., & Hau, K.-T. (2004). Structural Equation Models of Latent Interactions: Evaluation of Alternative Estimation Strategies and Indicator Construction. Psychological Methods, 9(3), 275-300.
http://dx.doi.org/10.1037/1082-989X.9.3.275
}
\author{
  ml2lab-nctu <mllab.nctu@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
demo_data <- seafooood::demo_data

# Step 1: Construct a 1-order CFA model
CFA_model <-'f1 =~ V1 + V2 +V3
             f2 =~ V4 + V5 +V6
             f3 =~ V7 + V8 +V9
             f4 =~ V10 + V11 +V12
             f5 =~ V13 + V14 +V15
             f6 =~ V16 + V17 +V18
             '
# Step 2-1: choose 'full' model method
full_result <- thx_seafood(data = demo_data,
                           model = CFA_model,
                           eta = c('f1','f2','f3'),
                           ksi = c('f4','f5','f6'),
                           method = "full"
                           )
# see model fit
summary(full_result$fit)

# see new data
full_data <- full_result$fscore_2inter

# Step 2-2: choose 'marsh' model method
marsh_result <- thx_seafood(data = demo_data,
                            model = CFA_model,
                            eta = c('f1','f2','f3'),
                            ksi = c('f4','f5','f6'),
                            method = "marsh"
                            )
summary(marsh_result$fit)
marsh_data <- marsh_result$fscore_2inter

# Step 2-3: choose 'cross' model method
cross_result <- thx_seafood(data = demo_data,
                            model = CFA_model,
                            eta = c('f1','f2','f3'),
                            ksi = c('f4','f5','f6'),
                            method = "cross",
                            product = c('f1f5','f1f6','f2f5')
                            )
summary(cross_result$fit)
cross_data <- cross_result$fscore_2inter

# Step 2-4: choose 'lms' model method
# file will save in "D:/Mplus_in_R"

lms_result <- thx_seafood(data = ori_data,
                          model = CFA_model,
                          eta = c('f1','f2','f3'),
                          ksi = c('f4','f5','f6'),
                          method = "lms",
                          DV = "V19"
                          )
lms_result$Summaries
lms_result$Parameters
lms_data <- lms_result$fscore_2inter



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
