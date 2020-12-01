\name{cubist_asR_idioms}
\alias{cubist_asR_idioms}
\title{Export Tidy Rules of Cubist Object to R Idioms}
\description{
This function exports five \R source code files containing low-level idioms of the language so that the \pkg{Cubist} model can be reused or idiom implemented without having the \pkg{Cubist} package itself. The function, thus, permits dependency free archival of a \pkg{Cubist} model based on the basic functionality of the language.

For idiom implementation, it is important that the concept of \dQuote{neighbors} as a feature and argument to the prediction of \pkg{Cubist} not be used. The pursuit here, for a given row of the input matrix, is to isolate by name each applicable rule and then trigger a prediction for each rule and then pool the estimate by some method.
}
\usage{
cubist_asR_idioms(cubist_object, cubist_tag="C0", nut_digits=3,
                  path=".", prefix="", tidyrule.return=FALSE,
                  sample_str="", var.names=NULL, write.utility.file=TRUE, ...)
}
\arguments{
  \item{cubist_object}{Either a \pkg{Cubist} object (model) from the \pkg{Cubist} package on which the function \cr \code{tidyrules::tidyRules()} will be called or the results from the user already having called \code{tidyRules()} on the \pkg{Cubist} object;}
  \item{cubist_tag}{A string thought of a being a small fragment of arbitrary text chosen by a user that identifies a particular \pkg{Cubist} model. This tag is critically important and useful for subroutines on export and the \R files will have this tag in their names. In imagined use, it is expected that the tag would be a just one or two characters long;}
  \item{nut_digits}{The number of digits to pass to the \code{round} function as the nuts of each branch are written. This helps keep file sizes down and the written code more readable than letting full floating point result;}
  \item{path}{The directory path to which the various \R files are to be written;}
  \item{prefix}{A string that is prefixed to the output file names of this function. This option is useful if the \code{cubist_tag} is a number to avoid having file names starting with a number;}
  \item{tidyrule.return}{A logical triggering the \code{tidyrules::tidyRules()} data frame;}
  \item{sample_str}{An arbitrary string of sample-size summary information or really any other content about the \pkg{Cubist} model that the user wants to give over to the idiom implementation by creating a variable \code{SAMPLE_STR} equal to \code{sample_str}. The user would then do their own development to parse this variable for their own purposes;}
  \item{var.names}{The comprehensive list of variable names given over to the \pkg{Cubist} fitting. These names need to be specified at least once for a given use of \code{write.utility.file} being set. The variable names are automatically derived from the \code{cubist_object}, if such an object (not \code{tidyRules}) is fed as the first argument, by the internal logic \code{names(cubist_object$coefficients)}. So if \code{tidyRules} were used for the first argument, then at some point, the user would have to specify\cr \code{var.names=names(cubist_object$coefficients)};}
  \item{write.utility.file}{A logical to trigger the writing of the \code{cubist_utils.R} file. This file is specific \code{cubist_tag} independent and only needs to be written once; and}
  \item{...}{Additional arguments to pass (currently not used);}
}
\value{
  This function is used for its side effects written to the file system, but can be used to return without modification the results of the \code{tidyrules::tidyRules()} function.
}
\note{
Basic description of the written files by this function follow in this section.

\bold{Utilities:} The file \code{cubist_utils.R} contains \code{cubist_tag} independent logic that is used as an abstraction layer to access the various idioms specific to the \pkg{Cubist} rules, branches, and nuts. The function therein \code{getVARIABLE_NAMES} returns a vector of variable names given over to \pkg{Cubist}. This vector could be used to testing external incoming input data frames as having all of the variables required. The function therein \code{useBRANCH} is the abstraction layer to access the linear model at the end of each rule by evaluation of its function name; this material is the linear model as shown by \code{tidyrules::tidyRules()}. Finally, the function therein \code{getNUTS} is the abstraction layer to access the diagnostics about the rule as shown by \code{tidyrules::tidyRules()} that include the support (number of observations) for the rule, mean response value for the rule, minimum response value for the rule, maximum response value for the rule, and error in units of the response for the linear model.
}
\author{ W.H. Asquith}
\seealso{  \code{\link{cubist_asPerl_idioms}} }
\examples{
\donttest{
# We set the simulation sample size to 1,000; the number of committees is 3.
nsim <- 1000; committees <- 3

# The high-level design is to have the ability to tag a given Cubist object.
# The tag will be used as output files and subroutines and variables therein
# are written.
cubist_tag <- 9 # say our "ninth" Cubist object
set.seed(cubist_tag) # arbitrary here, but setting the seed for reproducibility
cubist_tag <- paste0("C",cubist_tag) # create the tag "C9" to be used

# simulate some X and Y data
X <- sort(runif(nsim, min=0, max=2))
Y <- 0.34*sin(2*pi*X) - .74*cos(2*pi*X) -
     0.14*sin(4*pi*X) + .19*cos(4*pi*X) + rnorm(nsim, sd=0.2)
X <- data.frame(X=X, pi=pi) # the design that follows both by Cubist
# and by the idiom functions in this package need at least two columns
# even if the second column containing pi is never used.

# We can foreshadow some type of cross-validation, but here just use all.
phi <- 1
inSchool <- sample(1:nrow(X), floor(phi*nrow(X)))
Xs <- X[ inSchool,]; Xt <- X[-inSchool,]
Ys <- Y[ inSchool ]; Yt <- Y[-inSchool ]

# make the Cubist model
cubist_tree <- Cubist::cubist(x=Xs, y=Ys, committees=committees)
# user could inspect tidy_rules and see what is happening internally
# to functions of the Cubist.Idioms package, but we will let the package
# call the tidyRules internally in cubist_asR_idioms().
# tidy_rules  <- tidyrules::tidyRules(cubist_tree)

# later a diagnostic plot is made, so get the Cubist predictions
# neighbors=0 is default but critical as the idioms do not support
cubist_pred <- predict(cubist_tree, X, neighbors=0) # }

\donttest{
# construct a string as described above, here a colon-delimited,
# equal-sign keyed sequence of content that we want preserved in
# the exported Cubist idioms for access again when we build 
# infrastructure to use the idioms, not otherwise in this demo
txt <- paste0("committees=",committees,":sample_size=",nsim)

tmpath <- tempdir() # temporary directory
message("temporary path = ",tmpath)

cubist_asR_idioms(cubist_tree, cubist_tag=cubist_tag,
                  path=tmpath, sample_str=txt)
                  
files <- list.files(path=tmpath, pattern=paste0(cubist_tag,".+.R"))
files <- c(files, "cubist_utils.R")
print(files)
# [1] "C9cubist_cubes.R" "C9cubist_funcs.R" "C9cubist_nuts.R" 
# [4] "C9cubist_rules.R" "cubist_utils.R"
# so four source files of idioms for the C9 tagged Cubist were
# made, and the cubist_utils.R is tag-independent and has a couple
# of accessor functions to work with the our big picture }

\donttest{
# now begin a minimal recipe on use of the idioms and hence breaking
# from a dependency of having to have Cubist library around to make
# the predictions. first, the idiom files are sourced, here 5 files
for(file in files) source(paste0(tmpath,"/",file))

# now some trickery, in case we had multiple tagged Cubist models
# loaded via the idiom export, lets dynamically construct an
# interface called CUBES() that for a row of input matrix will provide
# the applicable rules.
CUBES <- eval(parse(text=paste0("CUBES_",cubist_tag)))

# initialize the predictions, here we want to demonstrate two
# the use of mean() is basically how Cubist works if neighbors=0, but
# because we have errors, a weighted mean is also accessible.
# is.na()'s (if they hit) are trapped separately, so we do not use
# na.rm=TRUE for the mean() or weighted.mean()
Y_by_idioms_mean <- Y_by_idioms_wgt <- rep(NA, nrow(X))

# loop through the rows of the input matrix
for(i in 1:nrow(X)) {
  x <- X[i,] # isolate a row
  rl <- CUBES(x) # figure out which rules are applicable
  rl <- rl[! is.na(rl)] # all rules for each committee are tested
  if(length(rl) == 0) { # so we need to strip the NA's and if zero
    Y_by_idioms_mean[i] <- NA # then trap the situation and make
    Y_by_idioms_wgt[i]  <- NA # the prediction NA
    next
  }
  # the "branch" here is the actually linear function sitting at the
  # end of the rule; the "nut" here are the diagnostics of the linear
  # function at the end of the branch.
  pr <- useBRANCH(x, rules=rl, cubes=CUBES) # vector of predictions
  nt <- getNUTS(     rules=rl, cubes=CUBES) # data frame of diagnostics
  Y_by_idioms_mean[i] <- mean(pr) # compute the predicted mean
  wgts <- 1/nt$error; wgts <- wgts/sum(wgts) # weighted mean
  Y_by_idioms_wgt[i]  <- sum(wgts*pr) # requires sum weights == 1
}
message("Last row in X matrix had these branches and nuts\n")
print(nt) # showing here the structure idea of the "nut"
#                    support   mean    min    max error
# MODC9_COM01_BRA002     172 -0.566 -1.293 -0.048 0.174
# MODC9_COM02_BRA001      80 -0.573 -1.269 -0.117 0.168
# MODC9_COM03_BRA002     172 -0.566 -1.293 -0.048 0.174
# the nut shows us by row.names(), which rules and committees 
# tested as applicable for the last row of the input, three here

plot(X$X,Y, type="p", lwd=0.4, col=grey(0.2),
      xlab="X variable", ylab="Y variable",
      xlim=c(0,2), ylim=c(-1,2))
lines(X$X, cubist_pred,      col=3, lwd=7)
lines(X$X, Y_by_idioms_mean, col=2, lwd=3)
lines(X$X, Y_by_idioms_wgt,  col="#2E5090", lwd=1.3, lty=2)
legend("topleft", c("Simulated data", "Cubist itself",
                    "Cubist idioms (mean)",
                    "Cubist idioms (weighted mean)"),
       lty=c(0, 1, 1, 2), lwd=c(0, 7, 3, 1.3), bty="n",
       pch=c(1, NA, NA, NA), pt.lwd=c(0.4, NA, NA, NA),
       col=c(grey(0.2), 3, 2, "#2E5090"), cex=0.8) #}
}
\keyword{R language}
