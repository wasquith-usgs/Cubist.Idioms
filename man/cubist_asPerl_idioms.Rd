\name{cubist_asPerl_idioms}
\alias{cubist_asPerl_idioms}
\title{Export Tidy Rules of Cubist Object to Perl Idioms}
\description{
This function exports one \bold{Perl} module source code file containing low-level idioms of the language so that the \pkg{Cubist} model can be reused or idiom implemented without having the \pkg{Cubist} package itself. The function, thus, permits dependency free archival of a \pkg{Cubist} model based on the basic functionality of the language.

For idiom implementation, it is important that the concept of \dQuote{neighbors} as a feature and argument to the prediction of \pkg{Cubist} not be used. The pursuit here, for a given row of the input matrix, is to isolate by name each applicable rule and then trigger a prediction for each rule and then pool the estimate by some method.
}
\usage{
cubist_asPerl_idioms(cubist_object, cubist_tag="C0", nut_digits=3,
                     parentpmdir="IDIOMS", path=".", prefix="",
                     tidyrule.return=FALSE, sample_str="", ...)
}
\arguments{
  \item{cubist_object}{Either a \pkg{Cubist} object (model) from the \pkg{Cubist} package on which the function \cr \code{tidyrules::tidyRules()} will be called or the results from the user already having called \code{tidyRules()} on the \pkg{Cubist} object;}
  \item{cubist_tag}{A string thought of a being a small fragment of arbitrary text chosen by a user that identifies a particular \pkg{Cubist} model. This tag is critically important and useful for subroutines on export and the \bold{Perl} module nomenclature will have this tag in their names;}
  \item{nut_digits}{The number of digits to pass to the \code{round} function as the nuts of each branch are written. This helps keep file sizes down and the written code more readable than letting full floating point result;}
  \item{parentpmdir}{The top-level directory name that the output file would need to be migrated into. For example, the default would represent a path of \code{IDIOMS::cubist*.pm} of the \bold{Perl} module where the \code{*} is the \code{cubist_tag};}
  \item{path}{The directory path to which the various \bold{Perl} files are to be written;}
  \item{prefix}{A string that is prefixed to the output file names of this function. This option is useful if the \code{cubist_tag} is a number to avoid having file names starting with a number;}
  \item{tidyrule.return}{A logical triggering the \code{tidyrules::tidyRules()} data frame;}
  \item{sample_str}{An arbitrary string of sample-size summary information about the Cubist model that the user wants to give over to the idiom implementation by creating a variable \code{$SAMPLE_STR} equal to \code{sample_str};}
  \item{...}{Additional arguments to pass (currently not used);}
}
\value{
  This function is used for its side effects written to the file system, but can be used to return without modification the results of the \code{tidyrules::tidyRules()} function.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{Xrow_to_Perl}}, \code{\link{cubist_asR_idioms}} }
\examples{
\dontrun{
# We set the simulation sample size to 1,000; the number of committees is 3.
nsim <- 1000; committees <- 3

# The high-level design is to have the ability to tag a given Cubist object.
# The tag will be used as output files and subroutines and variables therein
# are written.
cubist_tag <- 0 # say our "zeroth order" Cubist object
set.seed(9) # arbitrary here, but setting the seed for reproducibility
cubist_tag <- paste0("C",cubist_tag) # create the tag "C0" to be used

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

# construct a string as described above, here a colon-delimited,
# equal-sign keyed sequence of content that we want preserved in
# the exported Cubist idioms for access again when we build 
# infrastructure to use the idioms, not otherwise in this demo
txt <- paste0("committees=",committees,":sample_size=",nsim)

tmpath <- tempdir() # temporary directory
message("temporary path = ",tmpath)

cubist_asPerl_idioms(cubist_tree, cubist_tag=cubist_tag,
                     path=tmpath, sample_str=txt)
                  
files <- list.files(path=tmpath, pattern=paste0(".+",cubist_tag,".pm"))
print(files)
# [1] "cubistC9.pm"
# so one source file of idioms for the C9 tagged Cubist were
# made, and the cubist_utils.R is tag-independent and has a couple
# of accessor functions to work with the our big picture }
}
\keyword{Perl language}
