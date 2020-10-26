\name{Xrow_to_Perl}
\alias{Xrow_to_Perl}
\title{Export One Row of the X Data Frame to a Simple Perl Hash Reference Idiom}
\description{
Export one row of the \code{X} data frame suitable for the input to the \pkg{Cubist} modeling to a simple \bold{Perl} language hash reference idiom in a local variable (\code{  $X = { ... };}. The previous \bold{Perl} notation means create a variable \code{$X} and as a reference toa hash (the opening and closing braces) and the triple dots are \code{key => value,} pairs presenting the targeted \code{row} of \code{X}. This ability to export the row as a hash reference for \bold{Perl} makes it straightforward for testing \pkg{Cubist} model exported by \code{\link{cubist_asPerl_idioms}}. The variable \code{$X} can be vivicated into a \bold{Perl} script by the \code{eval} function. Values that are \code{NA} in \R become the string \code{"infinity"} in \bold{Perl}; this function thus makes that conversion on the fly.
}
\usage{
Xrow_to_Perl(x, row=1, file="", comment=NA)
}
\arguments{
  \item{x}{A data frame having at least the columns required by a \pkg{Cubist} model that the uer is pursuing. This data frame must be compatible with \code{Cubist::cubist()} and \code{Cubist::predict.cubist()};}
  \item{row}{The row number in \code{x} to export;}
  \item{file}{The argument of the same name and purpose as \code{file} for the \code{cat()} function. This is a connection; and}
  \item{comment}{An optional comment line that is written before the rest of the variables.}
}
\value{
  This function is used for its side effects written to the file system or to the console.
}
\note{
Some more details on how the results of the script could be used. The design here is to get the variable \code{$X} defined within a \bold{Perl} script and though not used here, the export by \code{\link{cubist_asPerl_idioms}} is designed around a variable of a hash reference contain a single row of the \code{X} input data frame for \pkg{Cubist}.

Suppose that we have run the code in the \bold{Examples} and have the hash reference written into a file named \code{input.txt}. We can have this hash reference available
inside a \bold{Perl} script. To demonstrate, suppose we have our script named \code{slurp.pl} and the contents of that script are shown below:
\preformatted{
  #!/usr/bin/perl -w            # very conventional declaration of Perl script
  use strict;                   # force declaration of variables
  my ($row, $X) = (0, "empty"); # declare and set them both
  undef $/;                     # undefine line ending to as to
  my $f = <>;                   # **slurp** file name into variable $f
  print "read : $f\n";          # echo what was read by prior line
  eval $f;                      # evaluate the contents of $f
  print "row=$row and pi=$X->{pi}\n";   # print to standard output
  # row=1 and pi=3.14159265358979       # should be seen on execution
}
We can run the Perl script from the command line by \code{./slurp.pl input.txt} or on a unix-pipe line by \code{cat input.txt | ./slurp.pl}.
}
\author{ W.H. Asquith}
\seealso{  \code{\link{cubist_asPerl_idioms}} }
\examples{
# We set the simulation sample size to 1,000
nsim <- 1000

set.seed(1) # arbitrary here, but setting the seed for reproducibility

# simulate some X and Y data
X <- sort(runif(nsim, min=0, max=2))
Y <- 0.34*sin(2*pi*X) - .74*cos(2*pi*X) -
     0.14*sin(4*pi*X) + .19*cos(4*pi*X) + rnorm(nsim, sd=0.2)
X <- data.frame(X=X, pi=pi) # the design that follows both by Cubist
# and by the idiom functions in this package need at least two columns
# even if the second column containing pi is never used.

# export row 3 with the comment and result go to the console
Xrow_to_Perl(X, row=3, comment=" from Cubist.Idioms::Xrow_to_Perl()")
##  from Cubist.Idioms::Xrow_to_Perl()
#  $ROW = 3;
#  $X = {
#   X => 0.00262931315228343,
#   pi => 3.14159265358979};

path <- tempdir()
file <- paste0(path,"/","perlsrc.txt")
FH <- file(file, open="w")
  Xrow_to_Perl(X, row=3, comment="", file=FH)
close(FH)
print(file)
}
\keyword{Perl language}
