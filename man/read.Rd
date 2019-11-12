\name{read}
\alias{read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{read
%%  ~~function to do ... ~~
}
\description{read() reads a character string to find a csv file in the root directory. It uses check.names=F so that the column names don't have periods in them. I basically just made it so we can omit the ".csv" from filenames, but I was thinking about adding some more functionality later, I need to play around with the function some more first though to see what would be appropriate.

}
\usage{
read(filename)
}
\arguments{
  \item{filename}{
should be a character string that is the title of a csv file saved in the root directory or a directory pathway to a file name. omit the .csv or it won't find it.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tony Boy
}
\note{
I figured this would be easier for Ichiko to remember
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
read.csv()
}
\examples{

{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
