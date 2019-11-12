\name{trim}
\alias{trim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{trim
%%  ~~function to do ... ~~
}
\description{
trim removes columns and rows outside of the bounds set in the function. Right now the default it set to use the design1 column as outside the bounds, and the "end" design element in the design1 column as outside the bounds. If only lcol or lrow are defined the data frame will keep the current number of columns/rows for the undefined value.
}
\usage{
trim(filename,lrow=a,lcol=a)
}

\arguments{
  \item{filename}{A data frame object
}
  \item{lrow}{the number of rows to be kept, the default value (auto) is to keep the current number of rows unless lcol is also default, then the function will find the boundaries defined in the design column}

  \item{lcol}{the number of columns to be kept, the default value is to keep the current number of columns unless lrow is also default, then the function will find the boundaries defined in the design column}
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
df <- data.frame(1:10,1:10)
df$design1 <- c(0,0,0,0,0,"end",0,0,0,0)
d <- trim(df)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
