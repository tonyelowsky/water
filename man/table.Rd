\name{table}
\alias{table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{table
%%  ~~function to automatically generate standard tables for water reports ... ~~
}
\description{This function accepts a data frame to create a standard (non-standard later) report table using the design data acquired with the design() function. Data frames read with read() and trimmed to the correct height and width using trim() will have a format which produces clean tables ~~
}
\usage{
table(frame,d=NULL,reg=T,type="",Ititle="")
}

\arguments{
  \item{frame}{the data frame object to be tabled
}
  \item{d}{the design object produced by design(), NULL produces a table with no additional design characteristics}
  \item{reg}{the table type, is the table regular? = True, other functionality will be added later}
  \item{type}{a character string which describes the contents of the table which will decide how it will be organized into the standard folders. the default is that it will be saved into the root folder, other options are "rates", if it should go in the rates folder, "cos", if it should go into the cost of service folder, "revreq", if it should go in the revenue requirements folder, and "key", if it should go in the key assumptions folder. folders should be created using the directory() function}
  \item{Ititle}{character string which signifies the name of the output jpg. Tables should always have a title}
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
\author{Tony Boy
%%  ~~who you are~~
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
table(df,Ititle="hey")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
