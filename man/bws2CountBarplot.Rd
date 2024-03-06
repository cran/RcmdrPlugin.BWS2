\name{bws2CountBarplot}
\alias{bws2CountBarplot}


\title{Draw Distributions of BWS2 Scores}

\description{
Draw distributions of the BW scores by level.}

\details{
This dialog box draws distributions (bar plots) of the BW scores by level
by using the function \code{\link[support.BWS2]{barplot.bws2.count}} 
(\code{barplot} for S3 class \sQuote{\code{bws2.count}}). 
This dialog box is activated if the data frame containing BW scores created
in the \sQuote{\code{Calculate BWS2 Scores}} dialog box is selected as 
the active data set. 
For details on the results of this dialog box, 
refer to help for \code{\link[support.BWS2]{bws2.count}}. 

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Score type}: Type of score to be drawn. Select one from three
radio buttons corresponding to \sQuote{\code{Best-minus-Worst}}, 
\sQuote{\code{Best}}, and \sQuote{\code{Worst}}.

\item \code{Arrangement of bar plots (optional)}: An arrangement of bar
plots to be drawn. The number of bar plots drawn by this dialog box is
equal to the number of items (\emph{K}). Total \emph{K} bar plots are 
drawn in an \emph{NR}-by-\emph{NC} array (matrix) on the device by row,
where \emph{NR} and \emph{NC} are integer values entered in
the \sQuote{\code{row(s)}} box and the \sQuote{\code{column(s)}} box,
respectively.
}

}

