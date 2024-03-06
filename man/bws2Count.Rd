\name{bws2Count}
\alias{bws2Count}
\alias{bws2CountP}

\title{Calculate BWS2 Scores}

\description{
Calculate various BW scores for each respondent from the data set for
BWS2 analysis.
}

\details{
This dialog box calculates various BW scores for each respondent from 
the data set for BWS2 analysis created in 
the \sQuote{\code{Create Data Set for BWS2 Analysis}} dialog box and
then stores them as a data frame using the function 
\code{\link[support.BWS2]{bws2.count}} in \pkg{support.BWS2}.
This dialog box is activated if the data set for BWS2 analysis is selected
as the active data set. 
For details on the results of this dialog box, refer to help for function
\code{\link[support.BWS2]{bws2.count}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for scores}: An R object name into which a data set 
generated using this dialog box is stored. 
The default name \sQuote{\code{BWS2data}} can be changed followingR's
rules for naming objects.
}

}

