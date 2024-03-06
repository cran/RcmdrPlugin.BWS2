\name{bws2Model}
\alias{bws2Model}
\alias{resetBws2Model}

\title{Fit Model to BWS2 Data}

\description{
Fit a model to BWS2 data.
}

\details{
This dialog box fits a conditional logit model to the data set for BWS2
analysis using the functions \code{\link[survival]{clogit}} in \pkg{survival} 
and \code{\link[support.CEs]{gofm}} in \pkg{support.CEs}.
This dialog box is activated if the data set for BWS2 analysis created in
the \sQuote{\code{Create Data Set for BWS2 Analysis}} dialog box is selected
as the active data set. 
For details on the results of this dialog box, refer to helps for functions
\code{\link[survival]{clogit}} and \code{\link[support.CEs]{gofm}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for model}: An R object name into which the results of 
a fitted model obtained through this dialog box are stored. 
The default name \sQuote{\code{BWS2model.x}}, where \code{x} indicates 
the integer number assigned automatically from \code{1}, can be changed
following R's rules for naming objects.

\item \code{Response variable}: A variable containing responses to BWS2
questions in the active data set. 
This variable name is fixed as \sQuote{\code{RES}}, which cannot be
changed by users (A fixed variable is colored in green).

\item \code{Attribute variables}: Attribute variables used in the model.
 Select zero or more variables from this list box.

\item \code{Level variables}: Level variables used in the model.
Select appropriate variables from this list box.

\item \code{Covariates}: Variables used as covariates in the model.
Select zero or more variables from this list box.

\item \code{Stratification variable}: A variable for identifying each
combination of respondents and BWS2 questions. 
This variable name is fixed as \sQuote{\code{STR}}.

\item \code{Subset expression}: A logical expression for specifying 
a subset of the observations in the active dataset to be used for
the fitting.
}

}

