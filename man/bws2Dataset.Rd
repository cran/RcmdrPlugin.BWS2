\name{bws2Dataset}
\alias{bws2Dataset}
\alias{resetBws2Dataset}
\alias{bws2DataP}

\title{Create Data Set for BWS2 Analysis}

\description{
Create a data set for BWS2 analysis combining the choice sets and data set containing responses to BWS2 questions.
}

\details{
This dialog box uses the function \code{\link[support.BWS2]{bws2.dataset}}
in \pkg{support.BWS2} to create a data set for BWS2 analysis combining 
the choice sets designed in the \sQuote{\code{Design Choice Set for BWS2}}
dialog box and a data set containing responses to BWS2 questions. 
For details on the results of this dialog box, refer to help for the function 
\code{\link[support.BWS2]{bws2.dataset}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for data set}: An R object name into which a data set
generated using this dialog box is stored. 
The default name \sQuote{\code{BWS2data}} can be changed
following R's rules for naming objects. If changed, you should take note of
the new object name, which will be used later.

\item \code{Save to file}: A check box for saving the resultant data set
to an external file in RDA format (\code{.rda}). 
If necessary, check this box.

\item \code{Design}: An R object name containing BWS2 choice sets generated
in the \sQuote{\code{Design Choice Set for BWS2}} dialog box. 
The default name \sQuote{\code{BWS2design}} is displayed automatically. 
The correct name should be entered if the name of BWS2 choice sets 
(\code{BWS2design}) was changed in the dialog box for BWS2 design.

\item \code{Attributes and levels}: An R object name containing attributes
and their levels set in the \sQuote{\code{Design Choice Set for BWS2}} 
dialog box.
The object name \sQuote{\code{BWS2attributes}}
cannot be changed (A fixed variable is colored in green).

\item \code{ID variable}: A variable name identifying respondents. 
The default name \sQuote{\code{id}} can be changed according to 
the corresponding variable name in the active data set.

\item \code{Model type}: Type of model used for the analysis. 
Select one from three radio buttons corresponding to 
\sQuote{\code{Paired model}}, \sQuote{\code{Marginal model}}, and 
\sQuote{\code{Marginal sequential model}}. 
See help for the function \code{\link[support.BWS2]{bws2.dataset}} 
for details on three models.

\item \code{Reverse attribute variables}: A check box to indicate 
when the signs of the attribute variables are reversed for 
the possible worst.

\item \code{Base attribute}: The base attribute when attribute variables 
are created as effect-coded variables. No variable is selected when 
attribute variables are created as dummy coded variables.

\item \code{Base level for X}: the base level in attribute \code{X} 
when level variables are created as effect-coded variables. 
No variable is selected when level variables are created as dummy coded
variables.

\item \code{Number of BWS2 questions}: Number of BWS2 questions per
respondent. According to the design parameters set in 
the \sQuote{\code{Design Choice Set for BWS2}} dialog box, 
this slider is set at the appropriate number. 
Depending on the number, the number of rows of 
the \sQuote{\code{Names of best- and worst-response variables}} table varies.

\item \code{Letters of best- and worst-response variables}: A type of 
letters used for best- and worst-variable names shown in the 
\sQuote{\code{Names of best- and worst-variable names}} table. 
Select one from three radio buttons corresponding to \sQuote{\code{Uppercase}}, 
\sQuote{\code{Lowercase}}, and \sQuote{\code{None}}. According to the
selection, letters of best- and worst variable names in the table are
changed. When selecting \sQuote{\code{None}}, no variable names appear in the table.
Users must type appropriate names in each cell of the table.

\item \code{Names of best- and worst-response variables}: Names of best- and
worst-response variables stored in the active data set. These names must
follow R's rules for naming variables.
}

}

