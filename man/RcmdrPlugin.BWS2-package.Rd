\docType{package}
\name{RcmdrPlugin.BWS2-package}
\alias{RcmdrPlugin.BWS2-package}
\alias{RcmdrPlugin.BWS2}
\alias{bws2CountSum}
\alias{bws2Load}


\title{R Commander Plug-in for Case 2 Best-Worst Scaling}

\description{
This package adds menu items for case 2 (profile case) best-worst 
scaling (BWS2) to the R Commander. BWS2 is a question-based survey method 
that constructs profiles (combinations of attribute levels) using 
an orthogonal array, asks respondents to select the best and worst levels 
in each profile, and measures preferences for attribute levels 
by analyzing the responses. 
For details, see Aizaki and Fogarty (2019) <doi:10.1016/j.jocm.2019.100171>.
}

\details{
This package is an R Commander plug-in package for Case 2 (profile case) 
best--worst scaling (BWS2). It depends on 
\pkg{\link{DoE.base}} (Groemping 2018), 
\pkg{\link{Rcmdr}} (Fox 2005, 2017; Fox and Bouchet-Valat 2021), 
\pkg{\link{support.BWS2}} (Aizaki 2021; Aizaki and Fogarty 2019), 
\pkg{\link{support.CEs}} (Aizaki 2012, 2021), and
\pkg{\link{survival}} (Therneau 2022; Therneau and Grambsch 2000). 
Refer to Aizaki and Fogarty (2019) and 
\sQuote{Non-Market Valuation with R} <\url{http://lab.agr.hokudai.ac.jp/nmvr/}> 
for a brief introduction to BWS2 in R, 
and Louviere et al. (2015) and Flynn et al. (2007) for BWS2.

After successfully installing and loading \pkg{RcmdrPlugin.BWS2}, 
the R Commander window will appear, and then you will find \sQuote{\code{BWS2}} 
added to the top-level menus in the R Commander window.

Selecting \sQuote{\code{BWS2}} displays menu items. When selecting a menu item, 
the corresponding dialog box opens. The following dialog boxes are 
provided by \pkg{RcmdrPlugin.BWS2}:

\itemize{
   \item The dialog box for \sQuote{\code{Design choice sets...}} designs 
choice sets (profiles) for BWS2 with an orthogonal design 
using the function \code{\link[DoE.base]{oa.design}} 
in \pkg{\link{DoE.base}}.

   \item The dialog box for \sQuote{\code{Display questions...}} 
displays BWS2 questions from the profiles designed in the first dialog box 
using the function \code{\link[support.BWS2]{bws2.questionnaire}} in 
\pkg{\link{support.BWS2}}.

   \item The dialog box for \sQuote{\code{Create data set for analysis...}} 
creates a data set for analysis combining the choice sets designed in the first 
dialog box and a data set containing responses to BWS2 questions using 
the function \code{\link[support.BWS2]{bws2.dataset}} 
in \pkg{\link{support.BWS2}}.

   \item Selecting the menu item \sQuote{\code{Measure preferences...}} gives 
options: \sQuote{\code{Counting approach}} and \sQuote{\code{Modeling approach}}.
Each has additional sub-items (see below).

   \item The dialog box for \sQuote{\code{Calculate scores...}} calculates 
various BW scores for each respondent from the data set for BWS2 analysis 
created in the previous dialog box, and then stores them into a data 
frame using the function \code{\link[support.BWS2]{bws2.count}} 
in \pkg{\link{support.BWS2}}. The menu item is activated 
if a data set for BWS2 analysis created in the previous dialog box is 
selected as the active data set.

   \item Selecting \sQuote{\code{Sum up scores}} sums up the BW scores 
calculated in the previous dialog box and displays the results in the 
Output pane of the R Commander window. The menu item is activated 
if a data frame containing BW scores created in the previous dialog box is 
selected as the active data set.

   \item The dialog box for \sQuote{\code{Draw distributions of scores...}} 
draws distributions (bar plots) of the BW scores by level using an R 
basic graphic function \code{barplot} (S3 method for class \sQuote{bws2.count}: 
\code{\link[support.BWS2]{bws2.count}}). The menu item is activated 
if a data frame containing BW scores created in the previous dialog box is 
selected as the active data set.

   \item The dialog box for \sQuote{\code{Fit model...}} implements a conditional 
logit model analysis of the data set for BWS2 analysis created in the previous 
dialog box using the functions \code{\link[survival]{clogit}} 
in \pkg{\link{survival}} and \code{\link[support.CEs]{gofm}} 
in \pkg{\link{support.CEs}}. The menu item is activated 
if a data set for BWS2 analysis created in the previous dialog box is selected as 
the active data set.

   \item The dialog box for \sQuote{\code{Load saved objects...}} loads an R 
Data (rda) file saved in the dialog box for \sQuote{\code{Design choice sets...}} 
or \sQuote{\code{Create data set for analysis...}} using the function 
\code{\link[base]{load}} in \pkg{\link{base}}.
}

}

\section{Acknowledgments}{
This work was supported by JSPS KAKENHI Grant Number JP20K06251.
}

\author{
Hideo Aizaki
}

\references{
Aizaki H (2012) Basic Functions for Supporting an Implementation of 
Choice Experiments in R.
\emph{Journal of Statistical Software}, 50(C2): 1--24.
DOI: 10.18637/jss.v050.c02.

Aizaki H (2022) support.BWS2: Tools for Case 2 Best-Worst Scaling.
R package version 0.4-0. 
\url{https://CRAN.R-project.org/package=support.BWS2}.

Aizaki H (2021) support.CEs: Basic Functions for Supporting an Implementation 
of Choice Experiments.
R package version 0.5-0. 
\url{https://CRAN.R-project.org/package=support.CEs}.

Aizaki H, Fogarty J (2019) 
An R package and tutorial for case 2 best--worst scaling. 
\emph{Journal of Choice Modelling}, \bold{32}, 100171. 
DOI: 10.1016/j.jocm.2019.100171.

Aizaki H, Nakatani T, Sato K (2014) 
\emph{Stated Preference Methods Using R}. 
Chapman and Hall/CRC. 

Flynn TN, Louviere JJ, Peters TJ, Coast J (2007) Best-Worst Scaling: 
What it can do for health care research and how to do it. 
\emph{Journal of Health Economics}, \bold{26}, 171--189.
DOI: 10.1016/j.jhealeco.2006.04.002.

Fox J (2005) The R Commander: A Basic Statistics Graphical User Interface to R. 
\emph{Journal of Statistical Software}, 14(9): 1--42. 
DOI: 10.18637/jss.v014.i09.

Fox J (2017) \emph{Using the R Commander: A Point-and-Click Interface for R}.
Chapman and Hall/CRC. 
\url{https://socialsciences.mcmaster.ca/jfox/Books/RCommander/}

Fox J, Bouchet-Valat M (2021) Rcmdr: R Commander.
R package version 2.7-2.
\url{https://socialsciences.mcmaster.ca/jfox/Misc/Rcmdr/}.

Groemping U (2018) R Package DoE.base for Factorial Experiments.
\emph{Journal of Statistical Software}, \bold{85}(5), 1--41.
DOI: 10.18637/jss.v085.i05.

Louviere JJ, Flynn TN, Marley AAJ (2015) \emph{Best-Worst Scaling: 
Theory, Methods and Applications}. Cambridge University Press.
DOI: 10.1017/CBO9781107337855.

Therneau T (2022) survival: Survival Analysis.
R package version 3.3-1.
\url{https://CRAN.R-project.org/package=survival}.

Therneau TM, Grambsch PM (2000) 
\emph{Modeling Survival Data: Extending the Cox Model}.
Springer.
}

\examples{
if(interactive()) {
  library(RcmdrPlugin.BWS2)
}
}

\keyword{
package
}

