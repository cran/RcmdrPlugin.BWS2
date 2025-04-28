\name{bws2Response}
\alias{bws2Response}
\alias{bws2ResponseSet}

\title{Collect Responses to BWS2 Questions}

\description{
Collect user's responses to BWS2 questions.
}

\details{
Three types of dialog boxes are provided to collect user's responses to
BWS2 questions that are converted from the choice sets designed in 
the \sQuote{\code{Design Choice Sets for BWS2}} dialog box.

The first dialog box (\sQuote{\code{Set Options for BWS2 Response Collection}})
sets two options for response collections:

\itemize{
\item \code{Design}: An R object name containing BWS2 choice sets
generated in the \sQuote{\code{Design Choice Sets for BWS2}} dialog box. 
The default name \sQuote{\code{BWS2design}} is displayed automatically.
The correct name should be entered if the name of BWS2 choice sets
(\code{BWS2design}) was changed in the dialog box for BWS2 design.

\item \code{Save to file}: When the user's responses should be saved
to an external file in CSV format (\code{.csv}), check this box.
}

After confirming all the settings on the first dialog box, press
the \sQuote{\code{OK}} button to continue.

A series of dialog boxes (\sQuote{\code{Collect Responses to BWS2 Questions}})
sequentially opens to collect the user's responses to BWS2 questions. 
In each dialog box,
levels designed in the \sQuote{\code{Design Choice Sets for BWS2}}
dialog box are presented. The user is asked to select their best and worst
levels from the drop-down list boxes. After making the selection,
the user clicks the \sQuote{\code{OK}} button to open a new dialog box
corresponding to the next BWS2 question.

When all user responses to BWS2 questions have been collected, a dialog
box (\sQuote{\code{Save As}}) opens if the \sQuote{\code{Save to file}}
option was selected in the first dialog box. This saves the user's
responses to an external file in CSV format.
}
