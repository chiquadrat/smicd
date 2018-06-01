#' Exam scores from inner London
#'
#' Exam scores of 4,059 students from 65 schools in Inner London, as in
#' \code{\link[mlmRev]{Exam}}.
#'
#' @format A data frame with 4059 observations on the following 10 variables:
#' \describe{
#' \item{school}{School ID - a factor.}
#' \item{examsc}{Exam score.}
#' \item{schgend}{School gender - a factor. Levels are mixed, boys, and girls.}
#' \item{schavg}{School average of intake score.}
#' \item{vr}{Student level Verbal Reasoning (VR) score band at intake - a factor.
#' Levels are bottom 25\%, mid 50\%, and top 25\%}
#' \item{intake}{Band of student's intake score - a factor. Levels are bottom
#' 25\%, mid 50\% and top 25\%}
#' \item{standLRT}{Standardised LR test score.}
#' \item{sex}{Sex of the student - levels are F and M.}
#' \item{type}{School type - levels are Mxd and Sngl.}
#' \item{student}{Student id (within school) - a factor}
#' }
#' @references Goldstein, H., Rasbash, J., et al (1993). A multilevel analysis
#' of school examination results. Oxford Review of Education 19: 425-433
#' @source \url{http://multilevel.ioe.ac.uk/softrev/exam.html}
#' @docType data
"Exam"
