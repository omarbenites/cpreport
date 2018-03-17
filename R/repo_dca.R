#' Automatic report for a Completely Randomized Design (CRD)
#'
#' Produces an automatic report for selected traits in an experiment with a CRD.
#' @param traits The traits to analize.
#' @param geno The genotypes.
#' @param data The name of the data frame.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Omar Benites.
#' @details It fits a linear model for a CRD and explains the results.
#'
#' Under the assumption of fixed effects an ANOVA table is computed. If the ANOVA
#' results in a significant value then the Tukey HSD method for pairwise differences
#' is applied. Assumptions of the model are evaluated with residual plots.
#'
#' Under the assumption of random effects the model is estimated using REML and the
#' variance components are shown.
#' @return It returns an explanation about the CRD fitted model.
# @examples
# repo_dca(c("trw", "vw"), "geno", pjpz09)
# 
# # With a small data set
# temp <- pjpz09[1:18, ]
# repo_dca(c("trw", "vw", "crw"), "geno", temp)
# @import st4gi
#' @importFrom utils browseURL
#' @export

repo_dca <- function(traits, geno, data, maxp = 0.1,
                     title = "Reporte Automatico para el Diseño Completamente al Azar",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf")) {
  
  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "cpreport")
  
  fileRmd <- paste(dirfiles, "/rmd/dca.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/dca.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/dca.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/dca.pdf", sep = "")
  
  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  geno = geno,
                                  data = data,
                                  maxp = maxp,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))
  
  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}