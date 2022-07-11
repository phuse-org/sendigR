library(reticulate)
# python 'XPTCleaner' module wrapped in the package

#' Create json file for vocabulary mappings.
#' Keys are synonyms and values are the CDISC Controlled Terminology Submission values.
#' Vocabularies are defined by column values from the tab-delimited files.
#' @param in_file Mandatory.\cr
#'   List of tab-delimited files with synonyms and preferred terms.
#' @param out_path Mandatory.\cr
#'  output json filename. \cr

#' @export
#' @examples
#' \dontrun{
#' gen_vocab(list(infile1, infile2),jsonfile)
#' }
gen_vocab <- function(in_file, out_path) {
  XPTClean$gen_vocab(in_file, out_path)
}
#' Standardizes SEND xpt files using CDISC controlled terminologies
#' @param input_xpt_dir Mandatory.\cr
#'   input folder name with xpt files under the folder.\cr
#' @param output_xpt_dir Mandatory.\cr
#'  output folder name for writing the cleaned xpt files. \cr
#' @param json_file Mandatory.\cr
#'  json filename used for mapping. \cr
#' @export
standardize_file <- function(input_xpt_dir, output_xpt_dir, json_file) {
  XPTClean$standardize_file(input_xpt_dir, output_xpt_dir, json_file)
}
