# Making a package to analyse both fastqc, and alignemts bwamem and bowtie.
# The package is named SeqReport
library(devtools)
use_this::create_package("D:/Raquel/Desktop/SeqReport/")
## conventions:
# variables and fucntions will be snake_case
# lines will have up to 90 characters
# lintr::lint_package() and styler::style_file() are going to be used to check the
# formatation.

# Doing the  version control
# I used GitBash to create the remote origin repository
use_this::use_git()

## first function
# This first funciton will be used to get information from the html file from FastQC
# for all files in a directory and putting them in a table.
## Creating a function file
usethis::use_r("fastqc_group_report")

# Creating an auxiliary function to convert the html table into a dataframe in R
usethis::use_r("readHTMLtable")
# I will rename the function because it has the same name as the XML function.
usethis::rename_files("readHTMLTable", "read_html_table")

# loading all functions
load_all()
