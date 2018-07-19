# tfdatalab

A blog project on (intellectual property) statistics in R. The site was built using the [blogdown](https://github.com/rstudio/blogdown) package for R by [Yihui Xie](https://github.com/yihui).

This project is a learning experience for me in making and maintaining a statical website, as well as R and statistical analyses. While I will try to present interesting analyses and code examples, the contents should be taken with a grain of salt, as I have little experience in the matters I deal with here.

The [OECD TPF database](http://www.oecd.org/sti/intellectual-property-statistics-and-analysis.htm) will serve as the major database for investigation. The raw datafiles are not supplied to this GitHub repository, as they are far too large for the upload. Instead they are located in a local subdirectory ./datasource. Manually downloading the datafiles from the OECD and adding them to this subdirectory will enable you to recreate my analyses.

The subdirectory ./scripts contains R scripts used to for data cleaning/transformation operations and analyses that are not carried out in the Rmarkdown content files or only developed for experimentation.

