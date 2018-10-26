# tfdatalab

A blog project on intellectual property statistics in R (and possibly other topics in the future). The site was built using the [blogdown](https://github.com/rstudio/blogdown) package for R by [Yihui Xie](https://github.com/yihui).

This project is a learning experience for me in making and maintaining a statical website, as well as R and statistics. While I will try to present interesting and accurate analyses, the contents should be taken with a grain of salt, as I have little experience in the matters I deal with here.

The [OECD TPF database](http://www.oecd.org/sti/intellectual-property-statistics-and-analysis.htm) will serve as the major data source investigated here. The raw datafiles are not supplied to this GitHub repository, as they are too large for the upload. Instead they are located in a local subdirectory ./datasource. Manually downloading the datafiles from the OECD and adding them to this subdirectory will enable you to recreate the complete website.

The subdirectory ./scripts contains R scripts used to for data cleaning/transformation and experimental analyses that are not included in the Rmarkdown content files.

