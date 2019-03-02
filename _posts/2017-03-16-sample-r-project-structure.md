---
title: A sample R project structure
author: manos_parzakonis
published : true
excerpt: Kickstarting an R project with reproducibility in mind. Sample folder structure and organization.
categories:
- measure
tags:
- model
- script
- R
- structure
- organization

---


# Intro
Is it common for you to just deep dive into writing an R Script and few iterations below to realize that this is actually something that you will use more frequently than you thought in the beginning? Well, this is not rare and it actually is the reason why you will need to actually embrace [Git](https://git-scm.com/) in your work-flow.

*Note* : This is a theme we will not touch in this note, but you can find a lot of generic and specialized guides into using it. Take my word for it ; YOU NEED IT! Checkout [Git and GitHub](http://r-pkgs.had.co.nz/git.html) ; the related chapter from *Hadley Wickham's* book '[R Packages](http://r-pkgs.had.co.nz/)'.

Our aim is to have a sample project to kickstart each new project we undertake as fast as possible and standardized.

# First things first
In this take we are using the following folder structure :

```bash
~
- Data              \..
- Output            \..
- Configuration     \..
  - \   project_settings.R
  - \   global_settings.R
- Images            \..  
- Output            \..  
- Queries           \..  
- Reports           \..  
  - \   report.md
```
At the root of the project we will have the following files

```bash
~
- .gitignore
- readme.md
- requirements.R
```

# What's in it?

While we can work with scripts and then render them as `knitr` reports we will opt for using an [R Notebook](http://rmarkdown.rstudio.com/r_notebooks.html) that will reside in `~/Reports/sample_report.md`. The [R Notebook](http://rmarkdown.rstudio.com/r_notebooks.html) is preferable because we can use snippets for `SQL`, `Python`, `C++`, `JavaScript` and even `Bash` along `R`.


## Create a Notebook

The header of the [R Notebook](http://rmarkdown.rstudio.com/r_notebooks.html) will handle basic sourcing of libraries, functions and specific project settings we will need in the progress.

``` r
```{r init stuff, message=FALSE, warning=FALSE, include=FALSE}
## Assuming your working directory is sample-r-project's below
## Required packages
source("./requirements.R")

## Functions needed
source("./Functions/functions.R")

## Theme(s) for ggthemr
source("./Configuration/custom_ggthemr.R")

## Project settings
source("./Configuration/project_settings.R")

## Authentication with googleapis -----------------------------------
options(
  googleAuthR.scopes.selected =
    c(
      "https://www.googleapis.com/auth/webmasters",
      "https://www.googleapis.com/auth/analytics",
      "https://www.googleapis.com/auth/analytics.readonly",
      "https://www.googleapis.com/auth/devstorage.full_control",
      "https://www.googleapis.com/auth/cloud-platform",
      "https://www.googleapis.com/auth/bigquery",
      "https://www.googleapis.com/auth/bigquery.insertdata"
    )
)

googleAuthR::gar_auth()
# acc.list <- google_analytics_account_list()
```
Notebook chunks can be inserted quickly using the keyboard shortcut `Ctrl + Alt + I` and can be executed using `Ctrl + Shift * Enter`. Since we've set `FALSE` into messages,warnings and output in the chunk initialization above we will get nothing below the chunk after execution.

Let's break down now the above, that will pretty much be the all we need.

### [requirements.R](https://github.com/IronistM/sample-r-project/blob/master/requirements.R)
This is a "loan" from Python's workflow. Each library we need is within this script and we will add/remove libraries within it, so it has a pretty easy structure.

```r
## This is our requirements.R script
## TODO : include an `InstalledPackage` fallback
require(googleAnalyticsR)
require(lubridate)
require(dataframes2xls)
require(ggthemr)
require(plyr)
require(dplyr) # need to load plyr before dplyr & not the other way!
require(tidyr)
require(stringr)
# require(bigQueryR)
require(janitor)
```

### [functions.R](https://github.com/IronistM/sample-r-project/blob/master/Functions/functions.R)

Functions that we'll need later on are defined here. For example an InstalledPackage() and a ggpie() function is defined.

```r
## Check and install package availability
InstalledPackage <- function(package)
{
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)))
  missing <- package[!available]
  if (length(missing) > 0) return(FALSE)
  return(TRUE)
}

CRANChoosen <- function()
{
  return(getOption("repos")["CRAN"] != "@CRAN@")
}

UsePackage <- function(package, defaultCRANmirror = "https://cran.rstudio.com/")
{
  if(!InstalledPackage(package))
  {
    if(!CRANChoosen())
    {       
      chooseCRANmirror()
      if(!CRANChoosen())
      {
        options(repos = c(CRAN = defaultCRANmirror))
      }
    }

    suppressMessages(suppressWarnings(install.packages(package)))
    if(!InstalledPackage(package)) return(FALSE)
  }
  return(TRUE)
}

## Define a ggpie function
# ggpie: draws a pie chart.
# give it:
# * `data`: your dataframe
# * `by` {character}: the name of the fill column (factor)
# * `totals` {character}: the name of the column that tracks
#    the time spent per level of `by` (percentages work too).
# returns: a plot object.
ggpie <- function (data, by, totals, main=NA, pal=NA) {
  p = ggplot(data, aes_string(x=factor(1), y=totals, fill=by)) +
    geom_bar(width=1, stat='identity', color='black') +
    guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
    coord_polar(theta='y') +
    theme(axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.title=element_blank(),
          panel.grid=element_blank()) +
    scale_y_continuous(breaks=cumsum(data[[totals]]) - data[[totals]] / 2, labels=data[[by]]) +
    theme(panel.background = element_rect(fill = "white"))
  if (!is.na(pal[1])) {
    p = p + scale_fill_manual(values=pal)
  }
  if (!is.na(main)) {
    p = p + ggtitle(main)
  }
  p
}
```
### [custom_ggthemr.R](https://github.com/IronistM/sample-r-project/blob/master/Configuration/custom_ggthemr.R)

Here we define a custom theme for [ggthemr](https://github.com/cttobin/ggthemr) that we can use in the graphs (if we like!).

```r
## Replicating Tableau colour pallete
require(ggthemr)
tableau_colours <- c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD', '#8C564B', '#CFECF9', '#7F7F7F', '#BCBD22', '#17BECF')
## you have to add a colour at the start of your palette for outlining boxes, we'll use a grey:
tableau_colours <- c("#555555", tableau_colours)
## remove previous effects:
ggthemr_reset()
## Define colours for your figures with define_palette
tableau <- define_palette(
  swatch = tableau_colours, # colours for plotting points and bars
  gradient = c(lower = tableau_colours[1L], upper = tableau_colours[2L]), #upper and lower colours for continuous colours
  background = "#EEEEEE" #defining a grey-ish background
)
## set the theme for your figures:
# ggthemr(palette = tableau, layout ='minimal')

```

###  [project_settings.R](https://github.com/IronistM/sample-r-project/blob/master/Configuration/project_settings.R)


In this script we define parameters that basically override the global settings of R.

```r
# Things you might want to change

options(show.signif.stars=FALSE)

# options(papersize="a4")
# options(editor="notepad")
# options(pager="internal")

# set the default help type
# options(help_type="text")
options(help_type="html")

# set a site library
# .Library.site <- file.path(chartr("\\", "/", R.home()), "site-library")

# set a CRAN mirror
options("repos" = c(CRAN = "https://cran.rstudio.com/"))

## Set theme default for ggthemr. We comment this out since we will set
## the option within project_settings.R!
# ggthemr(palette = "flat", layout = "minimal")
```

# TL;DR
Now, every time we need to start a new project we can just clone the repository and start working on it

```shell
$ git clone git@github.com:IronistM/sample-r-project.git
```
Note that you can clone and rename the repository at once or simple rename the folder as you would via the Explorer

```shell
$ git clone git@github.com:IronistM/sample-r-project.git MY_NEW_PROJECT
```

# Summary
While there are many projects on Project management, like [ProjectTemplate](http://projecttemplate.net/), I still find this approach more appealing as it seems more minimal. You can modify it to match your needs and likes as you wish, so drop me acomment if something cracking is missing!

Happy Analyzing,  
Manos
