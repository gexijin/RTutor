# RTutor, Chat with your data, in English!

RTutor is a Shiny App that can quickly generating and testing R code. Powered by API call's to OpenAI's language models, natural languages are translated into R scripts, which are then executed within the Shiny platform. An R Markdown source file and HTML report can be generated. 

## Installation


``` r
library(remotes)
install_github("gexijin/RTutor")
```

## Setting up the API key

This is a basic example which shows you how to solve a common problem:

```{r example}
library(RTutor)
run_app()
```
