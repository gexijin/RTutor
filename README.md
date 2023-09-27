# Analyzing financial data by chatting (RTutor -based)
Hosted at [http://44.204.182.200/](http://44.204.182.200/).  Contact Steven Ge on [Twitter](https://twitter.com/StevenXGe)

RTutor is an AI-based app that can quickly generate and test R code. Powered by API calls to OpenAI's GPT-4, RTutor translates natural languages into R scripts, which are then executed within the Shiny platform. An R Markdown source file and HTML report can be generated. 

## Installation
This repository is updated frequently, sometimes a few times a day. We suggest users reinstall everytime before using it, so that you always have the most recent version.

1. Update R and RStudio to the most recent version. 
2. Install RTutor package
``` r
if (!require("remotes")) {
  install.packages("remotes")
}
library(remotes)
#voice input package heyshiny
install_github("jcrodriguez1989/heyshiny", dependencies = TRUE)
install_github("gexijin/RTutor")
```
3. Install other R packages. If you want to use additional R package for analyzing your data, you should install these in your computer too.
## Obtain an API key from OpenAI
1.  Create a personal account at [OpenAI](https://openai.com/api/).
2.  After logging in, click on **Personal** from top left.
3.  Click **Manage Account** and then **Billing**, where you can add **Payment methods** and set **Usage limits**. $3-$5 per month is more than enough for most people.
4. Click on **API keys** to create a new key, which can be copied.

## Use the API key with RTutor
There are several ways to do this. 
- After the app is started, you can click on **Settings** and paste the API key.
- You can also save this key as a text file called **api_key.txt** in the working directory. 
- Finally, you can create an environment variable called **OPEN_API_KEY**. Instructions for [Windows](https://docs.oracle.com/en/database/oracle/machine-learning/oml4r/1.5.1/oread/creating-and-modifying-environment-variables-on-windows.html), 
[Mac](https://phoenixnap.com/kb/set-environment-variable-mac), and 
[Linux](https://linuxize.com/post/how-to-set-and-list-environment-variables-in-linux/). 

## To start RTutor
```{r example}
RTutor::run_app()
```
## License
(CC BY-NC 3.0) Non-commercial use.

