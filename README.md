# RTutor.ai - Talk to your data via AI

<!-- badges: start -->
[![R-CMD-check](https://github.com/gexijin/RTutor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gexijin/RTutor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package hosted at [RTutor.ai](https://RTutor.ai). <br>
Contact Steven Ge on [LinkedIn](https://www.linkedin.com/in/steven-ge-ab016947/) or [Twitter.](https://twitter.com/StevenXGe) Reach the RTutor team by email, ge@orditus.com.

No code? No problem. Analyze data with simple, natural language. Upload your data, ask questions, and get results in seconds!

RTutor is an AI-based app that can quickly generate and test R code. Powered by API calls to OpenAI's ChatGPT or other models, RTutor translates natural languages into R scripts, which are then executed within the Shiny platform. An R Markdown source file and HTML report can be generated. 

# Video tutorial
We highly recommend that users watch this 10-min [YouTube video.](https://youtu.be/a-bZW26nK9k)

# Install R Package

This repository is updated frequently. We suggest users reinstall everytime before use, so that you have the most recent version. Visit our [Github Wiki.](https://github.com/gexijin/RTutor/wiki) for additional information

### 1. Update R and RStudio to the most recent version.
### 2. Install the RTutor package
Run this code in RStudio:
``` r
# Install 'remotes' package
if (!require("remotes")) {
  install.packages("remotes")
}
library(remotes)

# Install 'heyshiny' package, for voice input
install_github("jcrodriguez1989/heyshiny", dependencies = TRUE)

# Install 'RTutor' package
install_github("gexijin/RTutor")
```

### 3. Install other R packages when prompted.
If you'd like to use additional R packages for analyzing your data, you should install those on your computer too.   
   
### 4. Obtain an API key from OpenAI
> 1. Create a personal account at [OpenAI](https://platform.openai.com/docs/overview).
> 2. After logging in, click 'Quickstart' at the sidebar on the left.
> 3. Click 'Create an API key in the dashboard here' underneath the heading 'Create and export an API key'
> 4. Click 'Create a new secret key' to create a key, which can be copied.    
> Currently, the first $5 of usage is free. If you exceed this, add a payment method to your account.    
> Then navigate to 'Usage' at the sidebar on the left and set usage limits. $3-$5 per month is more than enough for most people.

### 5. Use the API key with RTutor
There are several ways to do this:   
   
> * Once RTutor is running, click on the 'Settings' tab and paste in your API key.
> * You can also save this key as a text file called "api_key.txt" in the working directory.
> * Or, you can create an environment variable called "OPEN_API_KEY" where the value is your API key. Here are the instructions for [Windows](https://docs.oracle.com/en/database/oracle/machine-learning/oml4r/1.5.1/oread/creating-and-modifying-environment-variables-on-windows.html), [Mac](https://phoenixnap.com/kb/set-environment-variable-mac), and [Linux](https://linuxize.com/post/how-to-set-and-list-environment-variables-in-linux/).   
   
### 6. To start RTutor
Run the following code in RStudio:
``` r
library(RTutor)
run_app()
```



# License
(CC BY-NC 3.0) Non-commercial use.
