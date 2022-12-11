# RTutor, Chat with your data, in English!

RTutor is a Shiny App that can quickly generating and testing R code. Powered by API call's to OpenAI's language models, natural languages are translated into R scripts, which are then executed within the Shiny platform. An R Markdown source file and HTML report can be generated. 

## Installation


``` r
library(remotes)
install_github("gexijin/RTutor")
```

## Obtain an API key from OpenAI
1.  Create an account at [OpenAI](https://openai.com/api/).
2.  After logging in, click on **Personal** from top left.
3.  Click **Manage Account** and then **Billing**, where you can add **Payment methods** and set **Usage limits**. $5 per month is more than enough.
4. Click on **API keys** to create a new key, which can be copied.

## Use the API key with RTutor
There are several ways to do this. 
- After the app is started, you can click on **Settings** and paste the API key.
- You can also save this key as a text file called **api_key.txt** in the working directory. 
- Finally, you can create an environment variable called **OPEN_API_KEY** in your Windows, Mac, or Linux OS. 

## To start RTutor
```{r example}
library(RTutor)
run_app()
```
