# Talk to your data with RTutor

RTutor is an AI-based Shiny App that can quickly generate and test R code. Powered by API calls to OpenAI, natural languages are translated into R scripts, which are then executed within the Shiny platform. An R Markdown source file and HTML report can be generated. 

## Installation
``` r
library(remotes)
install_github("gexijin/RTutor")
```

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
library(RTutor)
run_app()
```

## Examples
**"Use the mpg data frame.  Use ggplot2 to create a boxplot of hwy vs. class. Color by class."**
```{R}
library(ggplot2)
ggplot(mpg, aes(x=class, y=hwy, fill=class)) +
  geom_boxplot()
```
**"Use the mpg data frame.  Conduct ANOVA of log-transformed hwy by class and drv."**
```{R}
#Load necessary libraries
library(tidyverse)
#Create log-transformed hwy variable
data <- mpg %>% 
  mutate(hwyLog = log(hwy))
#Perform ANOVA
modelResults <- aov(hwyLog ~ class * drv, data = data)
#View output
summary(modelResults)
```
**"Use the mpg data frame.  Create a correlation map of all the columns that contain numbers."**

```{R}
cor_data <- cor(select_if(mpg, is.numeric))
library(corrplot) 
corrplot(cor_data)
```

**"Use the mpg data frame.  hwy and cty represent miles per gallon (MPG) on the highway and in the city, respectively.
Only keep cars more efficient than 15 MPG, but less than 40, on the highway.
Add 0.5 to city MPG for correction.
Perform log transformation on city MPG.
Raise highway MPG to the second power.
Calculate correlation coefficient of  the two transformed variables."**

```{R, eval = FALSE}
# filter for cars more efficient than 15 on the highway, but less than 40
mpg_filtered <- mpg %>% 
    filter(hwy > 15 & hwy < 40)
# add 0.5 to city MPG
mpg_filtered$cty <- mpg_filtered$cty + 0.5
# perform log transformation on city MPG
mpg_filtered$cty <- log(mpg_filtered$cty)
# raise highway MPG to the second power
mpg_filtered$hwy2 <- mpg_filtered$hwy^2
# calculate correlation coefficient
cor(mpg_filtered$hwy2, mpg_filtered$cty)
```
Alternative solution:
```{R}
library(tidyverse)
mpg %>%
  filter(hwy > 15 & hwy < 40) %>%
  mutate(cty = cty + 0.5,
         cty = log(cty),
         hwy = hwy^2) %>%
  summarise(corr = cor(cty, hwy))
```
