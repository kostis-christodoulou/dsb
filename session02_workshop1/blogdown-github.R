# Make sure you are able to pull and push from a github repo
# Install Git locally https://happygitwithr.com/install-git.html

# Introduce yourself to Git: https://happygitwithr.com/hello-git.html
# In the RStudio console type the following

install.packages("usethis")
library(usethis)
use_git_config(user.name = "Jane Doe", #your github user name
               user.email = "jane@example.org") # the email you used when you signed upo with github

# Personal Access Tokens for HTTPS: https://happygitwithr.com/https-pat.html

# In the RStudio console type the following
usethis::create_github_token()

# Accept defaults and click “Generate token”.
# Copy the generated PAT to your clipboard. Or leave that browser window open and available 
# for a while, so you can come back to copy the PAT.

# Store the PAT explicitly. In the RStudio console type the following gitcreds::gitcreds_set() to get a prompt where you can paste your PAT:
gitcreds::gitcreds_set()

## Install blogodown, hugop, and find out your hugo version

# In the RStudio console type the following
install.packages("blogdown")
library(blogdown)
install_hugo
hugo_version()

# Once Hugo installs, make a template website using theme forty by typing in the console

blogdown::new_site(theme = "MarcusVirg/forty", 
                   sample = TRUE, 
                   theme_example = TRUE,            
                   empty_dirs = TRUE,            
                   to_yaml = TRUE)

# once default site builds, go to RStudio's Files section in the bottom right, open `netlify.toml`, **delete its contents** and paste the following. 
[build] 
publish = "public"
command = "hugo"

[build.environment]
HUGO_VERSION = "0.101.0"
HUGO_ENABLEGITINFO = "true"

[context.production.environment]
HUGO_ENV = "production"

[context.branch-deploy.environment]
HUGO_VERSION = "0.101.0"

[context.deploy-preview.environment]
HUGO_VERSION = "0.101.0"  


#  "0.101.0" refers to the hugo version you are using-- find out which one you have by typing `blogdown::hugo_version()` in the console


