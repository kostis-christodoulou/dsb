1.  Make sure you are able to pull and push from a github repo

-   Install Git locally <https://happygitwithr.com/install-git.html>
-   Introduce yourself to Git: <https://happygitwithr.com/hello-git.html>
-   Personal Access Tokens for HTTPS: <https://happygitwithr.com/https-pat.html>

2.  In github, create a blank repo called `my_website`, initialise it with a README file and copy its URL

3.  In RStudio, navigate to `File -> New Project -> Version Control -> Git`.

-   Paste your copied link into the "Repository URL:" box.
-   Choose the project path ("Create project as subdirectory of:") and click Create Project.

4.  IN RStudio install the package `blogdown`. Once it installs, run `blogdown::install_hugo()`

5.  Once Hugo installs, make a template website using theme forty by typing in the console

<!-- -->

        blogdown::new_site(theme = "MarcusVirg/forty", 
          sample = TRUE, 
          theme_example = TRUE,            
          empty_dirs = TRUE,            
          to_yaml = TRUE)

6.  Go to the Files section in the bottom right of RStudio, open `netlify.toml`, **delete its contents** and paste the following. The `"0.111.3"` refers to the hugo version you are using-- find out which one you have by typing `blogdown::hugo_version()` in the console

<!-- -->

    [build] 
      publish = "public"
      command = "hugo"

    [build.environment]
      HUGO_VERSION = "0.111.3"
      HUGO_ENABLEGITINFO = "true"

    [context.production.environment]
      HUGO_ENV = "production"
      
    [context.branch-deploy.environment]
      HUGO_VERSION = "0.111.3"

    [context.deploy-preview.environment]
      HUGO_VERSION = "0.111.3"  

7.  Do a basic customisation, by editing the `config.yaml` file

- First add these lines, with the exact identation at the very end of your config.yaml

<!-- -->

markup:
  goldmark:
    renderer:
      unsafe: true  


-Line 3: change the title to, e.g., title: Portfolio website for Kostis Christodoulou

-Line 13: Add your real name and short description

-Lines 11-12. If you don't want a subtitle, comment out that line with a hashtag \# -- Do not delete stuff, just comment it out

-Lines 15-30: Change your contact details: address (lines 17-19), email (line 24), phone (line 28). Please do not change line 15

-Line 37 onwards: Change what appears at the footer (bottom) of each webpage. If there is something you do not want to add, just comment it out with hashtags \# -Line 44 onward: Change the social media links to correspond to your own accounts. If you don't want something, comment it out with a hashtag \#-- Do not delete stuff, just comment it out.

-Line 62 onwards: navigation When a user clicks on the menu, there are four choices, defined in lines 70, 72, 74, 76. If you only want to keep, e.g, two choices, comment out with \# everything from lines 74-77 -BY default, we also get two buttons in lines 63-68.

-Line 81 onwards: This is where you define six tiles that appear on your website-- these could be six blog posts, six projects, or whatever you like. Let us look at the first one in detail:

-   Line 86: title: Aliquam -- this is the title of your tile shown in larger font size

-   Line 85: subtitle: Ipsum Dolor Sit Amet -- a subtitle, with smaller font size

-   Line 84: image: pic01.jpg - what image do you wish to use as background-- you can find them and change them, in the themes\forty\static\img directory

-   Line 86: url: blogs/aliquam - short hand URL address that the particular tile links to

8.  Sign up at `netlify.com` using your github account. Once you sign up (you can skip personalisation), click on the green button `New site from Git`. Once you authorise (again!) github, choose the repo you created tor the website. It may be empty now, as we haven't pushed everything from our local PC to github but that's ok.

9.  Netlify will build the website and come back with a unique name/ address-- when I tried it, my website was named `compassionate-galileo-051845`. Go to `Site Settings` and change site name to something that makes more sense, like `kostis-portfolio`

10. Go to line 1 of your `config.yaml` and change the baseURL to `baseURL: https://kostis-portfolio.netlify.app/` It's important you use secure http `https://` and you end your address with a `/`

11. You have changed config.yaml and netlify.toml. Save both of them and let us push everything to github by going to the terminal and typing the following four commands:

<!-- -->

    - git add -A
    - git commit -m "initial commit"
    - git pull
    - git push

12. To change the background pictures of the tiles, save any jpg/jpeg/png files you want in the `\themes\forty\static\img` folder. I have saved an LBS pic, called `lbs.jpg` in this directory, so please copy lbs.jpg to the `\themes\forty\static\img` folder. Then change `config.yaml` and to make the first tile picture be one ob LBS and not of NYC at night, change line 84 from `pic01.jpg` to `lbs.jpg`

13. Currently all tiles go to markdown (.md) documents. But what if we wanted to use an Rmd? I have saved one in this folder, `risk_return.Rmd`, but please copy it under `content/blogs`. It is a normal Rmd, the only difference is the front matter, the first 13 lines. In there, I have made the following changes

-   Line 8: added an image, called `spices.jpg`. You should save that picture in `\static\img\blogs`. Again, acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work
-   Line 11: changed the `slug` to `risk_return`. The slug is the shorthand URL address. but make sure you have no spaces in the slug. And how do I use the slug? Well, I need to edit line 87 of config.yaml and change it from `url: blogs/aliquam` to `url: blogs/risk_return`


14. If you want to add another picture `pic1.jpg` anywhere in the Rmd, make sure you save the picture in `\static\img` and insert a chunk of code as follows:

```
knitr::include_graphics("/img/pic1.jpg",error=FALSE)
```

15. If you want to add a datafile that you use in one of your Rmds, say brexit_results.csv, create a folder `\data\` in the root folder of your website (where you also have the *.Rproj file). Save the brexit_results.csv file in that folder and to read it, use 

```
brexit_results <- read_csv(here::here("data","brexit_results.csv"))
```

16. Save your config.yaml (again!)

17: In the console, type `blogdown::serve_site()` Blogdown will knit the Rmd and use hugo to build the entire webpage. Once it's done, you should be able to see it in the local viewer. Usually, if you add/make changes to Rmds, `blogdown::serve_site()` will automatically knit them to HTML and render the site; if not, restart R (Ctrl/Cmd + Shift + F10) and type in the console `blogdown::serve_site()`.

18. Push everything to github. You must push your changes by going through all of the steps below and Netlify will update your webpage


    - git add -A
    - git commit -m "initial commit"
    - git pull
    - git push
