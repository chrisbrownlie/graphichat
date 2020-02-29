# graphichat
An R Shiny app for visualising Whatsapp chat data

# Background
This project was started off the back of [an article I published on Medium](https://towardsdatascience.com/3-5-years-of-a-relationship-in-whatsapp-messages-4f4c95073c9d?source=friends_link&sk=d3e19170b2215e6ee5b3d13a80235177), where I analysed 3.5 years of data from my Whatsapp chat with my girlfriend. The article was extremely well received and I had many people contact me, asking how they could do the same.
The analysis in the original article was produced using R, so I decided to develop an app using the [shiny package](shiny.rstudio.com) so that other people could investigate their own chat data.

# UI flow
The app functions best when accessed on a computer - I am working on a more mobile-friendly version. The app works by asking the user to upload data from their Whatsapp chat. This can be in the form of the zip file which is the default, or the actual .txt file that is exported within the zip file. The app can handle either form of the data.
Once the file is uploaded, the user can specify alternate names for the chat participants and assign a colour to each, which will be used for distinguishing between them in the generated report.
When the user then clicks the 'Generate report' button, a series of visualisation and statistics are calculated and produced show a report-style page. Most of the visualisations have associated options to allow user to analyse their data however they wish.

# Repo contents
- ui.R, server.R and global.R files for the specification of the app
- custom.css for specifying custom css classes used in the UI
- renv files for package dependency management

# The app
The app is currently being hosted on shinyapps.io, [at this link here](https://chrisbrownlie.shinyapps.io/graphichat). Please try the app out and feel free to suggest any ways it could be improved.
I am currently working on making a more mobile-friendly version and in the future I will be producing a fully functioning mobile app.
