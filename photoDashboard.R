library(shiny)
library(shinydashboard)
library(magick)
library(shinyFiles)
library(exiftoolr)
library(tidyverse)
library(shinybusy)
library(lubridate)

  
  exif<-function(in.args, in.path){
    processx::run("./lib/exiftool.exe", args = c(in.args, in.path))
  }
  ## convert tag output to tibble of tags
  ## supply tags as text vector ie: c('-tag1', '-tag2')
  ## if dir then in.dir = T
  tagsToTab<-function(tags = NA, .in.path, in.dir = F){
    if(!in.dir){
      if(is.na(tags[1])){
        exif.out<-exif(c('-s','-s'),in.path = .in.path)$stdout 
      }else{
        exif.out<-exif(c('-s','-s',tags),in.path = .in.path)$stdout
      }
       exif.out <- exif.out %>% 
        str_replace_all('\r\n','", ') %>% 
        str_replace_all(': ', ' = "') %>% 
        str_replace('.{2}$', '')
    
      exif.out<- eval(parse(text = paste('tibble(',exif.out,')'))) 
    }else{
      if(is.na(tags[1])){
        exif.out<-exif(c('-s','-s'),in.path = .in.path)$stdout %>% 
          strsplit("======== ") %>% 
          unlist()
       #writeLines(exif.out) 
        
      }else{
        exif.out<-exif(c('-s','-s',tags),in.path = .in.path)$stdout %>% 
          strsplit("======== ") %>% 
          unlist()
      }
      exif.out<-exif.out[-1]
      exif.out[length(exif.out)]<- exif.out[length(exif.out)] %>% 
        str_replace('\r\n  .*','')
      tab<-exif.out[1] %>% 
        str_replace_all('\r\n','", ') %>% 
        str_replace_all(': ', ' = "') %>% 
        str_replace('.{2}$', '')  
      tab <- eval(parse(text = paste('tibble(','in_file = "', tab,')')))
      for(i in 2:length(exif.out)){
        str<-exif.out[i] %>%
          str_replace_all('\r\n','", ') %>% 
          str_replace_all(': ', ' = "') %>% 
          str_replace('.{2}$', '')
        tab[i,]<-eval(parse(text = paste('list(','in_file = "', str,')'))) 
        
      }
      exif.out<-tab %>% 
        mutate(in_file = basename(in_file))
    }
    colnames(exif.out)<-tolower(colnames(exif.out))
    return(exif.out)
  }
  
  # test<-tagsToTab(tags = NA, .in.path = .in.path, in.dir = T)
  
 

  sidebar <- dashboardSidebar(
    h5('*  navigate to photo directory:'),
    shinyDirButton('folder', 'Select a folder', 'photo directory:', FALSE),
    textOutput('selected_folder'),
    sidebarMenu(
      menuItem("Species Tagging", tabName = "SpTagging"),
      menuItem("Rename Photos", tabName = "Rename")
    )
  )

  body<-dashboardBody(
    tabItems(
      tabItem(tabName = "SpTagging",
              
          fluidRow(
            column(width = 3,
                   box(
                     title = "control", 
                     width = NULL, 
                     status = "primary",
                     
                     ## inputs:::
                     # shinyDirButton('folder', 'Select a folder', 'photo directory:', FALSE),
                     # textOutput('selected_folder'),
                     
                     sliderInput(inputId = "photo.select", label="photo number",
                                 min = 1, max = 100, step = 1,  value = 1),
                     checkboxInput("display_exif", "view exif data", FALSE),
                     htmlOutput('exif_data')
                   ),
            ),
            column(width = 9,
                   box(
                     title = textOutput('image'), 
                     width = NULL, 
                     #status = "primary",
                     imageOutput("myImage")
                   ),
                  ),
                )
              ),
      
      tabItem(tabName = "Rename",
              fluidRow(
                column(width = 4,
                       box(
                         title = "check Userlabel", 
                         width = NULL, 
                         status = "primary",
                         checkboxInput("display_ulab", "view userlabel", FALSE),
                         htmlOutput('userLab')
                       ),
                       box(
                         title = "update userlabel", 
                         width = NULL,
                         checkboxInput("update_ulab", "update userlabel?", FALSE),
                         conditionalPanel(
                              condition = "input.update_ulab",
                              textInput(inputId='ulab_new', label='input new userlabel', value = "", width = NULL, placeholder = NULL),
                              checkboxInput("change_ulab", "change_userlabel?", FALSE),
                              conditionalPanel(
                                condition = "input.change_ulab",
                                actionButton("burn_ulab", 
                                             "update(!)",
                                             style="color: #C93312 ; 
                                             background-color: #74A089; 
                                             border-color: #C93312; 
                                             border-radius: 10px;
                                             border-width: 3px")

                                
                              )
                            )
                       ),
                       box(
                         title = "rename photos", 
                         width = NULL,  
                         checkboxInput("rename_checkbox", "rename photos?", FALSE),
                         conditionalPanel(
                           condition = "input.rename_checkbox",
                           htmlOutput('exif_file'),
                           checkboxInput("rename_checkbox2", "update names?", FALSE),
                           conditionalPanel(
                             condition = "input.rename_checkbox2",
                             actionButton("burn_names", 
                                          "rename photos(!)",
                                          style="color: #C93312 ; 
                                             background-color: #74A089; 
                                             border-color: #C93312; 
                                             border-radius: 10px;
                                             border-width: 3px")
                             
                             
                           )
                         )
                       
                         ## inputs:::
                         # shinyDirButton('folder', 'Select a folder', 'photo directory:', FALSE),
                         # textOutput('selected_folder'),
                         
                    )
                ),
                column(width = 8,
                       box(
                         title = 'photo files', 
                         width = NULL, 
                         #status = "primary",
                         dataTableOutput('photoTab')
                       ),
                ),
              )
        )
      )
   )
  
   
  
  
ui <- dashboardPage(
  
  dashboardHeader(title = 'Meso Photos'),
  sidebar,
  body
  
)


################################################################################################
####
####               server

server <- function(input, output, session) {
  
  shinyDirChoose(input, 'folder', roots=c(wd='.'), filetypes=c('', 'JPG'))
  
  #photo.path<-'./data/in/testPhotos'
  
 ################
  ### reactive elements
  photo.path <- reactive({
    parseDirPath(roots = c(wd = '.'), input$folder)
  })
  
  
  
  
  
  photos<-reactive(list.files(photo.path())) 
  
  n <- reactive(length(photos()))     
  
  image <- reactive(photos()[input$photo.select])
  
  view_exif<-reactive(input$display_exif)
  
  view_ulab<-reactive(input$display_ulab)
  
  ulab<-reactive(input$ulab_new)
  
  rename1<-reactive(input$rename_checkbox)
  
  #image<-image.in()
  
  ################################################################################
  ## text output
  output$selected_folder <- renderText(photo.path())
  
  output$image<-renderText(image()) 
  
  output$photoTab <- renderDataTable(as_tibble(photos()), options = list(pageLength = 10, scrolly = TRUE))
  
  
  observe({

    updateSliderInput(session, inputId = 'photo.select', max = n() )
    
    ## species tagging, show photo data
    if(view_exif()){
      # photo.test<-list.files('./data/in/100RECNX', full.names = T )[1]
      
      # exif.dat<- tagsToTab(tags = c('-filename','-userlabel', '-createDate'), .in.path = photo.test, in.dir = F)
      exif.dat<- tagsToTab(tags = c('-filename','-userlabel', '-createDate'), 
                           .in.path = file.path(photo.path(),image()), 
                            in.dir = F)

      
      output$exif_data <- renderUI({
        str1 <- paste("file: ", exif.dat[['filename']])
        str2 <- paste("user label: ", exif.dat[['userlabel']])
        str3 <- paste("photo date: ", exif.dat[['creatdate']])
        HTML(paste(str1, str2, str3,sep = '<br/>'))
      })
    }else{
      output$exif_data <- renderUI({
        str1 <- ''
        str2 <- ''
        str3 <- ''
        HTML(paste(str1, str2, str3,sep = '<br/>'))
      })
    }
    
    #cat(file=stderr(),  file.path(photo.path(),photos())[1])
    ### photo nameing: show user label
    if(view_ulab()){
      
      # ulab.photos<-list.files(photo.path, full.names = T)
      ulab.photos<-file.path(photo.path(),photos())

      u.lab<-unique(tagsToTab(tags = c('-userlabel'), .in.path = ulab.photos, in.dir = T)[['userlabel']])
      output$userLab <- renderUI({
        HTML('userlabel: ',u.lab,'<br/>')

      })
    }else{
      output$userLab <- renderUI({
        HTML('<br/>')
      })
    }
  
  
     if(rename1()){
       # photo.test<-list.files('./data/in/100RECNX', full.names = T )[2]
       # rename.str<-c('-filename<${userlabel;s/-/_/g;s/ /_/g;s/,/_/g}_${datetimeoriginal}_${sequence;s/ of 10//g;s/^([1-9])$/0$1/g;s/0 of 0/00/g}_${filename;s/\\D+//g}.JPG',
       #               "-d",
       #               '%Y%m%d_%H%M%S'
       # )
       show_modal_spinner()
       in.photo<-list.files(photo.path(),full.names = T)[1]
       #in.photo<-list.files('./data/in/100RECNX', full.names = T )[2]
       test.names<-tagsToTab(tags = c('-userlabel','-datetimeoriginal','-sequence','-filename'), .in.path = in.photo, in.dir = F)
       # testname.str<-c('-testname<${userlabel;s/-/_/g;s/ /_/g;s/,/_/g}_${datetimeoriginal}_${sequence;s/ of 10//g;s/^([1-9])$/0$1/g;s/0 of 0/00/g}_${filename;s/\\D+//g}.JPG',
       #               "-d",
       #               '%Y%m%d_%H%M%S'
       # )
  
  
       output$exif_file <- renderUI({
         str1<-"file 1 example name:"
         str2<-test.names[['userlabel']] %>% 
           str_replace('-','_') %>% 
           str_replace(' ', '_')
         str3<-test.names[['datetimeoriginal']] %>% 
           ymd_hms() %>% 
           format('%Y%m%d_%H%M%S')
         str4<-test.names[['sequence']] %>% 
           str_replace(" of 10", "") %>% 
           if_else(nchar(.) == 1, paste0('0',.), .) %>% 
           str_replace("0 of 0", '00')
         str5<-(test.names[['filename']]) %>% 
           str_replace('\\D+', '')
         HTML(str1, paste(str2,str3,str4,str5,sep = '_'), sep = '<br/>')
         
       })
     }else{
       output$exif_file <- renderUI({
         HTML("")
    
    
       })
     }
    remove_modal_spinner()
  })
  #########
  ## update user label:
  ## test.photo<-file.path(photo.path,photos[1])
  #observe(cat(file=stderr(),  paste0('UserLabel=',ulab())))
  observeEvent(input$burn_ulab, {
    ## update userlabel..
    # exif_call(args = '-UserLabel',   path = test.photo)
    # test.input = '20791 P1'
    # test.call <- exif_call(args = paste0('-userlabel=',test.input), path= test.photo)
    show_modal_spinner() # show the modal window

    #ulab.path<-photo.path
    ulab.path<-photo.path()
    ulab.in<-ulab()
    
    #ulab.in<-'12345_1'
    
    #exif.out<-exif_call(args = paste0('-UserLabel=',ulab()), path = file.path(photo.path(),photos()))
    exif.out<-exif(in.args= paste0('-userlabel=',ulab.in), in.path = ulab.path)
    # tagsToTab(tags = '-userlabel')
    remove.files<-list.files(path = ulab.path,full.names = T, pattern = '_original')
    unlink(remove.files)
    updateCheckboxInput(
      session =  session,
      inputId =  "change_ulab", 
      value = FALSE
    )
    remove_modal_spinner()
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = exif.out$stdout)
    # 
    })
  
  observeEvent(input$burn_names, {
    ## update userlabel..
    # exif_call(args = '-UserLabel',   path = test.photo)
    # test.input = '20791 P1'
    # test.call <- exif_call(args = paste0('-userlabel=',test.input), path= test.photo)
    show_modal_spinner() # show the modal window
    
    #ulab.path<-'./data/in/100RECNX'
    ulab.path<-photo.path()
    ulab.in<-ulab()
    #ulab.in<-'12345_1'
    
    rename.str<-c('-filename<${userlabel;s/-/_/g;s/ /_/g;s/,/_/g}_${datetimeoriginal}_${sequence;s/ of 10//g;s/^([1-9])$/0$1/g;s/0 of 0/00/g}_${filename;s/\\D+//g}.JPG',
                  "-d",
                  '%Y%m%d_%H%M%S'
    )
    #exif.out<-exif_call(args = paste0('-UserLabel=',ulab()), path = file.path(photo.path(),photos()))
    exif.out<-exif(in.args = rename.str, in.path = ulab.path)
    # tagsToTab(tags = '-userlabel')
    updateCheckboxInput(
      session =  session,
      inputId =  "rename_checkbox2", 
      value = FALSE
    )
    remove_modal_spinner()
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = exif.out$stdout)
    # 
  })
  
  ## rename photos string:
   #'-filename<${userlabel;s/-/_/g;s/ /_/g;s/,/_/g}_${datetimeoriginal}_${sequence;s/ of 10//g;s/^([1-9])$/0$1/g;s/0 of 0/00/g}_${filename;s/\D+//g}.JPG' -d "%Y%m%d_%H%M%S"
   #'
   ##############################
  ####   rename photos:
  
  
  
##########################################################
  ### view photos
  output$myImage <- renderImage({
    
    
    
    image.dat<-image_info(image_read(file.path(photo.path(),image())))
    #str(image.dat)
    width.o<-image.dat$width
    height.o<-image.dat$height  

    # expression will re-run whenever they change.
    width  <- session$clientData$output_myImage_width
    height <- session$clientData$output_myImage_height
    ratio<-width/width.o

    filename <- normalizePath(file.path(photo.path(),
                                        image()))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         #alt = paste("Image number", input$n)),
         width = width,
         height = round(height.o*ratio)
    )
    
  }, deleteFile = FALSE)
  # if(is.null(image.in)){
  #   
  # }else{
  
  
  
  # A dynamically-sized plot
  
  # }
  
  
  # This code reimplements many of the features of `renderPlot()`.
  # The effect of this code is very similar to:
  # renderPlot({
  #   hist(rnorm(input$obs))
  # })
  
}



shinyApp(ui, server)