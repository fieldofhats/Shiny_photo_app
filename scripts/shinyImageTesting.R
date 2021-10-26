library(shiny)
library(magick)
library(shinyFiles)
library(exiftoolr)

ui <- pageWithSidebar(
  headerPanel("renderImage example"),
  sidebarPanel(
    shinyDirButton('folder', 'Select a folder', 'photo directory:', FALSE),
    textOutput('selected_folder'),
 
    sliderInput(inputId = "photo.select", label="photo number",
                min = 1, max = 100, step = 1,  value = 1),
    #verbatimTextOutput('exif_data')
    checkboxInput("display_exif", "view exif data", FALSE),
    htmlOutput('exif_data')
    # conditionalPanel(
    #   condition = "distplay_exif == TRUE",
    #    htmlOutput('exif_data')
    # ),
    # conditionalPanel(
    #   condition = "distplay_exif == FALSE",
    #   htmlOutput('exif_data')
    # )
    
    # selectInput(
    #   inputId = 'photo.select',
    #   label = 'select photo',
    #   choices = ''
    # )
    #selectizeInput(inputId = 'photo.select', label = 'select photo', choices = NULL)
    
  ),
  mainPanel(
    # Use imageOutput to place the image on the page
    imageOutput("myImage")
  )
)


server <- function(input, output, session) {
  
  shinyDirChoose(input, 'folder', roots=c(wd='..'), filetypes=c('', 'JPG'))
  
  #photo.path<-'../data/in/testPhotos'
  
  observe({
      cat(file=stderr(),  parseDirPath(roots = c(wd = '..'), input$folder))
  })

 
    photo.path <- reactive({
      parseDirPath(roots = c(wd = '..'), input$folder)
    })
    

    
    output$selected_folder <- renderText({ 
      photo.path()
    })

  # photo.path<-'./data/in/testPhotos'

  
   #updateSelectizeInput(session, 'photo.select', choices = photos, server = TRUE) 

    photos<-reactive({
      list.files(photo.path())
    }) 
    
    n <- reactive({
      length(photos())
    })     
    
  
    image <- reactive({ 
      photos()[input$photo.select]
    })
    
    view_exif<-reactive({
      input$display_exif
    })
    #image<-image.in()
    observe({
      
      updateSliderInput(session, inputId = 'photo.select', max = n() )
      if(view_exif()){
              exif.dat<-exif_read(file.path(photo.path(),image()))
      output$exif_data <- renderUI({
        str1 <- paste("file: ", exif.dat[['FileName']])
        str2 <- paste("user label: ", exif.dat[['UserLabel']])
        str3 <- paste("photo date: ", exif.dat[['CreateDate']])
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
      
    })

      
      
    
    output$myImage <- renderImage({


      
      image.dat<-image_info(image_read(file.path(photo.path(),image())))
    #str(image.dat)
      width.o<-image.dat$width
      height.o<-image.dat$height  
      
      
      # test.image<-'./data/in/testPhotos/20791_P2_20180711_134316_01_0001.JPG'
      # exif.dat<-exif_read('./data/in/testPhotos/20791_P2_20180711_134316_01_0001.JPG')
      # names(exif.dat)
     # exif_read XMP-JobRef
      
    
     
 

         # exif_call(args = c("-n", "-j", "-q", "-*GPS*"), path = image_files[1])
     # exif_call(args = '-xmp:all' , path = test.image)
     # exif_call(args = '-xmp-jobref:name = "test"' , path = test.image)
      # Read myImage's width and height. These are reactive values, so this
      # expression will re-run whenever they change.
      width  <- session$clientData$output_myImage_width
      height <- session$clientData$output_myImage_height
      ratio<-width/width.o
      
      # For high-res displays, this will be greater than 1
      #pixelratio <- session$clientData$pixelratio
      
    
      
      # Generate the image file
    
        # When input$n is 3, filename is ./images/image3.jpeg
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