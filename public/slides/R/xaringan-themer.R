
## Xaringna themer
style_mono_accent(   
   # Colors
   base_color = "#4D4D4E",
   black_color = "#272822",
   white_color = "#FFFFFF",
    
   # Fonts
   base_font_size = "30px",
   
   # Text
   text_font_size = "1rem",
   text_font_google = google_font('Sen'),
   
   # Header
   header_font_google  = google_font('Noto Serif JP', '700'),
   
   # H1
   header_h1_font_size = "1.50rem",
   # H2
   header_h2_font_size = "1.25rem",
   # H3
   header_h3_font_size = "1rem",

   # Title slide
   title_slide_background_color = '#FFFFFF',
   #title_slide_background_image = "https://raw.githubusercontent.com/fabbiocrux/Figures/main/Inedit/INEDIT-logo-00.jpg",
   title_slide_background_size = '80%',
   title_slide_background_position = '50% 15%',
   title_slide_text_color = "#4D4D4E",
   
   
   # Slide content
   #header_background_auto = 'FALSE',
   #header_background_color = '#FFFFFF',
   #header_background_text_color = "#1381B0",
   #header_background_padding = '0px 0px',
   #header_background_content_padding_top = "5px",
   padding = '25px 50px 10px 25px',
   
   # Footnote
   footnote_position_bottom = '0px',
   footnote_font_size = "60%",

   colors = c(
      red = "#f34213",
      purple = "#3e2f5b",
      orange = "#ff8811",
      green = "#d9edc2",
      white = "#FFFFFF",
      grey = "#F5F5F5",
      yellow = "#FAFE83",
      blue = "#1381B0",
      grey_Inedit = "#969696"
   ),
   
   extra_css = list(
      
      # Changing the Title H1
      ".remark-slide-content > h1:first-of-type" = list(
         "margin-top" = "5px",
         "padding-bottom" = "7px",
         "border-bottom" = '1px solid #EB811B'
      ),
      
      ".remark-slide-content h1" = list(
         "margin-bottom" = 0, 
         "margin-top" = 0),
      
      
      # Changing the Title H2
      ".remark-slide-content > h2:first-of-type" = list(
         "margin" = "0px",
         "color" = "#EB811B"
      #    "padding-bottom" = "7px",
      #    "border-bottom" = '1px solid #EB811B'
       ),
      
      # Changing the Title H3
      ".remark-slide-content > h3:first-of-type" = list(
         "margin-top" = "5px",
         "margin-bottom" = "10px"
      ),
      
      ".remark-slide-content > p" = list(
         "margin-top" = "2px",
         "margin-bottom" = "2px"
      ),
      
      ".remark-slide-content > ul" = list(
         "margin-top" = "2px",
         "margin-bottom" = "2px"
      ),
      
      ".small" = list("font-size" = "90%"),
      ".Large , .Large .remark-code, .Large .remark-inline-code" = list("font-size"= "144%"), 
      ".large , .large .remark-code, .large .remark-inline-code" = list("font-size"= "120%"), 
      ".small , .small .remark-code, .small .remark-inline-code" = list("font-size"= "90%"), 
      ".footnotesize, 
    .footnotesize .remark-code, .footnotesize .remark-inline-code" = list( "font-size"= "80%"),
      
      ".scriptsize , .scriptsize .remark-code, .scriptsize .remark-inline-code" = list("font-size"= "70%"),
      ".tiny , .tiny .remark-code, .tiny .remark-inline-code" = list("font-size"= "60%"),
      

      ".fn" = list(
         "vertical-align" = "super",
         "font-size" = "60%",
         "color" = "purple"
      ),
      
      
      # Custom classes for Pull Left/Right
      
      ".pull-left-2"= list(
      "float"= "left",
      "width"= '60%'
      ),
      
      ".pull-right-1"= list(
         "float"= "right",
         "width"= '36%'
      ),
      
      ".pull-left-1"= list(
         "float"= "left",
         "width"= '36%'
      ),
      ".pull-right-2"= list(
         "float"= "right",
         "width"= '60%'
      ),
      ".pull-down" = list (
         "position" = "absolute",
         "bottom" =  "50px"
      )
   ),
   outfile = here("Fabio.css")
)


