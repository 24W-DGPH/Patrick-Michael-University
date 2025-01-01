#load necessary packages
pacman::p_load(
  tidyverse,   # Collection of R packages for data science (ggplot2, dplyr, tidyr, etc.)
  data.table,  # Fast data manipulation
  lubridate,   # Date and time handling
  stringr,     # String manipulation
  readr,       # Reading data
  magrittr,    # Pipes (%>%)
  devtools,    # Tools for R package development
  knitr,       # Dynamic report generation
  rmarkdown,   # R Markdown for reproducible reports
  shiny,       # Interactive web applications
  plotly,      # Interactive plots
  tidymodels,  # Tidy machine learning
  readxl,      # Excel sheets
  here,        # Versatile pathways
  janitor,     # Cleaning functions
  googleLanguageR,  # Translating functions 
  openxlsx        #Exports into excel
)
  
   
   library(readxl)
   stats_doc_202412 <- read_excel("stats_doc_202412.xlsm")
   View(stats_doc_202412)
   
   
  dataset <- stats_doc_202412        #identify the dataset for easier manipulation
    
  dataset <- dataset 
  colnames(dataset) <- dataset[4,]   #Clean redundant columns and empty cells
  dataset <- dataset[-c(1,2,3,4),]
  
  dataset <- dataset %>%
    janitor::clean_names() %>%       #Clean and transliterate column names
    
    slice(-c(58:144))  %>%            #Remove empty rows
  
    mutate(across(3:31, as.numeric))    #Changes the value of all relevant columns 
                                           # to numeric instead of character
    
    
    
    #replace clean names with custom intelligeble names
  
  colnames(dataset) <- recode(colnames(dataset)                               ,
      "na"             = "names_dr"     ,
      "na_2"           = "type_mn"      , "khlʿ_lbny"        = "ext_dc"       ,
      "khlʿ_bdly"      = "ext_pm"       , "ghyar"            = "drsng"        ,
      "khlʿ_jrahy"     = "ext_srg"      , "ʿmlyat_jrahyt"    = "surgery"      ,
      "kht_jyr"        = "scaling"      , "ʿmlyat_ltht"      = "perio_srg"    , 
      "hshw_mwqt_lbny" = "fl_dc_temp"   , "hshw_mmlghm_lbny" = "fl_dc_amlg"   ,
      "kmbwzyt_lbny"   = "fl_dc_cmp"    , "zjajy_lbny"       = "fl_dc_gi"     ,
      "ghyar_ʿsb_lbny" = "endo_dc_acc"  , "hshw_ʿsb_lbny"    = "endo_dc"      ,
      "hshw_mwqt_bdly" = "endo_pm_temp" , "hshw_mmlghm_bdly" = "fl_pm_amlg"   ,
      "kmbwzyt_bdly"   = "fl_pm_cmp"    , "zjajy_bdly"       = "fl_pm_gi"     ,
      "hshw_ʿsb_bdly"  = "endo_pm"      , "ghyar_ʿsb_bdly"   = "endo_pm_acc"  ,
      "na_3"           = "meds"         , "na_4"             = "xray"         ,
      "thwyl_kharjy"   = "rfr_ext"      , "thwyl_dakhly"     = "rfr_int"      ,
      "na_5"           = "general"      , "na_6"             = "other"        ,
      "na_7"           = "total"        , "na_8"             = "fl_total"     ,
      "na_9"           = "fl_total_mn"  , "na_10"            = "procedures"
       )
    
    
    
    for (current_col in colnames(dataset)) {
    assign(current_col, dataset[[current_col]]) #Changes column names into objects
    }                                           #for easier manipulation                     
    
 
  
  dataset$names_dr <- ifelse(is.na(dataset$names_dr), lag(dataset$names_dr),
                             dataset$names_dr)     
                                              #Repeat the first name into the 
                                               #two next NA rows
    
    
  dataset$names_dr <- ifelse(is.na(dataset$names_dr), lag(dataset$names_dr,2)
                             ,dataset$names_dr) 
  
  
  
  #Translation and transliteration for names into Latin Characters
    
  dataset$type_mn <-  recode(dataset$type_mn, "اقتصادي مسائي" = "paid_night" ,
                                               "اقتصادي صباحي" = "paid_day"  ,
                                               "مجاني"         = "free"      ) 
   
  dataset$names_dr <- recode(dataset$names_dr                                , 
                             "د.رانيا محفوظ بخيت"   = "Rania M. Bekheit"     ,
                             "د.مارتن ماجد لويس"    = "Martin M. Louis"      ,
                             "د.شيماء زغلول أحمد"   = "Shaima Z. Ahmed"      ,
                             "د.كريستين ماجد الفونس"= "Christine M. Alfons"  ,
                             "د.ساندى عماد منير"    = "Sandy E. Mounir"      ,
                             "د.ياسر عبد الرحمن على"= "Yassir A. Ali"        ,
                             "د.ريم  حسن فارس"      = "Reem H. Fares"        ,
                             "د.ساندرا اشرف موريس"  = "Sandra A. Maurice"    ,
                             "د.دينا عونى محمد"     = "Dina A. Mohamed"      ,
                             "د.محمد اشرف الحسيني"  = "Mohamed A. Alhusseiny",
                             "د.احمد احمد عباس"     = "Ahmed A. Abbas"       ,
                             "د.مارينا سامى صبرى"   = "Marina S. Sabry"      , 
                             "د.شاهنده مجدى محمد"   = "Shahenda M. Mohamed"  ,
                             "د.فاطمه طارق محمد"    = "Fatima T. Mohamed"    ,
                             "د.عمرو عصام الدين"    = "Amr Essameldein"      ,
                             "د.حمزه محمد عاشور"    = "Hamza M. Ashour"      ,
                             "د.تقي ياسر محمد"      = "Toqa Y. Mohamed"      ,
                             "د.سعد مرشد حنا"       = "Saad M. Hanna"        ,
                             "هالة رشدي داود"       = "Hala R. Dawood"       ,
                             "الاجمالي"              = "Total"                )
  
  

  