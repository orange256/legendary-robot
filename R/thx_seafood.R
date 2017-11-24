thx_seafood <- function(data, model, eta, ksi,
                        method = c(full, marsh, cross, lms),
                        product = NULL,
                        DV = NULL,
                        ID = NULL,
                        missing_value = NULL,
                        data_format = NULL,
                        save_data = TRUE){
  library(lavaan)
  library(magrittr)
  library(dplyr)
  library(MplusAutomation)

  # [1] construct CFA model --------------------------------------------------

  # [1-1] full & marsh & cross
  if(method == 'full' || method == 'marsh' || method == 'cross'){
    Eta_formula <- paste(eta, collapse = " + ") %>% paste("Eta =~",.)
    Ksi_formula <- paste(ksi, collapse = " + ") %>% paste("Ksi =~",.)
    CFA_model <- paste(model, Eta_formula, Ksi_formula, sep = '\n')

    # If user has missing flag, then we will replace all missing_value to NA
    if(!is.null(missing_value)){
      data[data == missing_value] <- NA
    }
  }

  # [1-2] lms
  if(method == "lms"){
    Eta_formula <- paste(eta, collapse = " ") %>% paste("Eta by", ., ";\n")
    Ksi_formula <- paste(ksi, collapse = " ") %>% paste("Ksi by", ., ";\n")
    CFA_model <- gsub("=~","by",CFA_model) %>% gsub("\\+","",.) %>% gsub("\n",";\n
                                                                         ",.)
    # If user has missing flag, then we will replace all missing_value to -9999
    if(!is.null(missing_value)){
      data[data == missing_value] <- -9999
    }
    data[is.na(data)] <- -9999
  }
  # (1-end) ----------------------------------------------------------------


  # [2] Run CFA model with original data (without interaction term) ------------------------------

  # [2-1] full & marsh & cross
  if(method == 'full' || method == 'marsh' || method == 'cross'){

    # set ID variable (if necessary)
    if(!is.null(ID)){
      ID_var <- data[ID]
    }

    # CFA
    fit_tmp <- cfa(CFA_model, data=data, missing = 'fiml')

    ##  Get factor scores from model fit , and save as data.frame
    fscore_tmp <- fit_tmp %>% predict %>% as.data.frame

    # construct interaction term formula
    inter <-  sapply(1:length(ksi), function(i) {paste(eta, ksi[i], sep = "*") %>%
        paste(paste0(eta, ksi[i]) ,. , sep = ' = ')}) %>%
      as.vector()

    interaction_formula <- paste(inter, collapse = ",")

    ##  compute interaction term, and save as new_data.
    new_data <- eval(parse(text = paste("fscore_tmp %>% mutate(", interaction_formula, ") %>% cbind.data.frame(data, .)")))
  }

  # [2-2] lms
  if(method == "lms"){

    # set path (default)
    Mplus_file <- "D:/Mplus_in_R"
    # create folder
    dir.create(Mplus_file, showWarnings = FALSE)

    # create data
    my_dat_path <- paste0(Mplus_file, "/test1.dat")
    #prepareMplusData(data[, !(colnames(data) %in% c(ID))], filename = my_dat_path)
    prepareMplusData(data, filename = my_dat_path)
  }

  # (2-end) ---------------------------------------------------------------------------------------


  # [3] Run CFA model with new data (with interaction term) ---------------------------------------

  # [3-1] full model (default)
  if(method == 'full'){
    EtaKsi_formula <- do.call(paste0, expand.grid(eta, ksi)) %>%
      paste(., collapse = " + ") %>%
      paste("EtaKsi =~",.)
  }

  # [3-2] marsh model
  if(method == "marsh"){

    # compute the length of eta & ksi, choose the smaller one.
    n <- min(length(eta), length(ksi))

    EtaKsi_formula <- sapply(1:n, function(i){paste0(eta[i], ksi[i])}) %>%
      paste(., collapse = " + ") %>%
      paste("EtaKsi =~",.)
  }

  # [3-3] cross-product
  if(method == "cross"){
    EtaKsi_formula <- paste0(product) %>% paste(., collapse = " + ") %>% paste("EtaKsi =~",.)
  }

  # run CFA model, and get fit
  if(method == 'full' || method == 'marsh' || method == 'cross'){
    CFA_2_model <- paste(model, Eta_formula, Ksi_formula, EtaKsi_formula, sep = '\n')

    ##  Run CFA model (with new data) , and return the model fit
    fit <- cfa(CFA_2_model, data = new_data, missing = 'fiml')
  }


  # [3-4] Run LMS CFA model in Mplus
  if(method == "lms"){

    # set format
    if(!is.null(data_format)){
      M_format <- paste0("format is ", data_format, ";")
    }else{M_format <- " "}

    if(!is.null(ID)){
      M_id <- paste0("IDVARIABLE IS ", ID, ";")
    }else{M_id <- " "}

    # construct model
    Mplus_syntax <- paste(
      "[[init]]
      iterators= iter;
      iter = 1:1;
      filename =",'"LMS_[[iter]].inp";',
      '
      outputDirectory="D:/Mplus_in_R";',
      "
      [[/init]]
      ",

      "
      TITLE:	Second-Order Factor Analysis-True Model",
      '
      DATA:		FILE IS "', paste0(Mplus_file, "/test[[iter]].dat"), '";
      ',M_format,
      "

      VARIABLE:	NAMES ARE ", paste(colnames(data), collapse=" "), ";",
      "MISSING ARE ALL (-9999);
      ",M_id,"

      ANALYSIS:  	TYPE = RANDOM;
      ALGORITHM=INTEGRATION;",

      "
      MODEL:
      ",
      CFA_model, # first order CFA model
      paste(  (colnames(data)[which(colnames(data)!=c(ID,DV))]), collapse=" "), ";
      ",

      Eta_formula,"     ",
      Ksi_formula,
      "
      EtaKsi |Eta xwith Ksi;
      "
      ,
      DV ,"on Eta Ksi EtaKsi;",

      "
      [Eta@0];",
      "
      [Ksi@0];",

      "

      SAVEDATA:",
      '
      File = Mplus_LMS_op[[iter]].dat;',
      "
      FORMAT IS F8.3;",
      "
      SAVE = FSCORES;"
    )

    # write Mplus syntax to .txt file
    write.table(Mplus_syntax, file=paste0(Mplus_file, "/Mplus_LMS.txt"), col.names = F,row.names=FALSE, sep="\t", quote=FALSE)

    # use this .txt file to create Mplus '.inp' file
    createModels( paste0(Mplus_file, "/Mplus_LMS.txt") )

    # run this .inp file
    runModels(Mplus_file)
  }


  # (3-end) ---------------------------------------------------------------------------------------


  # [4] Get results --------------------------------------------

  # [4-1] full & marsh & cross
  if(method == 'full' || method == 'marsh' || method == 'cross'){

    if(is.null(ID)){
      #Get factor scores from model fit , and save as data.frame
      fscore <- fit %>% predict %>% as.data.frame

      ##  Construct interaction term (full model)
      final_data <- eval(parse(text = paste("fscore %>% mutate(", interaction_formula, ") %>% cbind.data.frame(data, .)")))

      # save fit & final data as list
      result_list <- list('fit'=fit, 'fscore_2inter'=final_data)
    }
    else{
      #Get factor scores from model fit , and save as data.frame
      fscore <- fit %>% predict %>% as.data.frame %>% cbind.data.frame(ID_var,.)

      ##  Construct interaction term (full model)
      final_data <- eval(parse(text = paste("fscore %>% mutate(", interaction_formula, ") %>% merge(data, ., by = ID)")))

      # save fit & final data as list
      result_list <- list('fit'=fit, 'fscore_2inter'=final_data)
    }

  }

  # [4-2] lms
  if(method == "lms"){
    final_data <- getSavedata_Data(paste0(Mplus_file, '/lms_1.out'))

    summaryStats <- extractModelSummaries(paste0(Mplus_file, '/lms_1.out'))

    modelResults <- extractModelParameters(paste0(Mplus_file, '/lms_1.out'))

    # save fit & final data as list
    result_list <- list('Summaries'=summaryStats, 'Parameters'=modelResults, 'fscore_2inter'=final_data)

    # if user don't want to save the mplus result, then we will delete it
    if(save_data!=TRUE) {unlink("D:/Mplus_in_R", recursive = T,force = T)}

  }

  # function return
  return(result_list)
  # (4-end) -------------------------------------------------------------

}
# end
