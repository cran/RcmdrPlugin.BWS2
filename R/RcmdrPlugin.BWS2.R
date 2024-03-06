###############################################################################

.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

###############################################################################

bws2Design <- function() {
  initializeDialog(title = gettextRcmdr("Design Choice Sets for BWS2"))
  defaults <- list(
    designName         = "BWS2design",
    attributelevelName = "BWS2attributes",
    RNGseedName        = "",
    RNGoptionVariable  = "0",
    saveVariable       = "0")
  dialog.values <- getDialog("bws2Design", defaults)

  if(is.null(getDialog("bws2Design"))) putRcmdr("savedTableAttributes", NULL)
  
  ##### Output Frame #####
  outputFrame     <- tkframe(top)
  designFrame     <- tkframe(outputFrame)
  attributesFrame <- tkframe(outputFrame)
  saveFrame       <- tkframe(outputFrame)

  # Choice sets
  designName <- tclVar(dialog.values$designName)
  design     <- ttkentry(designFrame, width = "13", 
                         textvariable = designName)

  # Attributes and levels
  attributelevelName <- tclVar(dialog.values$attributelevelName)
  attributelevel     <- ttkentry(attributesFrame, width = "20",
                                 textvariable = attributelevelName)

  # Save
  saveVariable <- tclVar(dialog.values$saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)
  

  ##### Input Frame #####
  inputsFrame       <- tkframe(top)
  AltBlkRngFrame    <- tkframe(inputsFrame)
  RNGoptionFrame    <- tkframe(inputsFrame)
  TABLEFrame        <- tkframe(inputsFrame)
  tableFrame        <- tkframe(TABLEFrame)

  # Seed for RNG
  RNGseedName <- tclVar(dialog.values$RNGseedName)
  RNGseed     <- ttkentry(AltBlkRngFrame,
                          width = "10",
                          textvariable = RNGseedName)

  # RNG option
  RNGoptionVariable <- tclVar(dialog.values$RNGoptionVariable)
  RNGoptionCheckBox <- ttkcheckbutton(RNGoptionFrame,
                                      variable = RNGoptionVariable)

  # Table for attributes and levels
  ## Initial settings
  env <- environment()
  assign(".tableFrame", tkframe(tableFrame), envir = env)
  tkdestroy(get(".tableFrame", envir = env))
  assign(".tableFrame", tkframe(tableFrame), envir = env)
  nrows <- 6
  ncols <- 7

  initial.table <- getRcmdr("savedTableAttributes")

  ## Names of columns
  make.col.names <- "labelRcmdr(.tableFrame, text='')"
  col.varname <- c("Attribute", "Level 1", "Level 2", "Level 3",
                                "Level 4", "Level 5", "Level 6")
  for (j in 1:ncols) {
    make.col.names <- 
      paste(make.col.names, ", ",
            "labelRcmdr(.tableFrame, text = '", col.varname[j], "')",
            sep = "")
  }
  eval(parse(text=paste("tkgrid(", make.col.names, ", sticky = 'w')", 
                        sep = "")), envir = env)

  ## Names of rows and cells in table
  for (i in 1:nrows) {
    varname <- paste(".tab.", i, ".1", sep = "")
    assign(varname, if (is.null(initial.table)) {
                      tclVar("")
                    } else {
                      tclVar(initial.table[i, 1])
                    }, envir = env)
    row.varname <- paste(".rowname.", i, sep = "")
    make.row <- paste("labelRcmdr(.tableFrame, text ='')")
    make.row <- paste(make.row, ", ",
                      "ttkentry(.tableFrame, width = '15', textvariable =", 
                      varname, ")", sep="")
    for (j in 2:ncols) {
      varname <- paste(".tab.", i, ".", j, sep = "")
      assign(varname, if (is.null(initial.table)) {
                        tclVar("")
                      } else {
                        tclVar(initial.table[i, j])
                      }, envir = env)
      make.row <- paste(make.row, ", ",
                        "ttkentry(.tableFrame, width = '11', textvariable =", 
                        varname, ")", sep="")
    }
    eval(parse(text = paste("tkgrid(", make.row, ")", sep = "")), envir = env)
  }
  tkgrid(get(".tableFrame", envir = env), sticky = "w")


  ##### onOK Function #####
  onOK <- function() {

    putDialog("bws2Design", list(
      designName         = tclvalue(designName),
      attributelevelName = tclvalue(attributelevelName),
      RNGseedName        = tclvalue(RNGseedName),
      RNGoptionVariable  = tclvalue(RNGoptionVariable),
      saveVariable       = tclvalue(saveVariable)))

    closeDialog()

    # Table of attributes and levels
    nrows <- 6
    ncols <- 7
    varNames <- matrix("", nrow = nrows, ncol = ncols)

    for (i in 1:nrows) {
      for (j in 1:ncols) {
        varname <- paste(".tab.", i, ".", j, sep = "")
        varNames[i, j] <- 
          eval(parse(text =
            paste("as.character(tclvalue(", varname, "))", sep = "")))
      }
    }

    # Store the table of attributes and levels into savedTableAttributes 
    putRcmdr("savedTableAttributes", varNames) 

    # Variables for attributes and levels
    attributeNames <- varNames[, 1]
    varidRows      <- which(attributeNames != "")
    nrows          <- length(varidRows)
    attributeNames <- attributeNames[varidRows]
    levelNames     <- varNames[varidRows, -1]

    attribute.names.list <- vector("list", nrows)

    for (i in 1:nrows) {
      levelnames <- levelNames[i, ]
      levelnames <- levelnames[levelnames != ""]
      attribute.names.list[[i]] <- levelnames
    }

    # Code for argument 'attribute.names'
    cmd.attributes <- paste("list(", attributeNames[1], " = ",
                            attribute.names.list[1], sep = "")
    for (i in 2:nrows) {
      cmd.attributes <- paste(cmd.attributes, ", ", attributeNames[i], " = ",
                              attribute.names.list[i], sep = "")
    }
    cmd.attributes <- paste(cmd.attributes, ")", sep = "")

    # Code for argument 'seed'
    if (is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste(", seed = NULL))", sep = "")
    } else {
      cmd.seed <- paste(", seed = ",  as.numeric(tclvalue(RNGseedName)), 
                        "))", sep = "")
    }

      cmd.cateA <- paste("c('", paste(na.omit(attributeNames), 
                                      collapse = "', '"),
                         "')", sep = "")
      cmd.contA <- paste("''")

    # Randomize order of runs
    if (tclvalue(RNGoptionVariable) == 1) {
      cmd.randomize <- paste(", randomize = TRUE")
    } else {
      cmd.randomize <- paste(", randomize = FALSE")
    }

    # Code for nlevels
    nlevels <- sapply(attribute.names.list, length)

    # Design choice sets
    doItAndPrint(paste0(tclvalue(attributelevelName), " <- ", cmd.attributes))
    doItAndPrint(
      paste(tclvalue(designName), " <- data.matrix(DoE.base::oa.design(",
            "nlevels = c(", paste(nlevels, collapse = ", "), ")",
            cmd.randomize,
            cmd.seed, sep = ""))
    doItAndPrint(paste(tclvalue(designName)))
    
    # Save choice sets and attributes and levels
    if (tclvalue(saveVariable) == 1) {
      saveFile <- tclvalue(tkgetSaveFile(
        filetype = gettextRcmdr(
          '{"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}'),
        defaultextension = ".rda",
        initialfile = paste0(tclvalue(designName), ".rda"),
        parent = CommanderWindow()))
      if (saveFile == "") {
        tkfocus(CommanderWindow())
        return()
      }
      cmd <- paste0('save(', tclvalue(designName),
                    ', ', tclvalue(attributelevelName),
                    ', file = "', saveFile, '")')
      justDoIt(cmd)
      logger(cmd)
      Message(paste0(gettextRcmdr(
            "BWS2 design and attributes-and-levels were exported to file: "),
          saveFile),
        type = "note")
    }
    
    tkfocus(CommanderWindow())
  }

  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "bws2Design",
               reset       = "resetBws2Table",
               apply       = "bws2Design")

  # Output
  tkgrid(labelRcmdr(designFrame,
                    text = gettextRcmdr("Name for design ")),
         design, sticky = "nw")
  tkgrid(labelRcmdr(attributesFrame,
                    text = gettextRcmdr("Name for attributes and levels ")),
         labelRcmdr(attributesFrame, text = tclvalue(attributelevelName),
                    relief = "solid", foreground = "green"),
         sticky = "nw")
  tkgrid(saveCheckBox,
         labelRcmdr(saveFrame,
                    text = gettextRcmdr("Save to file")),
         sticky = "nw")
  tkgrid(designFrame,     labelRcmdr(outputFrame, text = "   "),
         attributesFrame, labelRcmdr(outputFrame, text = "   "),
         saveFrame,       sticky = "nw")
  tkgrid(outputFrame, sticky = "nw")

  # Blank line
  tkgrid(labelRcmdr(top, text = ""))

  # Input
  ## Table
  tkgrid(labelRcmdr(
           inputsFrame,
           text = gettextRcmdr("Attributes and their levels:")),
         sticky = "w")
  tkgrid(tableFrame, sticky = "ew")
  tkgrid(TABLEFrame, sticky = "ew")
    
  ## RNG option
  tkgrid(RNGoptionCheckBox,
         labelRcmdr(
           RNGoptionFrame,
           text = gettextRcmdr("Randomize the order of sets")),
         sticky = "w")
  tkgrid(RNGoptionFrame, sticky = "w")
  
  ## Seed for RNG
  tkgrid(labelRcmdr(
           AltBlkRngFrame,
           text = gettextRcmdr("Reproducibility:")),
         sticky = "w")
  tkgrid(labelRcmdr(
           AltBlkRngFrame,
           text = gettextRcmdr("Seed for random number generator (optional) ")),
         RNGseed, sticky = "w")
  tkgrid(AltBlkRngFrame, sticky = "w")

  tkgrid(inputsFrame, sticky = "w")

  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

resetBws2Table <- function() {
  putRcmdr("savedTableAttributes", NULL)
  putDialog("bws2Design", NULL)
  bws2Design()
}

###############################################################################

bws2Questions <- function() {
  initializeDialog(title = gettextRcmdr("Display BWS2 Questions"))
  defaults <- list(designName         = "BWS2design",
                   attributelevelName = "BWS2attributes",
                   ini.positiontype   = "left")
  dialog.values <- getDialog("bws2Questions", defaults)


  ##### Input Frame #####
  inputsFrame     <- tkframe(top)
  designFrame     <- tkframe(inputsFrame)
  attributesFrame <- tkframe(inputsFrame)
  positionFrame   <- tkframe(top)

  # Choice sets
  designName <- tclVar(dialog.values$designName)
  design     <- ttkentry(designFrame, width = "13",
                         textvariable = designName)

  # Attributes and levels
  attributelevelName <- tclVar(dialog.values$attributelevelName)
  attributelevel     <- ttkentry(attributesFrame, width = "13",
                                 textvariable = attributelevelName)

  # position of attribute column
  radioButtons(positionFrame,
    name = "positiontype",
    title   = gettextRcmdr("Position of attribute column"),
    buttons = c("left", "center", "right"),
    values  = c("left", "center", "right"),
    labels  = gettextRcmdr(c("Left", "Center", "Right")),
    initialValue = dialog.values$ini.positiontype)


  ##### onOK Function #####
  onOK <- function() {
    putDialog("bws2Questions",
              list(ini.positiontype   = tclvalue(positiontypeVariable),
                   attributelevelName = tclvalue(attributelevelName), 
                   designName         = tclvalue(designName)))

    designValue <- tclvalue(designName)

    closeDialog()

    doItAndPrint(paste("bws2.questionnaire(choice.sets = ", designValue,
                       ", attribute.levels = ", tclvalue(attributelevelName),
                       ", position = '", tclvalue(positiontypeVariable), "')",
                       sep = ""))
    tkfocus(CommanderWindow())
  }

  
  ##### Specification of dialog box #####
  # Ok Cancel Help Buttons 
  OKCancelHelp(helpSubject = "bws2Questions",
               reset       = "bws2Questions",
               apply       = NULL)

  # Design
  tkgrid(labelRcmdr(designFrame,
                    text = gettextRcmdr("Design ")),
         design, sticky = "w")
  # Attributes and levels
  tkgrid(labelRcmdr(attributesFrame, text = gettextRcmdr("Attributes and levels ")),
         labelRcmdr(attributesFrame, text = tclvalue(attributelevelName),
                    relief = "solid", foreground = "green"),
         sticky = "w")
  tkgrid(designFrame, labelRcmdr(inputsFrame, text = "   "),
         attributesFrame, sticky = "w")
  tkgrid(inputsFrame, sticky = "w")
  # Position
  tkgrid(positiontypeFrame, sticky = "w")
  tkgrid(positionFrame, sticky = "w")
  
  # OK Cancel Help Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

###############################################################################
bws2Dataset <- function() {
  initializeDialog(
    title = gettextRcmdr("Create Data Set for BWS2 Analysis"))
  defaults <- list(
    ini.baseAttribute      = "<no variable selected>",
    ini.baseLevels         = "NULL",
    ini.reverseAttributes  = "0",
    ini.responsetype       = 1,
    ini.modeltype          = "paired",
    ini.rowsValue          = "4",
    ini.datasetName        = "BWS2data",
    ini.designName         = "BWS2design",
    ini.idName             = "id",
    ini.letterRB           = "1",
    attributelevelName     = "BWS2attributes",
    saveVariable           = "0")
  dialog.values <- getDialog("bws2Dataset", defaults)


  ###### Output frame
  outputFrame      <- tkframe(top)
  datasetnameFrame <- tkframe(outputFrame)
  saveFrame        <- tkframe(outputFrame)

  # Output name
  datasetName <- tclVar(dialog.values$ini.datasetName)
  dataset     <- ttkentry(datasetnameFrame, width = "14",
                          textvariable = datasetName)

  # Save
  saveVariable <- tclVar(dialog.values$saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)


  ###### Inputs frame
  inputsFrame <- tkframe(top)

  ### Frame in left
  leftFrame          <- tkframe(inputsFrame)
  objectsFrame       <- tkframe(leftFrame)
  radio1Frame        <- tkframe(leftFrame)
  radio2Frame        <- tkframe(leftFrame)
  radio3Frame        <- tkframe(leftFrame)
  chkbuttonFrame     <- tkframe(leftFrame)
  baseAttributeFrame <- tkframe(leftFrame)
  baseLevelsFrame    <- tkframe(leftFrame)
  baseLevelFrame1    <- tkframe(leftFrame)

  nAlts <- length(BWS2attributes)
  for (i in 1:nAlts){
    eval(parse(text = paste("baseLevelBox", i,
                            " <- variableComboBox(baseLevelFrame1",
                            ", variableList = BWS2attributes[[", i,
                            "]], title = '')",
                            sep = "")))
  }

  # choice.sets
  designName <- tclVar(dialog.values$ini.designName)
  design     <- ttkentry(objectsFrame, width = "13",
                         textvariable = designName)

  # Attributes and levels
  attributelevelName <- tclVar(dialog.values$attributelevelName)
  attributelevel     <- ttkentry(objectsFrame, width = "13",
                                 textvariable = attributelevelName)

  # id
  idName <- tclVar(dialog.values$ini.idName)
  id <- ttkentry(objectsFrame, width = "13", textvariable = idName)

  # response.type
  radioButtons(radio1Frame, 
    name    = "responsetype",
    buttons = c("rowNumber", "itemNumber"),
    values  = c(1, 2),
    labels  = gettextRcmdr(c("Row number format", "Item number format")),
    initialValue = dialog.values$ini.responsetype,
    title   = gettextRcmdr("Response variable format"))

  # model
  radioButtons(radio3Frame,
    name = "modeltype",
    title   = gettextRcmdr("Model type"),
    buttons = c("paired", "marginal", "sequential"),
    values  = c("paired", "marginal", "sequential"),
    labels  = gettextRcmdr(c("Paired model", "Marginal model",
                             "Marginal sequential model")),
    initialValue = dialog.values$ini.modeltype)

  # Reverse attributes
  reverseAttributesVariable <- tclVar(dialog.values$ini.reverseAttributes)
  reverseAttributesCheckBox <- ttkcheckbutton(chkbuttonFrame,
                                          variable = reverseAttributesVariable)

  # Base attribute
  baseAttributeBox <- variableComboBox(
    baseLevelFrame1, 
    variableList = names(BWS2attributes),
    initialSelection = dialog.values$ini.baseAttribute,
    title = "")

  ### Frame in right
  rightFrame  <- tkframe(inputsFrame)
  letterFrame <- tkframe(rightFrame)
  tableFrame  <- tkframe(rightFrame)
  rowsFrame   <- tkframe(rightFrame)


  # Table
  env <- environment()
  assign(".tableFrame", tkframe(tableFrame), envir = env)

  setUpTable <- function(...){
    tkdestroy(get(".tableFrame", envir = env))
    assign(".tableFrame", tkframe(tableFrame), envir = env)
    nrows <- as.numeric(tclvalue(rowsValue))
    ncols <- 2

    # Set colnames
    make.col.names <- "labelRcmdr(.tableFrame, text='')"
    for (j in 1:ncols) {
      if (j == 1) {
        col.varname <- "Best"
      } else {
        col.varname <- "Worst"
      }
      make.col.names <- 
        paste(make.col.names, ", ",
              "labelRcmdr(.tableFrame, text = '", col.varname, "')",
              sep = "")
    }
    eval(parse(text=paste("tkgrid(", make.col.names, ", sticky = 'w')", 
                          sep = "")), envir = env)

    # Make rows for questions
    for (i in 1:nrows){
      if (tclvalue(lettertypeVariable) == "1") {
        b <- "B"
        w <- "W"
      } else if (tclvalue(lettertypeVariable) == "2") {
        b <- "b"
        w <- "w"
      } else {
        b <- ""
        w <- ""
      }

      Bvarname <- paste(".tab.", i, ".1", sep = "")
      if (is.null(ini.table)) {
        if (tclvalue(lettertypeVariable) == "3") {
          eval(parse(text = paste("assign(Bvarname, tclVar(''), envir = env)",
                                  sep = "")))
        } else {
          eval(parse(text = paste("assign(Bvarname, tclVar('", b, i,
                                  "'), envir = env)", sep = "")))
        }
      } else {
        eval(parse(text = paste("assign(Bvarname, tclVar(ini.table[", i,
                                ", 1]), envir = env)", sep = "")))
      }

      Wvarname <- paste(".tab.", i, ".2", sep = "")
      if (is.null(ini.table)) {
        if (tclvalue(lettertypeVariable) == "3") {
          eval(parse(text = paste("assign(Wvarname, tclVar(''), envir = env)",
                                  sep = "")))
        } else {
          eval(parse(text = paste("assign(Wvarname, tclVar('", w, i,
                                  "'), envir = env)", sep = "")))
        }
      } else {
        eval(parse(text = paste("assign(Wvarname, tclVar(ini.table[", i,
                                ", 2]), envir = env)", sep = "")))
      }

      row.varname <- paste("Q", i, sep = "")

      make.row <- paste("labelRcmdr(.tableFrame, text = '", row.varname,
                        "')", sep = "")
      make.row <- paste(make.row, ", ", 
                        "ttkentry(.tableFrame, width = '10', 
                        textvariable = ", Bvarname, ")", sep = "")
      make.row <- paste(make.row, ", ",
                        "ttkentry(.tableFrame, width = '10', 
                        textvariable = ", Wvarname, ")", sep = "")
      eval(parse(text=paste("tkgrid(", make.row, ", sticky = 'w')",
                            sep = "")), envir = env)
    }

    tkgrid(get(".tableFrame", envir = env), sticky = "ew", padx = 6)
  }

  ini.table <- getRcmdr("savedTable")

  # Slider
  if (is.null(ini.table)) {
    rowsValue <- tclVar(dialog.values$ini.rowsValue)
  } else {
    rowsValue <- tclVar(nrow(ini.table))
  }
  rowsSlider <- tkscale(rowsFrame, from = 4, to = 21, showvalue = FALSE,
                        variable = rowsValue, resolution = 1, 
                        orient = "horizontal", command = setUpTable)
  rowsShow   <- labelRcmdr(rowsFrame, textvariable = rowsValue, width = 3,
                           justify = "right")

  # letter
  radioButtons(letterFrame,
    name = "lettertype",
    title   = gettextRcmdr("Letters of best- and worst-response variables"),
    buttons = c("Uppercase", "Lowercase", "None"),
    values  = c("1", "2", "3"),
    labels  = gettextRcmdr(c("Uppercase", "Lowercase", "None")),
    initialValue = dialog.values$ini.letterRB,
    command = setUpTable)


  onOK <- function() {

    BaseAttributeVar <- getSelection(baseAttributeBox)

    BaseLevelList <- vector("list", nAlts)
    names(BaseLevelList) <- names(BWS2attributes)
    for (i in 1:nAlts){
      eval(parse(text = paste(
        "BaseLevelVar", i, 
        " <- getSelection(baseLevelBox", i, ")",
        sep = "")))
      eval(parse(text = paste(
        "BaseLevelList[[", i, "]] <- BaseLevelVar", i,
        sep = "")))
    }

    if(all(!unlist(BaseLevelList) == "<no variable selected>")) {
      cmd.base.level <- paste("list(", names(BWS2attributes)[1], " = '",
                              BaseLevelList[1], "'", sep = "")
      for (i in 2:nAlts) {
        cmd.base.level <- paste(cmd.base.level, ", ", names(BWS2attributes)[i], 
                                " = '", BaseLevelList[i], "'", sep = "")
      }
      cmd.base.level <- paste(cmd.base.level, ")", sep = "")
    } else {
      cmd.base.level <- paste("NULL")
    }



    putDialog("bws2Dataset", list(
    ini.baseAttribute      = BaseAttributeVar,
    ini.reverseAttributes  = tclvalue(reverseAttributesVariable),
    ini.responsetype       = tclvalue(responsetypeVariable),
    ini.modeltype          = tclvalue(modeltypeVariable),
    ini.rowsValue          = tclvalue(rowsValue),
    ini.datasetName        = tclvalue(datasetName),
    ini.designName         = tclvalue(designName),
    ini.idName             = tclvalue(idName),
    ini.letterRB           = tclvalue(lettertypeVariable),
    attributelevelName     = tclvalue(attributelevelName),
    saveVariable           = tclvalue(saveVariable)))

    if(BaseAttributeVar != "<no variable selected>") {
      BaseAttributeVar <- paste("'", BaseAttributeVar, "'", sep = "")
    } else {
      BaseAttributeVar <- paste("NULL")
    }

    closeDialog()

    nrows           <- as.numeric(tclvalue(rowsValue))
    ncols           <- 2
    k               <- 0
    BWvarNames      <- rep("", nrows * ncols)
    BWvarNamesTable <- matrix("", nrow = nrows, ncol = ncols)

    for (i in 1:nrows) {
      for (j in 1:2) {
        k <- k + 1
        BWvarname <- paste(".tab.", i, ".", j, sep = "")
        BWvarNames[k] <- 
          eval(parse(text =
            paste("as.character(tclvalue(", BWvarname, "))", sep = "")))
        BWvarNamesTable[i, j] <- BWvarNames[k]
      }
    }

    putRcmdr("savedTable", BWvarNamesTable)

    cmd <- paste("c('", paste(BWvarNames, collapse = "','"), "')", sep = "")

    # Randomize order of runs
    if (tclvalue(reverseAttributesVariable) == 1) {
      cmd.reverseAttributes <- paste(", reverse = TRUE")
    } else {
      cmd.reverseAttributes <- paste(", reverse = FALSE")
    }

    # Create data set for BWS2
    doItAndPrint(
      paste(tclvalue(datasetName), " <- bws2.dataset(data = ", 
            getRcmdr(".activeDataSet"),
            ", id = '", tclvalue(idName), "'",
            ", response = ", cmd,
            ", choice.sets = ", tclvalue(designName),
            ", attribute.levels = ", tclvalue(attributelevelName),
            ", base.attribute = ", BaseAttributeVar,
            ", base.level = ", cmd.base.level,
            cmd.reverseAttributes, 
            ", model = '", tclvalue(modeltypeVariable), "')", sep = ""))

    activeDataSet(tclvalue(datasetName))

    # Save to file
    if (tclvalue(saveVariable) == 1) {
      saveFile <- tclvalue(tkgetSaveFile(
        filetypes = gettextRcmdr(
          '{"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}'),
        defaultextension = ".rda",
        initialfile = paste0(tclvalue(datasetName), ".rda"),
        parent = CommanderWindow()))
      if (saveFile == "") {
        tkfocus(CommanderWindow())
        return()
      }
      cmd <- paste0('save(', tclvalue(datasetName),
                    ', file = "', saveFile, '")')
      justDoIt(cmd)
      logger(cmd)
      Message(paste(gettextRcmdr(
            "BWS2 data set for analysis was exported to file: "),
          saveFile),
        type = "note")
    }


    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws2Dataset",
               reset       = "resetBws2Dataset",
               apply       = "bws2Dataset")

  # Output
  tkgrid(labelRcmdr(datasetnameFrame,
    text = gettextRcmdr("Name for data set ")),
    dataset, sticky = "w")
  tkgrid(saveCheckBox,
         labelRcmdr(saveFrame, text = gettextRcmdr("Save to file")),
         sticky = "w")
  tkgrid(datasetnameFrame, labelRcmdr(outputFrame, text = "   "),
         saveFrame, sticky = "w")
  tkgrid(outputFrame, sticky = "w")

  # Blank
  tkgrid(labelRcmdr(top, text = ""))

  # Inputs
  ## Left side
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("Design")),
    design, sticky = "w")
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("Attributes and levels ")),
    labelRcmdr(inputsFrame, text = tclvalue(attributelevelName),
               relief = "solid", foreground = "green"),
    sticky = "w")
  tkgrid(labelRcmdr(objectsFrame,
    text = gettextRcmdr("ID variable")),
    id, sticky = "w")
  tkgrid(objectsFrame, sticky = "w")

  tkgrid(modeltypeFrame, sticky = "w")
  tkgrid(radio3Frame, sticky = "w")

  tkgrid(reverseAttributesCheckBox,
      labelRcmdr(
        chkbuttonFrame,
        text = gettextRcmdr("Reverse attribute variables")),
    sticky = "w")
  tkgrid(chkbuttonFrame, sticky = "w")

  tkgrid(labelRcmdr(
           baseAttributeFrame,
           text = gettextRcmdr("Select effect-coded base attribute and/or levels:")),
         sticky = "w")
  tkgrid(baseAttributeFrame, sticky = "w")

  tkgrid(labelRcmdr(baseLevelFrame1,
                    text = gettextRcmdr("Base attribute ")),
  getFrame(baseAttributeBox), sticky = "w")
  tkgrid(baseAttributeFrame, sticky = "w")

  for (i in 1:nAlts){
    eval(parse(text = paste(
      "tkgrid(labelRcmdr(baseLevelFrame1", 
      ", text = gettextRcmdr('Base level for ", names(BWS2attributes)[i], 
      "')), getFrame(baseLevelBox", i, 
      "), sticky = 'w')",
      sep = "")))
    eval(parse(text = paste(
      "tkgrid(baseLevelFrame1",
      ", sticky = 'w')",
      sep = "")))
  }

  ## Right side
  tkgrid(labelRcmdr(rowsFrame,
    text = gettextRcmdr("Number of BWS2 questions ")),
    rowsSlider, rowsShow, sticky = "w")
  tkgrid(rowsFrame, sticky = "w")
  tkgrid(lettertypeFrame, sticky = "w")
  tkgrid(letterFrame, sticky = "w")
  tkgrid(labelRcmdr(
           tableFrame,
           text = gettextRcmdr("Names of best- and worst-response variables:")),
         sticky = "w")
  tkgrid(tableFrame, sticky="w")


  tkgrid(leftFrame, labelRcmdr(inputsFrame, text = "    "),
         rightFrame, sticky = "nw")
  tkgrid(inputsFrame, sticky = "w")

  setUpTable()

  # Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

resetBws2Dataset <- function(){
  putRcmdr("savedTable", NULL)
  putDialog("bws2Dataset", NULL)
  bws2Dataset()
}
###############################################################################
bws2Count <- function() {
  initializeDialog(title = gettextRcmdr("Calculate BWS2 Scores"))
  defaults <- list(
    ini.dataName = "BWS2scores")
  dialog.values <- getDialog("bws2Count", defaults)

  optionsFrame <- tkframe(top)
  datasetFrame <- tkframe(optionsFrame)
  activeFrame  <- tkframe(optionsFrame)

  # data
  dataName <- tclVar(dialog.values$ini.dataName)
  data     <- ttkentry(datasetFrame, width = "14", textvariable = dataName)


  onOK <- function() {
    putDialog("bws2Count", list(
    ini.dataName = tclvalue(dataName)))

    dataValue <- tclvalue(dataName)
    closeDialog()

    doItAndPrint(paste(dataValue," <- bws2.count(data = ", ActiveDataSet(), ")", 
                       sep = ""))

    activeDataSet(tclvalue(dataName))

    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws2Count",
               reset       = "bws2Count",
               apply       = NULL)

  tkgrid(labelRcmdr(
    datasetFrame,
    text = gettextRcmdr("Name for scores ")),
    data, sticky = "w")
  tkgrid(datasetFrame, sticky = "w")
  tkgrid(optionsFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}
###############################################################################
bws2CountSum <- function() {
  doItAndPrint(paste("sum(", ActiveDataSet(), ")", sep = ""))
}
###############################################################################
bws2CountBarplot <- function() {
  initializeDialog(
    title = gettextRcmdr("Draw Distributions of BWS2 Scores"))
  defaults <- list(
    ini.scoretype = "bw",
    ini.NrowsName = "",
    ini.NcolsName = "")
  dialog.values <- getDialog("bws2CountBarplot", defaults)

  optionsFrame <- tkframe(top)
  scoreFrame   <- tkframe(optionsFrame)
  mfrowFrame   <- tkframe(optionsFrame)
  rowcolFrame  <- tkframe(mfrowFrame)

  # type of scores
  radioButtons(scoreFrame, 
    name    = "scoretype",
    buttons = c("BW", "B", "W"),
    values  = c("bw", "b", "w"),
    labels  = gettextRcmdr(c("Best-minus-Worst", "Best", "Worst")),
    initialValue = dialog.values$ini.scoretype,
    title   = gettextRcmdr("Score type"))

  # Nrows
  NrowsName <- tclVar(dialog.values$ini.NrowsName)
  Nrows     <- ttkentry(rowcolFrame, width = "4", textvariable = NrowsName)

  # Ncols
  NcolsName <- tclVar(dialog.values$ini.NcolsName)
  Ncols     <- ttkentry(rowcolFrame, width = "4", textvariable = NcolsName)

  onOK <- function() {
    putDialog("bws2CountBarplot", list(
    ini.scoretype = tclvalue(scoretypeVariable),
    ini.NrowsName = tclvalue(NrowsName),
    ini.NcolsName = tclvalue(NcolsName)))

    closeDialog()

    if (tclvalue(NrowsName) == "" & tclvalue(NcolsName) == "" ) {
      cmd.mfrow <- paste(", mfrow = NULL", sep = "")
    } else {
      cmd.mfrow <- paste(", mfrow = c(", tclvalue(NrowsName),
                         ", ", tclvalue(NcolsName), ")", sep = "")
    }

    doItAndPrint(paste("barplot(height = ", ActiveDataSet(),
                       ", score = '", tclvalue(scoretypeVariable), "'", 
                       cmd.mfrow, ")", sep = ""))

    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "bws2CountBarplot",
               reset       = "bws2CountBarplot",
               apply       = "bws2CountBarplot")

  tkgrid(labelRcmdr(
    mfrowFrame,
    text = gettextRcmdr("Arrangement of bar plots (optional)")), 
    sticky = "w")
  tkgrid(Nrows, labelRcmdr(rowcolFrame,
                           text = gettextRcmdr("row(s) and ")), 
         Ncols, labelRcmdr(rowcolFrame,
                           text = gettextRcmdr("column(s)")),
         sticky = "w")
  tkgrid(rowcolFrame, sticky = "w")
  tkgrid(scoretypeFrame, sticky = "w")
  tkgrid(scoreFrame, labelRcmdr(optionsFrame, text = "    "),
         mfrowFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}
###############################################################################
bws2Model <- function() {
  initializeDialog(title = 
    gettextRcmdr("Fit Model to BWS2 Data"))
  defaults <- list(
    ini.responseVarName = "RES",
    ini.strataVarName   = "STR",
    ini.attributesVar   = NULL,
    ini.levelsVar       = NULL,
    ini.covariatesVar   = NULL)
  dialog.values <- getDialog("bws2Model", defaults)

  .activeModel <- ActiveModel()
  currentModel <- if(!is.null(.activeModel)) {
    class(get(.activeModel, envir = .GlobalEnv))[1] == "clogit"
  } else {
    FALSE
  }
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir = .GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }

  # remove a term 'strata' from the current model formula
  if (currentModel) {
    currentRhs <- currentFields$rhs
    currentRhs <- gsub(' +', '', currentRhs)
    currentRhs <- unlist(strsplit(currentRhs, "\\+"))
    strataPos  <- grep("strata\\(", currentRhs)
    currentRhs <- currentRhs[-strataPos]
    currentRhs <- paste(currentRhs, collapse = " + ")

    currentFields$rhs <- currentRhs
  }

  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }

  ##### Output Frame
  UpdateModelNumber()
  outputFrame <- tkframe(top)
  modelName   <- tclVar(paste("BWS2model.", getRcmdr("modelNumber"), sep = ""))
  model       <- ttkentry(outputFrame, width = "14", textvariable = modelName)

  ##### Input Frame
  inputFrame <- tkframe(top)
  
  ## Frames in left
  leftFrame        <- tkframe(inputFrame)
  responseVarFrame <- tkframe(inputFrame)
  strataVarFrame   <- tkframe(inputFrame)
  attributesFrame  <- tkframe(leftFrame)

  # set response variable (responseVarFrame)
  responseVarName  <- tclVar(dialog.values$ini.responseVarName)
  responseVar      <- ttkentry(responseVarFrame, width = "5",
                               textvariable = responseVarName)

  # set strata variable (strataVarFrame)
  strataVarName  <- tclVar(dialog.values$ini.strataVarName)
  strataVar      <- ttkentry(strataVarFrame, width = "5",
                             textvariable = strataVarName)

  # select attribute variables
  availableAttributes <- names(BWS2attributes)
  attributesBox <- variableListBox(
                     attributesFrame, availableAttributes,
                     title = gettextRcmdr("Attribute variables (pick zero or more)"),
                     selectmode = "multiple", listHeight = 4,
                     initialSelection = varPosn(dialog.values$ini.attributesVar,
                                                vars = availableAttributes))


  ## Frames in center
  centerFrame <- tkframe(inputFrame)
  levelsFrame <- tkframe(centerFrame)

  # select attribute variables
  availableLevels <- 
    attributes(eval(parse(text = ActiveDataSet())))$lev.var.wo.ref
  levelsBox <- variableListBox(
                 levelsFrame, availableLevels,
                 title = gettextRcmdr("Level variables (pick)"),
                 selectmode = "multiple", listHeight = 4,
                 initialSelection = varPosn(dialog.values$ini.levelsVar,
                                            vars = availableLevels))

  
  ## Frames in right
  rightFrame      <- tkframe(inputFrame)
  covariatesFrame <- tkframe(rightFrame)

  # select covariates
  availableCovariates <- 
    sort(attributes(eval(parse(text = ActiveDataSet())))$respondent.characteristics)
  covariatesBox <- variableListBox(
                     covariatesFrame, availableCovariates,
                     title = gettextRcmdr("Covariates (pick zero or more)"),
                     selectmode = "multiple", listHeight = 4,
                     initialSelection = varPosn(dialog.values$ini.covariatesVar,
                                                vars = availableCovariates))


  onOK <- function () {
    modelValue  <- trim.blanks(tclvalue(modelName))
    responseVar <- trim.blanks(tclvalue(responseVarName))
    strataVar   <- trim.blanks(tclvalue(strataVarName))
    attributes  <- getSelection(attributesBox)
    levels      <- getSelection(levelsBox)
    covariates  <- getSelection(covariatesBox)
    closeDialog()

    attributesLevelsMF <- paste(c(attributes, levels), collapse = " + ")
   
    putDialog("bws2Model", 
      list(ini.responseVarName = tclvalue(responseVarName),
           ini.strataVarName   = tclvalue(strataVarName),
           ini.attributesVar   = attributes,
           ini.levelsVar       = levels,
           ini.covariatesVar   = covariates))

    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    } else {
      subset <- paste(", subset = ", subset, sep = "")
      putRcmdr("modelWithSubset", TRUE)
    }

    if (length(covariates) == 0) {
      formula <- paste(responseVar, " ~ ", attributesLevelsMF,  
                       " + strata(", strataVar ,")", sep = "")
    } else {
      covariates <- paste(covariates, collapse = " + ")
      formula <- paste(responseVar, " ~ (", attributesLevelsMF, ") * (", covariates, 
                       ") - (", covariates, ") + strata(", strataVar ,")", 
                       sep = "")
    }

    cmd <- paste("clogit(", formula, ", data = ", ActiveDataSet(), subset, 
                 ")", sep = "")

    doItAndPrint(paste(modelValue, " <- ", cmd, sep = ""))
    doItAndPrint(paste0(modelValue))
    doItAndPrint(paste0("gofm(", modelValue,")"))

    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "bws2Model", model = TRUE,
               reset       = "resetBws2Model",
               apply       = "bws2Model")

  ## Output
  tkgrid(labelRcmdr(outputFrame, text = gettextRcmdr("Name for model ")),
         model, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  ## Inputs
  # Frames in left  
  tkgrid(labelRcmdr(responseVarFrame, 
                    text = gettextRcmdr("Response variable ")),
         labelRcmdr(responseVarFrame,
                    text = tclvalue(responseVarName),
                    relief = "solid", foreground = "green"),
         sticky = "w")
  tkgrid(responseVarFrame, sticky = "w")

  tkgrid(getFrame(attributesBox), sticky = "nw")
  tkgrid(attributesFrame, sticky = "w")

  # Frames in center
  tkgrid(getFrame(levelsBox), sticky = "nw")
  tkgrid(levelsFrame, sticky = "w")

  # Frames in right
  tkgrid(getFrame(covariatesBox), sticky = "nw")
  tkgrid(covariatesFrame, sticky = "w")

  # Inputs Frame
  tkgrid(leftFrame,   labelRcmdr(inputFrame, text = "   "), 
         centerFrame, labelRcmdr(inputFrame, text = "   "), 
         rightFrame,  sticky = "nw")

  tkgrid(labelRcmdr(strataVarFrame, 
                    text = gettextRcmdr("Stratification variable ")),
         labelRcmdr(strataVarFrame,
                    text = tclvalue(strataVarName),
                    relief = "solid", foreground = "green"),
         sticky = "w")
  tkgrid(strataVarFrame, sticky = "w")

  tkgrid(inputFrame, sticky = "w")
  

  # subset
  subsetBox(inputFrame, model = TRUE)
  tkgrid(labelRcmdr(inputFrame, text = ""))
  tkgrid(subsetFrame, sticky = "w")

  # Buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix()
}

resetBws2Model <- function() {
  putRcmdr("reset.model", TRUE)
  putDialog("bws2Model", NULL)
  putDialog("bws2Model", NULL, resettable = FALSE)
  bws2Model()
}
###############################################################################
bws2Load <- function() {
  file <- tclvalue(tkgetOpenFile(filetype = gettextRcmdr(
    ' {"R Data Files" {".rda" ".RDA" ".rdata" ".RData"}}')))

  if (file == "") {
    return()
  }

  setBusyCursor()
  on.exit(setIdleCursor())

  cmd <- paste0('load("', file, '")')
  loadedObjects <- justDoIt(cmd)
  logger(cmd)
  Message(paste0(gettextRcmdr("Names of loaded objects: "),
                 paste(loadedObjects, collapse = ", ")),
          type = "note")

  tkfocus(CommanderWindow())
}
###############################################################################
bws2DataP <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "bws2dataset"
}
bws2CountP <- function() {
  activeDataSetP() && class(get(ActiveDataSet()))[1] == "bws2.count"
}

