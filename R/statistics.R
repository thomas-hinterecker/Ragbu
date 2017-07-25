#' Calculates the standard error of the mean for a given vector
#'
#' @title SE
#' @param x Data (vector)
#' @return The standard error
#' @export
se <-  function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

#' Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#'
#' @title Summarizes data
#' @param data          A data frame
#' @param measurevar    The name of a column that contains the variable to be summariezed
#' @param groupvars     Vector containing names of columns that contain grouping variables
#' @param na.rm         Boolean that indicates whether to ignore NA's
#' @param conf.interval The percent range of the confidence interval (default is 95%)
#' @return a data frame with count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#' @importFrom data.table data.table setkey
#' @export
summarySE <- function (data = NULL, measurevar, groupvars = NULL, na.rm = TRUE, conf.interval = 0.95) {
  data <- data.table(data)
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm)
      sum(!is.na(x))
    else length(x)
  }
  datac <- data[, .(unlist(lapply(.SD, length2, na.rm = na.rm)),
                    unlist(lapply(.SD, mean, na.rm = na.rm)),
                    unlist(lapply(.SD, sd, na.rm = na.rm))),
                by = groupvars, .SDcols = measurevar]
  names(datac) <- c(groupvars, "N", measurevar, "sd")
  setkeyv(datac, groupvars)
  datac[, se := unlist(sd) / sqrt(unlist(N))] # compute standard error
  ciMult <- qt(conf.interval / 2 + 0.5, unlist(datac$N) - 1)
  datac[, ci := se * ciMult]
  datac <- data.frame(datac)
  return(datac)
}

#' Norms the data within specified groups in a data frame; it normalizes each subject (identified by idvar) so that they have the same mean, within each group specified by betweenvars.
#'
#' @title Normalize within-group data
#' @param data          A data frame
#' @param idvar         The name of a column that identifies each subject (or matched subjects)#'
#' @param measurevar    The name of a column that contains the variable to be summariezed
#' @param betweenvars   A vector containing names of columns that are between-subjects variables
#' @param na.rm         Boolean that indicates whether to ignore NA's
#' @param conf.interval The percent range of the confidence interval (default is 95%)
#' @return a data frame with normalized data
#' @importFrom data.table data.table setkey
#' @export
normDataWithin <- function (data = NULL, idvar, measurevar, betweenvars = NULL, na.rm = TRUE) {
  data <- data.table(data)
  setkeyv(data, idvar)
  data.subjMean <- data[, .(unlist(lapply(.SD, mean, na.rm = na.rm))), by = idvar, .SDcols = measurevar]
  names(data.subjMean) <- c(idvar, 'subjMean')
  data <- merge(data, data.subjMean)
  setkeyv(data, c(idvar, betweenvars))
  measureNormedVar <- paste(measurevar, "Normed", sep = "")
  data <- data.frame(data)
  data[, measureNormedVar] <- data[, measurevar] - unlist(data[, "subjMean"]) + mean(data[, measurevar], na.rm = na.rm)
  return(data)
}


#' Summarizes data, handling within-subjects variables by removing inter-subject variability
#'
#' @title Summarize within-subjects data
#' @param data          A data frame
#' @param measurevar    The name of a column that contains the variable to be summariezed
#' @param betweenvars   A vector containing names of columns that are between-subjects variables
#' @param withinvars    Vector containing names of columns that are within-subjects variables
#' @param idvar         The name of a column that identifies each subject (or matched subjects)
#' @param na.rm         Boolean that indicates whether to ignore NA's
#' @param conf.interval The percent range of the confidence interval (default is 95%)
#' @return a data frame with count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#' @export
summarySEwithin <- function (data = NULL, measurevar, betweenvars = NULL, withinvars = NULL, idvar = NULL, na.rm = TRUE, conf.interval = 0.95) {
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- sapply(data[, c(betweenvars, withinvars), drop = FALSE], FUN = is.factor)
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval)
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm)
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "Normed", sep="")
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval)
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN= function(x) length(levels(x)),
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  # Apply the correction factor
  ndatac$sd <- unlist(ndatac$sd) * correctionFactor
  ndatac$se <- unlist(ndatac$se) * correctionFactor
  ndatac$ci <- unlist(ndatac$ci) * correctionFactor

  # Combine the un-normed means with the normed results
  merged <- merge(datac, ndatac)
  #merged[, 1] <- as.numeric(as.character(merged[, 1]))
  #merged <- merged[order(merged[, 1]), ]
  return(merged)
}

#' This function provides easy analysis of data from factorial experiments,
#' including purely within-Ss designs (a.k.a. "repeated measures"),
#' purely between-Ss designs, and mixed within-and-between-Ss designs,
#' yielding either Linear mixed effect model, ANOVA or t-test results,
#' as well as effect sizes and assumption checks.
#'
#' @title Compute ANOVA, LMM, or t-test
#' @param data                 Data frame containing the data to be analyzed.
#' @param dv                   Name of the column in data that contains the dependent variable. Values in this column must be numeric.
#' @param wid                  Name of the column in data that contains the variable specifying the case/Ss identifier. This should be a unique value per case/Ss.
#' @param within               Names of columns in data that contain predictor variables that are manipulated (or observed) within-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param between              Names of columns in data that contain predictor variables that are manipulated (or observed) between-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param test                 Use this parameter to specify the test that should be run. (aov = ANOVA, lmer = linear mixed effects model)
#' @param options              Use this parameter to specify the test options. For lmer these are random_effects, contrasts and ddf. For aov sph.cor and mau.p
#' @param return_obj           Set this parameter to TRUE if you want that the analysis model is returned as function value.
#' @param print                Set this parameter to FALSE if the function should not print the results.
#' @param dfsep                This specifies the seperator used to seperator the df values in the formatted results. By default, a comma is used.
#' @param random_effects       LMER parameter: Use this parameter to specify a more complex random effects term of a linear mixed effects model. The given value must be of type string.
#' @param contrasts            LMER parameter: Use this paramater to specify a list of contrasts for the fixed effects factors in a linear mixed effects model.
#' @param ddf                  LMER parameter: This specifies the method used for calculating p-values of the fixed effects of the linear mixed model (Satterthwaite, Kenward-Roger, etc.).
#' @param sph.cor              ANOVA parameter: Use this paramater to specify the correction estimates to use for sphericity corrections of within factors of a ANOVA (GG, HF, no; default="GG").
#' @param mau.p                ANOVA parameter: Use this paramater to specify the threshold for Mauchly's test of sphericity (default=0.05).
#' @return Model object (optional)
#' @importFrom DescTools EtaSq
#' @importFrom lmerTest lmer anova
#' @importFrom plyr .
#' @importFrom stats aov
#' @importFrom ez ezANOVA
#' @importFrom afex aov_4
#' @importFrom effsize cohen.d
#' @export
ezAnalysis <- function(data, dv, wid= NULL, within = NULL, between = NULL, test = "aov", options = list(random_effects = NULL, contrasts = NULL, ddf = "Satterthwaite", sph.cor = "GG", mau.p = 0.05, peta = TRUE), return_obj = FALSE, print = TRUE, dfsep = ", ") {
  ################################ check args
  args_to_check = c('dv', 'wid', 'within', 'between')
  args = as.list(match.call()[-1])
  for(i in 1:length(args)){
    arg_name = names(args)[i]
    if(arg_name%in%args_to_check){
      if(is.symbol(args[[i]])){
        code = paste(arg_name,'=.(', as.character(args[[i]]),')', sep='')
        eval(parse(text=code))
      }else{
        if(is.language(args[[i]])){
          arg_vals = as.character(args[[i]])
          arg_vals = arg_vals[2:length(arg_vals)]
          arg_vals = paste(arg_vals,collapse=',')
          code = paste(arg_name, '=.(',arg_vals,')', sep='')
          eval(parse(text=code))
        }
      }
    }
  }
  if (!is.data.frame(data)) {
    stop('"data" must be a data frame.')
  } else { data <- data.frame(data) }
  vars = as.character(c(dv, wid, between, within))
  for (var in vars) {
    if (!(var %in% names(data))) {
      stop(paste("\"", var, "\" is not a variable in the data frame provided.", sep = ""))
    }
  }
  if (is.null(within) & is.null(between)) {
    stop('is.null(within) & is.null(between)\nYou must specify at least one independent variable.')
  }
  if (!is.numeric(data[,names(data)==dv])) {
    stop('"dv" must be numeric.')
  }
  if (!is.factor(data[,names(data)==wid])) {
    warning(paste('Converting "', wid, '" to factor.', sep=''), immediate.=TRUE, call.=FALSE)
    data[,names(data)==wid] <- factor(data[,names(data)==wid])
  }else{
    if (length(unique(data[,names(data)==wid]))!=length(levels(data[,names(data)==wid]))){
      warning(paste('You have removed one or more Ss from the analysis. Refactoring "', wid, '"', sep=''), immediate.=TRUE, call.=FALSE)
      data[,names(data)==wid] <- factor(data[,names(data)==wid])
    }
  }
  ################################ DO Analysis
  # Create formula
  slopes <- within
  intercepts <- wid
  if (is.null(options$random_effect) == FALSE) { random_effects <- options$random_effect } else { random_effects <- NULL }
  if (is.null(slopes)) { slopes <- "1"  }
  if (is.null(random_effects)||random_effects=="") {
    random_effects <- paste('(', paste(as.character(slopes), collapse = '*'), '|', paste(as.character(intercepts), collapse = ':'), ')', sep = '')
  }
  model.formula <- paste(
    as.character(dv), '~', paste(as.character(between), collapse = '*'),
    ifelse(is.null(between), '', ifelse(is.null(within), '', '*')),
    paste(as.character(within), collapse = '*'),
    paste('+', random_effects, sep = ''), sep = ''
  )
  # If test is aov then check if only one factor is provided and with only 2 factor levels -> if so, run t-test
  if (test != "lmer" && length(c(between, within))==1) {
    nlevels <- eval(parse(text=paste("length(unique(data$", paste(c(between, within), sep=""), "))", sep="")))
    if (nlevels == 2) {
      test <- "t.test"
    }
  }
  # DO TEST
  if (test == "lmer") {
    contrasts <- options$contrasts
    if (is.null(options$ddf)) { ddf <- "Satterthwaite" } else { ddf <- options$ddf }
    ## LMER
    model <- lmer(formula(model.formula), data=data, contrasts=contrasts)
    model.ANOVA <- anova(model, ddf=ddf)
    ## Create output table
    outtable <- data.frame(
      Effect=rownames(model.ANOVA),
      'Sum Sq'=format(round(model.ANOVA$`Sum Sq`,2), nsmall=2),
      'Mean Sq'=format(round(model.ANOVA$`Mean Sq`,2), nsmall=2),
      df1=model.ANOVA$NumDF,
      df2=format(round(model.ANOVA$DenDF,0), nsmall=0),
      F=format(round(model.ANOVA$F.value,2), nsmall=2),
      p=format(round(model.ANOVA$`Pr(>F)`,3), nsmall=3)
    )
    ## Aggregate and AOV
    agg_formula <- paste(
      as.character(dv), "~", as.character(wid), "+",
      paste(as.character(between), collapse = '+'),
      ifelse(is.null(between), '', ifelse(is.null(within), '', '+')),
      paste(as.character(within), collapse = '+'), sep=""
    )
    data_agg <- aggregate(formula(agg_formula), data=data, FUN=mean)
    # AOV for etasq
    aov_formula <- paste(
      as.character(dv), '~',
      paste(as.character(c(between, within)), collapse='*'),
      ifelse(is.null(within)==FALSE, paste('+Error(', as.character(wid), '/(', paste(as.character(within), collapse = '*'), '))', sep=""), ''),
      sep=""
    )
    # list_etasq <- EtaSq(aov(formula(aov_formula), data=data_agg), type=ifelse(is.null(within), 2, 1))
    # Add etasq ...
#     for (i in 1:nrow(outtable)) {
#       outtable$etasq[i] <- format(round(list_etasq[as.character(outtable$Effect[i]), 1],2), nsmall=2)
#       outtable$petasq[i] <- format(round(list_etasq[as.character(outtable$Effect[i]), 2],2), nsmall=2)
#       if (ncol(list_etasq) > 2) { outtable$pgetasq[i] <- format(round(list_etasq[as.character(outtable$Effect[i]), 3],2), nsmall=2) }
#     }
    rm(agg_formula, aov_formula) #, list_etasq
    #
    txttable <- outtable
    outspher_lev <- "N/A"
    test.desc <- "Linear Mixed Effects Model"
  } else if (test=="t.test") { # T.TEST
    # Refactor so that only two factor levels are remaining
    eval(parse(text=paste("data$", paste(c(between, within), sep=""), " <- factor(data$", paste(c(between, within), sep=""), ", unique(data$", paste(c(between, within), sep=""), "))", sep="")))
    levels <- eval(parse(text=paste("unique(data$", paste(c(between, within), sep=""), ")", sep="")))
    if (is.null(within)==TRUE) {
      # Levene test
      levene <- eval(parse(text=paste("leveneTest(",
                                      as.character(dv), "~", paste(c(between, within), sep=""), ",",
                                      "data=data.frame(data)", ")", sep="")))
      if (levene$"Pr(>F)"[1]<0.05) { test <- "welch t.test" }
      outspher_lev <- data.frame(
        "Levene"=paste(c(between, within), sep=""),
        Df1=levene$Df[1],
        Df2=levene$Df[2],
        F=format(round(levene$"F value"[1], 2), nsmall=3),
        p=format(round(levene$"Pr(>F)"[1], 3), nsmall=3)
      )
    } else {
      test <- "paired t.test"
      outspher_lev <- "N/A"
    }
    model <- eval(parse(text=paste("t.test(", as.character(dv), "~", paste(c(between, within), sep=""), ", ",
                                   "data=data.frame(data), ",
                                   ifelse(is.null(between), ", paired=TRUE", ""), ")",sep="")))
    # Effect size
    cohensd <- eval(parse(text=paste("cohen.d(",
                                     "data$", as.character(dv), "[data$", paste(c(between, within), sep=""), "=='", levels[1], "']", ",",
                                     "data$", as.character(dv), "[data$", paste(c(between, within), sep=""), "=='", levels[2], "']",
                                     ifelse(is.null(between), ", paired=TRUE", ""), ")", sep="")))
    if (test=="paired t.test") { cohensd$estimate <- cohensd$estimate*sqrt(2) }
    # Create output table
    outtable <- data.frame(
      Effect=paste(c(between, within), sep=""),
      df=format(round(model$parameter, 0), nsmall=0),
      t=format(round(model$statistic, 2), nsmall=2),
      p=format(round(model$p.value, 3), nsmall=3),
      cohens.d=format(round(cohensd$estimate, 2), nsmall=2)
    )
    txttable <- outtable
    if (test=="welch t.test") { # add note for welch t.tests
      note <- paste("Levene's test for homogeneity of variance shows a significant result.")
    }
    test.desc <- model$method
  } else { # ANOVA
    if (is.null(options$sph.cor)) { sph.cor <- "GG" } else { sph.cor <- options$sph.cor }
    if (is.null(options$mau.p)) { mau.p <- 0.05 } else { mau.p <- options$mau.p }
    # Check for inconsistent sphericity mehod
    if (toupper(sph.cor)!="GG" & toupper(sph.cor)!="HF" & toupper(sph.cor)!="NO" ) {
      sph.cor <- "no"
      print(paste("Warning: Unknown correction method specified!", " Reporting uncorrected p-values instead.", sep=""), quote=FALSE)
    }
    # Run
    model.test <- eval(parse(text=paste("ezANOVA(",
                                        "data,", as.character(dv), ",", as.character(wid), ",",
                                        ifelse(is.null(within), "NULL,", paste("within=.(", paste(as.character(within), collapse = ','), "),", sep="")),
                                        ifelse(is.null(between), "NULL,",paste("between=.(", paste(as.character(between), collapse = ','), "),", sep="")),
                                        "detailed=TRUE, return_aov=TRUE", ")", sep="")))
    model <- aov_4(formula(model.formula), data=data, anova_table=list(correction="none", es="pes"))
    if (is.null(within)==TRUE) { model.ANOVA <- model.test$ANOVA[,] } else { model.ANOVA <- model.test$ANOVA[-1,] }
    # Effect sizes
    list_etasq <- EtaSq(model.test$aov,type=ifelse(is.null(within), 2, 1))
    # Create output table
    outtable <- data.frame(
      Effect=model.ANOVA$Effect,
      MSE=format(round(model.ANOVA$SSd/model.ANOVA$DFd, 2), nsmall=0),
      df1=format(round(model.ANOVA$DFn, 0), nsmall=0),
      df2=format(round(model.ANOVA$DFd, 0), nsmall=0),
      F=format(round(model.ANOVA$F, 2), nsmall=2),
      p=format(round(model.ANOVA$p, 3), nsmall=3),
      petasq=format(round(model.ANOVA$SSn/(model.ANOVA$SSn+model.ANOVA$SSd), 2), nsmall=2),
      getasq=format(round(model.ANOVA$ges, 2), nsmall=2)
    )
    # Sphericity tests
    if ("Mauchly's Test for Sphericity" %in% names(model.test)) {
      outspher_lev <- data.frame(
        Effect=model.test$"Mauchly's Test for Sphericity"$Effect,
        p_Mauchly=format(round(model.test$"Mauchly's Test for Sphericity"$p, 3), nsmall=3),
        GGEpsilon=format(round(model.test$"Sphericity Corrections"$GGe, 3), nsmall=3),
        p_GG=format(round(model.test$"Sphericity Corrections"$"p[GG]", 3), nsmall=3),
        HFEpsilon=format(round(model.test$"Sphericity Corrections"$HFe, 3), nsmall=3),
        p_HF=format(round(model.test$"Sphericity Corrections"$"p[HF]", 3), nsmall=3)
      )
    } else {
      outspher_lev <- "N/A"
      sph.cor <- "no"
    }
    txttable <- outtable
    ajdffcts <- list()
    # Check all effects listed in "Sphericity Corrections"
    if (toupper(sph.cor)!="NO") {
      for (isph in 1:length(model.test$"Sphericity Corrections"$Effect)) {
        # Get effects of interest and check corresponding p_Mauchly
        if (model.test$"Mauchly's Test for Sphericity"$p[isph] <= mau.p) {
          eoi <- model.test$"Sphericity Corrections"$Effect[isph]
          # Cycle through ANOVA table and check for effects
          for (iaov in 1:length(model.ANOVA$Effect)) {
            # Adjust p-value and degrees of freedom
            if (model.ANOVA[iaov, 1]==eoi) {
              if (toupper(sph.cor)=="GG") {
                pmaucorr <- format(round(model.test$"Sphericity Corrections"$"p[GG]"[isph], 3), nsmall=3);
                df1maucorr <- format(round(model.test$"Sphericity Corrections"$GGe[isph]*model.test$ANOVA[iaov+1, 2], 2), nsmall=2);
                df2maucorr <- format(round(model.test$"Sphericity Corrections"$GGe[isph]*model.test$ANOVA[iaov+1, 3], 2), nsmall=2);
                levels(txttable$p) <- c(levels(txttable$p), pmaucorr)
                levels(txttable$df1) <- c(levels(txttable$df1), df1maucorr)
                levels(txttable$df2) <- c(levels(txttable$df2), df2maucorr)
                txttable[iaov, 6] <- pmaucorr;
                txttable[iaov, 3] <- df1maucorr;
                txttable[iaov, 4] <- df2maucorr;
              } else if (toupper(sph.cor)=="HF") {
                pmaucorr <- format(round(model.test$"Sphericity Corrections"$"p[HF]"[isph], 3), nsmall=3);
                levels(txttable$p) <- c(levels(txttable$p), pmaucorr)
                txttable[iaov, 6] <- pmaucorr
              }
              ajdffcts <- c(ajdffcts,eoi);
            }
          }
        }
      }
      # Construct note
      if (length(ajdffcts)==0) {
        note <- paste("No adjustments necessary (all p_Mauchly > ", mau.p, ").",sep="")
      } else {
        note <- paste("p-values and degrees of freedom for the following effects were ", sph.cor, "-adjusted (p_Mauchly <= ", mau.p, "): ", paste(paste(ajdffcts,collapse="; ",sep=""),".",sep=""),sep="");
      }
    } else {
      if (toupper(sph.cor) == "NO") {
        note <- "Reporting unadjusted p-values."
      } else if (outspher_lev!="N/A") {
        note <- "Reporting unadjusted p-values."
      }
    }
    #
    if (is.null(within)) { test.desc <- "Between-Subjects ANOVA" } else if (is.null(between)) { test.desc <- "Within-Subjects ANOVA" } else { test.desc <- "Mixed-Effects ANOVA" }
  }
  ## Prepare formatted output
  pcorr <- paste(", p = ", txttable$p, sep="")
  pcorr <- gsub("p = 1.000","p > .999", pcorr, fixed=TRUE)
  pcorr <- gsub("p = 0.000","p < .001", pcorr, fixed=TRUE)
  pcorr <- gsub("p = 0","p = ", pcorr, fixed=TRUE)
  #
  if (test=="t.test"|test=="paired t.test"|test=="welch t.test") {
    cohendscorr <- paste(", d = ", txttable$cohens.d, sep="")
    cohendscorr <- gsub("d = 1.00", "d > .99", cohendscorr, fixed=TRUE)
    cohendscorr <- gsub("d = 0.00", "d < .01", cohendscorr, fixed=TRUE)
    cohendscorr <- gsub("d = 0","d = ", cohendscorr, fixed=TRUE)
    outtext <- data.frame(
      Effect=txttable$Effect,
      Text=paste("t(", txttable$df, ") = ", txttable$t, pcorr, cohendscorr, sep="")
    )
  } else {
    if (is.null(within)==FALSE && options$peta==TRUE) {
      petasqcorr <- paste(", np2 = ", txttable$petasq, sep="")
      petasqcorr <- gsub("np2 = 1.00", "np2 > .99", petasqcorr, fixed=TRUE)
      petasqcorr <- gsub("np2 = 0.00", "np2 < .01", petasqcorr, fixed=TRUE)
      petasqcorr <- gsub("np2 = 0","np2 = ", petasqcorr, fixed=TRUE)
      outtext <- data.frame(
        Effect=txttable$Effect,
        Text=paste("F(", txttable$df1, dfsep ,txttable$df2, ") = ", txttable$F, pcorr, petasqcorr, sep="")
      )
    } else {
      getasqcorr <- paste(", n2G = ", txttable$getasq, sep="")
      getasqcorr <- gsub("n2G = 1.00", "n2G > .99", getasqcorr, fixed=TRUE)
      getasqcorr <- gsub("n2G = 0.00", "n2G < .01", getasqcorr, fixed=TRUE)
      getasqcorr <- gsub("n2G = 0","n2G = ", getasqcorr, fixed=TRUE)
      outtext <- data.frame(
        Effect=txttable$Effect,
        Text=paste("F(", txttable$df1, dfsep, txttable$df2, ") = ", txttable$F, pcorr, getasqcorr, sep="")
      )
    }
  }
  x <- list(
    "--- TEST -------------------------------------------------" = test.desc,
    "--- FORMULA ----------------------------------------------" = model.formula,
    "--- RESULTS ----------------------------------------------" = outtable,
    "--- SPHERICITY TESTS / LEVENE TEST -----------------------" = outspher_lev,
    "--- FORMATTED RESULTS  -----------------------------------" = outtext
  )
  if (exists("note")) {
    x = c(x,"NOTE:"=note);
  }
  if (print==TRUE) {
    print(x)
  }
  if (return_obj==TRUE) {
    return(model)
  }
}

#' Distilles the most relevant data from an output object of lsmeans, calculates effect sizes, and displays the results in a compact format.
#'
#' @title Format LSMEANS Output
#' @param lsmobj Output object created by a call to lsmeans.
#' @param paired Set to true for paired data
#' @param n.equal Set to false if number of subjects per group is not equal
#' @export
#'
lsmeans_out <- function (lsmobj, paired=FALSE, n.equal=TRUE) {
  #
  lsmobj_summary <- summary(lsmobj)
  #
  if (paired==FALSE) {
    # Two samples, equal sample sizes
    if (identical(TRUE, n.equal)) {
      lsmobj_summary$cohens.d <- lsmobj_summary$t.ratio*sqrt(4/(lsmobj_summary$df+2))
      # Two samples, unequal sample sizes
    } else if (identical(FALSE, n.equal)) {
      stop("Please enter sample sizes, e.g., n.equal = c(12,8).", call.=TRUE)
    } else {
      if ((sum(n.equal)==lsmobj_summary$df+2) && (length(n.equal == 2))) {
        lsmobj_summary$cohens.d <- lsmobj_summary$t.ratio*sqrt(1/n.equal[1] + 1/n.equal[2])
      } else {
        stop("Sample sizes are inconsistent with degrees of freedom.", call. = TRUE)
      }
    }
  } else {
    lsmobj_summary$cohens.d <- lsmobj_summary$t.ratio/sqrt(lsmobj_summary$df+1)*sqrt(2)
  }
  #
  outtext <- data.frame(
    Num=integer(length=nrow(lsmobj_summary))
  )
  #
  names <- colnames(lsmobj_summary)
  for (i in 1:length(names)) {
    if (names[i] == "estimate") {
      groups_cols <- i-1
    }
  }
  #
  for (i in 1:nrow(lsmobj_summary)) {
    ## Prepare formatted output
    pcorr <- paste(", p = ", format(round(lsmobj_summary$p.value[i], 3), nsmall=3), sep="")
    pcorr <- gsub("p = 1.000","p > .999", pcorr, fixed=TRUE)
    pcorr <- gsub("p = 0.000","p < .001", pcorr, fixed=TRUE)
    pcorr <- gsub("p = 0","p = ", pcorr, fixed=TRUE)
    # effsize
    cohendscorr <- paste(", d = ", format(round(lsmobj_summary$cohens.d[i], 2), nsmall=2), sep="")
    cohendscorr <- gsub("d = 1.00", "d > .99", cohendscorr, fixed=TRUE)
    cohendscorr <- gsub("d = 0.00", "d < .01", cohendscorr, fixed=TRUE)
    cohendscorr <- gsub("d = 0","d = ", cohendscorr, fixed=TRUE)
    #
    groups <- c()
    if (groups_cols >= 2) {
      for (z in 2:groups_cols) {
        groups <- c(groups, as.character(lsmobj_summary[i,z]))
      }
      outtext$Effect[i] <- paste(paste(groups, collapse=" x "), lsmobj_summary$contrast[i], sep=", ")
    } else {
      outtext$Effect[i] <- paste(lsmobj_summary$contrast[i], sep=", ")
    }
    outtext$Text[i] <- paste(
      "t(", format(round(lsmobj_summary$df[i], 0), nsmall=0), ") = ",
      format(round(lsmobj_summary$t.ratio[i], 2), nsmall=2), pcorr, cohendscorr, sep="")
  }
  print(outtext[,2:3])
}