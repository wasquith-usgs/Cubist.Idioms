"cubist_asR_idioms" <-
function(cubist_object, cubist_tag="C0", nut_digits=3,
         path=".", prefix="", tidyrule.return=FALSE,
         sample_str="", var.names=NULL, write.utility.file=TRUE, ...) {

  if(class(cubist_object)[1] == "tbl_df") {
    tidy_cubist <- cubist_object
  } else if(! class(cubist_object) == "cubist") {
    warning("cubist variable is not a 'cubist' object")
    return()
  } else {
    var.names <- names(cubist_object$coefficients)
    var.names[1] <- NA # (Intercept)
    var.names[(length(var.names)-1):length(var.names)] <- NA # committee and rule
    var.names <- var.names[! is.na(var.names)]
    tidy_cubist <- tidyrules::tidyRules(cubist_object)
  }
  cubist_tag <- cubist_tag[1] # silent devectorization
  nut_digits <- nut_digits[1] # silent devectorization
  ixfrag <- paste0("MOD",cubist_tag,"_")

  path <- paste0(gsub("\\/$", "", path),"/")
  if(! dir.exists(path)) {
    message("creating '",path,"'")
    dir.create(path)
  }

  CH <- file(paste0(path,prefix,cubist_tag,"cubist_cubes.R"), open="w") # a wrapper for rule dispatch
  FH <- file(paste0(path,prefix,cubist_tag,"cubist_funcs.R"), open="w") # righthand side of Cubist
  NH <- file(paste0(path,prefix,cubist_tag,"cubist_nuts.R"),  open="w") # support, mean, min, max, error
  RH <- file(paste0(path,prefix,cubist_tag,"cubist_rules.R"), open="w") # lefthand side of Cubist
  if(write.utility.file) {
    UT <- file(paste0(path,prefix,"cubist_utils.R"),            open="w") # wrapper utilities
  }
  # MODel (result from a call to Cubist::cubist()). Could have multiple
  #    models formed from independent slices of the data set.
  # COMmmittee (a "boost" of branches for MODel [A|B|C|...])
  # BRAnch (a limb on the tree). A branch is the "same" as a RULe. There are
  #    nnn rules and mmm committees in a given Cubist. We don't use the idea of
  #    neighbors because we want to run Cubist completely outside its framework.
  #    Each branch is unique to a given committee. The distinction here is that
  #    the RULe is the triggering mechansim (tests as TRUE) along the BRAnch
  #    and if the RULe is applicable then the BRAnch (the regression) is triggered.

  # MOdel A --> COMmittee 001 ---> RULe001 --> BRAnch 001
  #                           ---> RULe002 --> BRAnch 002
  #                               ...
  #                           ---> RULennn --> BRAnch nnn
  #         --> COMmittee 002 ---> RULe001 --> BRAnch 001
  #                           ---> RULe002 --> BRAnch 002
  #                               ...
  #                           ---> RULennn --> BRAnch nnn
  #            ...
  #         --> COMmittee mmm ---> RULe001 --> BRAnch 001
  #                           ---> RULe002 --> BRAnch 002
  #                               ...
  #                           ---> RULennn --> BRAnch nnn
  # MOdel B --> COMmittee 001 ---> RULe001 --> BRAnch 001
  #                           ---> RULe002 --> BRAnch 002
  #                               ...
  #                           ---> RULennn --> BRAnch nnn
  #         --> COMmittee 002 ---> RULe001 --> BRAnch 001
  #                           ---> RULe002 --> BRAnch 002
  #                               ...
  #                           ---> RULennn --> BRAnch nnn
  #            ...
  #         --> COMmittee mmm ---> RULe001 --> BRAnch 001
  #                           ---> RULe002 --> BRAnch 002
  #                               ...
  #                           ---> RULennn --> BRAnch nnn
  #

  cat(paste0("SAMPLE_STR",cubist_tag,' = "',sample_str,'"',"\n\n"), file=CH)
  cat(paste0("CUBES_",cubist_tag," <- function(x) {\n"), file=CH)
  cat("  x <- x[1,]\n", file=CH)
  cat("  zz <- c(\n  ", file=CH)
  rule_k <- 0; previous_committee <- NULL
  for(i in 1:length(tidy_cubist$id)) {
    id  <- tidy_cubist$id[i]
    com <- tidy_cubist$committee[i]
    if(is.null(previous_committee)) previous_committee <- com
    if(previous_committee != com) rule_k <- 0
    rule_k <- rule_k + 1
    nmfa <- paste0("BRA", stringr::str_pad(rule_k,  width=3, pad="0"))
    nmfb <- paste0("COM", stringr::str_pad(com,     width=2, pad="0"))
    nmra <- paste0("RUL", stringr::str_pad(rule_k,  width=3, pad="0"))
    nmfb <- paste0(ixfrag,nmfb)

    txt <- paste0(nmfb,"_",nmfa)
    txtn <- paste0(txt,"_NUT"," <- function() {\n",
               "  zz <- c(",tidy_cubist$support[i],", ",
                            tidy_cubist$mean[i],   ", ",
                            tidy_cubist$min[i],    ", ",
                            tidy_cubist$max[i],    ", ",
                            tidy_cubist$error[i],  ")\n",
               "  zz <- round(zz, digits=",nut_digits,")\n",
               "  names(zz) <- c('support','mean','min','max','error')\n",
               "  return(zz)\n",
               "}\n\n")
    cat(txtn, file=NH)

    cat(paste0(nmfb,"_",nmra,"(x),"), file=CH)
    if(i %/% 3 == i / 3) cat("\n  ",  file=CH) # just a line break trigger

    xf <- paste0("; if(is.na(",nmfb,"_",nmra,"(x))) return(NA)") # the rule function
    f <- tidy_cubist$RHS[i] # the Cubist regression equation
    f <- gsub(" ([0-9A-Z\\_a-z]+\\))", " x$\\1", tidy_cubist$RHS[i])
    f <- noquote(paste0("'",nmfb,"_",nmfa,"'",
                        " <- function(x) {\n  x <- x[1,]",xf,
                        "\n  ",f,"\n}"))

    r <- unlist(strsplit(tidy_cubist$LHS[i], split="&"))
    r <- gsub("^\\s+", "", r) # strip leading spaces
    r <- gsub("\\s+$", "", r) # strip trailing spaces
    r <- paste("x$",r, sep="") # x is a data.frame
    r <- paste0("  zz <- ",paste(r, collapse=" & "),"\n") # Cubist rule as function assigned
    r <- paste0(r,"  return(ifelse(zz == TRUE, '",nmfb,"_",nmfa,"', NA))")
    # returning the name of the branch (i.e. MODA_COM001_BRA001) function
    r <- noquote(paste0("'",nmfb,"_",nmra,"'",
                 " <- function(x) {\n  x <- x[1,]\n",r,"\n}"))
    # prepending boiler plate and ensuring nonvectorized

    cat(f, file=FH); cat("\n\n", file=FH)
    cat(r, file=RH); cat("\n\n", file=RH)
    previous_committee <- com
  }
  cat("  NA)\n", file=CH)
  cat("  return(zz[! is.na(zz)])\n", file=CH)
  cat("}\n",     file=CH)
  close(CH)
  close(FH)
  close(NH)
  close(RH)

  if(write.utility.file) {
    if(is.null(var.names)) {
      warning(" var.names is NULL, please use \n",
              "'var.names=names(cubist_object$coefficients)'\n",
              " if you are passing tidyRules in lieu of the cubist_object")
    }
    n <- length(var.names)
    cat("getVARIABLE_NAMES <- function() {\n",   file=UT)
    if(is.null(var.names)) {
      cat("  c()\n",                             file=UT)
    } else {
      cat("  c(\n",                              file=UT)
      for(nm in var.names[1:(n-1)]) {
        cat(paste0("     '",nm,"',\n"),          file=UT)
      }
        cat(paste0("     '",var.names[n],"'\n"), file=UT)
      cat("   )\n",                              file=UT)
    }
    cat("}\n\n",                                 file=UT)

    cat("useBRANCH <- function(x, rules=NULL, cubes=NULL) {\n", file=UT)
    cat("  if(is.null(rules)) rules <- cubes(x)\n",             file=UT)
    cat("  return(sapply(rules, function(k) {\n",               file=UT)
    cat("  eval(parse(text=paste0(k,'(x)'))) }) )\n",           file=UT)
    cat("}\n\n",                                                file=UT)

    cat("getNUTS <- function(rules=NULL, cubes=NULL, x=NULL) {\n", file=UT)
    cat("  if(is.null(rules)) rules <- cubes(x)\n",             file=UT)
    cat("  nt <- sapply(rules, function(k) eval(parse(text=paste0(k,'_NUT()'))))\n", file=UT)
    cat("  nt <- as.data.frame(t(nt))\n",                       file=UT)
    cat("  nt$support <- as.integer(nt$support)\n",             file=UT)
    cat("  return(nt)\n",                                       file=UT)
    cat("}\n",                                                  file=UT)
    close(UT)
  }

  if(tidyrule.return) return(tidy_cubist)
}
