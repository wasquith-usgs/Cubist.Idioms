"cubist_asPerl_idioms" <-
function(cubist_object, cubist_tag="C0", nut_digits=3,
         parentpmdir="MAP", path=".", prefix="",
         tidyrule.return=FALSE, sample_str="", ...) {
  if(class(cubist_object)[1] == "tbl_df") {
    tidy_cubist <- cubist_object
  } else if(! class(cubist_object) == "cubist") {
    warning("cubist variable is not a 'cubist' object")
    return()
  } else {
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

  PM <- file(paste0(path,"cubist",cubist_tag,".pm"), open="w")

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

  cat(paste0("package ",parentpmdir,"::",prefix,"cubist",cubist_tag,";\n"), file=PM)
  cat("use strict;\n", file=PM)
  cat(paste0("use vars qw( @ISA @EXPORT $BRA",cubist_tag,
                                 " $RULE_BRA",cubist_tag,
                                      " $NUT",cubist_tag,
                                 " $RULE_NUT",cubist_tag,
                               " $SAMPLE_STR",cubist_tag," );\n"), file=PM)
  cat("use Exporter;\n", file=PM)
  cat("@ISA    = qw( Exporter );\n", file=PM)
  cat(paste0("@EXPORT = qw( $BRA",cubist_tag,
                     " $RULE_BRA",cubist_tag,
                          " $NUT",cubist_tag,
                     " $RULE_NUT",cubist_tag,
                   " $SAMPLE_STR",cubist_tag," );\n\n"), file=PM)
  cat(paste0(  "     $BRA",cubist_tag," = {};\n"), file=PM)
  cat(paste0(  "$RULE_BRA",cubist_tag," = {};\n"), file=PM)
  cat(paste0(  "     $NUT",cubist_tag," = {};\n"), file=PM)
  cat(paste0(  "$RULE_NUT",cubist_tag," = {};\n"), file=PM)
  cat(paste0("\n$SAMPLE_STR",cubist_tag,' = "',sample_str,'"',";\n"), file=PM)
  cat("\n# --- anonymous subroutines into the hash references\n\n",  file=PM)

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
    txtn <- paste0("$NUT",cubist_tag,"->{",txt,"_NUT}"," = sub {\n",
               "  my @nut = (",tidy_cubist$support[i],", ",
                               tidy_cubist$mean[i],   ", ",
                               tidy_cubist$min[i],    ", ",
                               tidy_cubist$max[i],    ", ",
                               tidy_cubist$error[i],  ");\n",
               "  my @key = qw(support mean min max error);\n",
               "  my $zz  = {};\n",
               "  for(0..$#nut) { $zz->{$key[$_]} = $nut[$_] };\n",
               "  $zz;\n",
               "};\n\n")
    cat(txtn, file=PM)

    f <- tidy_cubist$RHS[i] # the Cubist regression equation
    f <- gsub(" ([0-9A-Z\\_a-z]+)", " $x->{\\1}", tidy_cubist$RHS[i])
    f <- noquote(paste0("$BRA",cubist_tag,"->{",nmfb,"_",nmfa,"}",
                        " = sub {\n",
                        "  my $x = shift;\n",
                        "  ",f,";\n};"))

    r <- unlist(strsplit(tidy_cubist$LHS[i], split="&"))
    r <- gsub("^\\s+", "", r) # strip leading spaces
    r <- gsub("\\s+$", "", r) # strip trailing spaces
    r <- paste("x$",r, sep="") # x is a data.frame
    r <- paste(r, collapse=" & ") # Cubist rule as function assigned
    # returning the name of the branch (i.e. MODA_COM001_BRA001) function
    r <- gsub("x\\$", "$x->{", r)
    r <- gsub("\\s=", "} =",   r)
    r <- gsub("\\s<", "} <",   r)
    r <- gsub("\\s>", "} >",   r)
    r <- gsub("\\s!", "} !",   r)
    r <- gsub(" & ", " and ",  r)
    # The nut function can actually be bypassed because in Perl run time,
    # we only need to add _NUT to the returned from the RULE_BRA.
    #n <- noquote(paste0("$RULE_NUT",cubist_tag,"->{",nmfb,"_",nmra,"}",
    #             " = sub {\n  my $x = shift;\n",
    #             "  my $zz = (",r,") ? 1 : 0;\n",
    #             "  ($zz) ? '",nmfb,"_",nmfa,"_NUT' : 0;",
    #             "\n};"))
    r <- noquote(paste0("$RULE_BRA",cubist_tag,"->{",nmfb,"_",nmra,"}",
                 " = sub {\n  my $x = shift;\n",
                 "  my $zz = (",r,") ? 1 : 0;\n",
                 "  ($zz) ? '",nmfb,"_",nmfa,"' : 0;",
                 "\n};"))
    # prepending boiler plate and ensuring nonvectorized

    cat(f, file=PM); cat("\n\n", file=PM)
    #cat(n, file=PM); cat("\n\n", file=PM)
    cat(r, file=PM); cat("\n\n", file=PM)
    cat("# on to next rule \n\n", file=PM)
    previous_committee <- com
  }
  cat("\n1;\n", file=PM)
  close(PM)
}
