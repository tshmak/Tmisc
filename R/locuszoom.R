locuszoom <- function(data=NULL, ...,
                      type=c("auto", "metal", "epacts"),
                      chr.col=NULL, pos.col=NULL, marker.col=NULL, pval.col=NULL,
                      annot.col=NULL,
                      ld=NULL,
                      locuszoom.dir="~/software/locuszoom_tim/",
                      ld.cache=tempfile(),
                      Rplink.opts=list(),
                      plot=TRUE,
                      verbose=debug,
                      debug=FALSE,
                      help=FALSE,
                      keepfiles=FALSE) {

  #' @title Wrapper around locuszoom (1.4)
  #' @details Note that the conf/m2zfast.conf file in locuszoom must be set up to point to the various
  #' dependent programs first. The github version of locuszoom (statgen/locuszoom-standalone) appears to be missing
  #' the conf/m2zfast.conf file as of 19/4/2018. If that's the case, simply clone a copy of
  #' conf/dist.conf as conf/m2zfast.conf, and modify the various variables.

  #### Work in new directory ####
  curdir <- getwd()
  setwd(tempdir())
  newdir <- tempfile("lz")
  dir.create(newdir)
  setwd(newdir)
  wdir <- getwd()
  if(keepfiles) {
    on.exit({
      setwd(curdir)
      try(detach("file:data4graph.Rdata"), silent=T)
    })
  } else {
    on.exit({
      setwd(curdir)
      unlink(wdir)
      try(detach("file:data4graph.Rdata"), silent=T)
    })
  }

  #### Locuszoom exec ####
  locuszoom.exec <- paste0(locuszoom.dir, "/bin/locuszoom")
  cmd <- locuszoom.exec

  #### Options ####
  commonOptions <- list(scalar=c("build", "pop", "source"),
                        region=c("chr", "start", "end", "flank"),
                        data=c("metal", "epacts"),
                        metal.columns=c("pvalcol", "markercol"),
                        epacts.columns=c("epacts-chr-col", "epacts-beg-col", "epacts-end-col", "epacts-pval-col"),
                        ref=c("refsnp", "refgene"),
                        control=c("verbose"),
                        annotation=c("annot", "ld", "recomb"))

  #### Load the python-specific options from m2zfast.py ####
  python.file <- paste0(locuszoom.dir, "/src/m2zfast.py")
  python.script <- readLines(python.file)
  python.opts <- rep("", length(python.script))
  for(i in 1:length(python.script)) {
    pattern <- '^[[:blank:]]*parser\\.add_option\\("--([A-z0-9-]*)".*$'
    if(grepl(pattern, python.script[i])) {
      python.opts[i] <- sub('^[[:blank:]]*parser\\.add_option\\("--([A-z0-9-]*)".*$', "\\1", python.script[i])
    }
  }
  python.opts <- python.opts[python.opts != ""]
  python.opts <- gsub("-", ".", python.opts)
  # print(python.opts)

  if(help) {
    return(list(commonOptions=commonOptions, python.options=python.opts))
  }

  options <- list(...)
  # options[['cache']] <- ld.cache

  #### Checks ####
  if(any(sapply(options, is.list))) {
    stop("Options included in ... shouldn't be lists")
  }

  if(any(sapply(options, length) > 1)) {
    stop("Options included in ... shouldn't be non-scalars")
  }

  if(is.null(data)) {
    stop("data must be specified.")
  }

  if(!is.data.frame(data)) {
    stop("data should be a data.frame")
  }

  #### Read columns ####
  silent <- FALSE
  colnames <- colnames(data)

  chr.j <- match.col(colnames, c(options[['epacts.chr.col']], chr.col), c("CHROMOSOME", "#CHROM"),
                       silent=silent, ignore.multiple.matches=T)
  pos.j <- match.col(colnames, pos.col, c("POSITION", "BP"), silent=silent, ignore.multiple.matches=T)
  start.j <- match.col(colnames, c(options[['epacts.beg.col']]), c("begin", "start"), silent=silent,
                         ignore.multiple.matches=T)
  end.j <- match.col(colnames, c(options[['epacts.end.col']]), c("end"), silent=silent, ignore.multiple.matches=T)
  marker.j <- match.col(colnames, c(options[['markercol']], marker.col), c("marker", "SNP", "rsid"), silent=silent,
                          ignore.multiple.matches=T)
  pval.j <- match.col(colnames, c(options[['pvalcol']], options[['epacts.pval.col']], pval.col), "^p[_\\.-]?(val|$)",
                        silent=silent, regex=T)
  annot.j <- match.col(colnames, c(annot.col), silent=silent)

  if(length(pval.j) == 0) {
    stop("pval column cannot be identified.")
  }

  if(length(start.j) && !length(end.j)) {
    start.j <- integer(0)
    message("Cannot find end column given start/begin column.")
  } else if(length(end.j) && !length(start.j)) {
    end.j <- integer(0)
    message("Cannot find start column given end column.")
  } else if(length(start.j) == 0 && length(end.j) == 0) {
    if(length(pos.j)) {
      end.j <- start.j <- pos.j
    }
  }


  #### Determining whether we should use epacts or metal ####
  type <- match.arg(type)
  if(type == "auto") {
    if(length(marker.j)) {
      type <- "metal"
    } else if(length(chr.j) && (length(pos.j) || (length(start.j) && length(end.j)))) {
      type <- "epacts"
    } else {
      stop("Cannot identify what type of data this is. Either specify chr, start, end or marker columns.")
    }
    message(paste("We're treating the data as a", type, "file."))
  }

  #### colnames ####
  # colnames <- colnames(data)
  if(type == "metal") {
    stopifnot(length(marker.j) > 0)
    data.to.write <- data[, c(marker.j, pval.j)]
    colnames(data.to.write) <- c("MarkerName", "P-value")
    # options[['markercol']] <- colnames[marker.j]
    # options[['pvalcol']] <- colnames[pval.j]
    if(is.null(options[['build']])  && (is.null(options[['no.ld']]) || !options[['no.ld']]) &&
       length(start.j) == 0 && is.null(ld)) {
      options[['build']] <- "hg19"
      message("build assumed to be hg19") # Default if none specified
    }

  } else if(type == "epacts") {
    stopifnot(length(chr.j) > 0)
    stopifnot(length(start.j) > 0)
    data.to.write <- data[, c(chr.j, start.j, end.j, pval.j)]
    colnames(data.to.write) <- c("#CHROM", "BEGIN", "END", "PVALUE")
    # options[['epacts.chr.col']] <- colnames[chr.j]
    # options[['epacts.beg.col']] <- colnames[start.j]
    # options[['epacts.end.col']] <- colnames[end.j]
  }

  if(length(annot.j)) {
    data.to.write <- cbind(data.to.write, data[, annot.j, drop=F])
    options[['annotCol']] <- annot.col
  }

  data.file <- tempfile("locuszoom.data")
  write.table.tim(data.to.write, file=data.file)
  options[[type]] <- data.file
  options[['epacts.chr.col']] <- options[['epacts.beg.col']] <- options[['epacts.end.col']] <- options[['markercol']] <-
    options[['pvalcol']] <- options[['epacts.pval.col']] <- NULL

  #### Range ####
  if(is.null(options[['refgene']]) && is.null(options[['refsnp']]) && is.null(options[['hitspec']]) &&
     (is.null(options[['chr']]) && is.null(options[['start']]) && is.null(options[['end']]))) {
    if(length(start.j) && length(chr.j)) {
      options[['chr']] <- unique(data[,chr.j])
      if(length(options[['chr']]) > 1) {
        stop("There are multiple chromosomes in data.")
      }
      options[['start']] <- min(data[,c(start.j, end.j)], na.rm = T)
      options[['end']] <- max(data[,c(start.j, end.j)], na.rm = T)
    }
  }

  #### Defaults ####
  if(is.null(options[['source']]) && !(is.null(options[['build']])) &&
     (is.null(options[['no.ld']]) || !options[['no.ld']]) && is.null(ld)) {
    if(options[['build']] == "hg19") {
      options[['source']] <- "1000G_March2012"
      message("Source assumed to be '1000G_March2012'")
    } else if(options[['build']] == "hg18") {
      options[['source']] <- "1000G_June2010"
      message("Source assumed to be '1000G_June2010'")
    } else if(options[['build']] == "hg17") {
      options[['source']] <- "hapmap"
      message("Source assumed to be 'hapmap'")
    }
  }
  if(is.null(options[['pop']]) && !is.null(options[['source']]) &&
     (is.null(options[['no.ld']]) || !options[['no.ld']]) && is.null(ld)) {
    if(options[['source']] %in% c("hapmap", "1000G_June2010", "1000G_Aug2009")) {
      options[['pop']] <- "CEU"
      message("pop assumed to be 'CEU'")
    } else if(options[['source']] %in% c("1000G_Nov2010", "1000G_March2012")) {
      options[['pop']] <- "EUR"
      message("pop assumed to be 'EUR'")
    }
  }

  #### ld ####
  if(is.null(ld) && !file.exists(ld.cache)) {
    if((is.null(options[['source']]) || is.null(options[['pop']])) && is.null(options[['no.ld']])) {
      options[['no.ld']] <- TRUE
      message("Population not specified for calculating LD. No LD will be calculated.")
    }
  } else {
    if(is.data.frame(ld)) {
      stopifnot(all(c("snp1", "snp2", "dprime", "rsquare") %in% colnames(ld)))
      ld.file <- tempfile()
      write.table.tim(ld, file=ld.file)
      options[['ld']] <- ld.file
    } else if(is.character(ld) && length(ld) == 1 && file.exists(ld)) {
      options[['ld']] <- ld
    } else if(is.character(ld) && length(ld) == 1 && file.exists(paste0(ld, ".bed"))) {
      # This is a PLINK bfile
      if(!("package:Rplink" %in% search())) {
        stop(paste("You're specifying a plink bfile for ld. Please ensure Rplink is loaded first. "))
      }
      # Run locuszoom again to find out the range of the plot.
      if(verbose) cat("Rerun locuszoom to obtain the range of the plot.\n")
      options2 <- options
      options2$data <- data.to.write
      options2$type <- type
      options2$locuszoom.dir <- locuszoom.dir
      options2$ld.cache <- ld.cache
      options2$plot <- F
      options2$verbose <- debug
      options2$no.ld <- T

      test <- do.call("locuszoom", options2)
      plink.range <- range(test$metal$pos_int)
      plink.chr <- unique(test$metal$chr)
      cat("Range of LD region determined to be:\n")
      cat("Chr", plink.chr, ":", plink.range[1], "-", plink.range[2], "\n")

      # ref SNP
      refsnp <- test$args$refsnpName
      message(paste("Lead SNP is", refsnp))
      names(refsnp) <- test$args$refsnp
      refsnp.chr <- as.integer(sub("^chr([0-9]*):([0-9]*)", "\\1", test$args$refsnp))
      refsnp.pos <- as.integer(sub("^chr([0-9]*):([0-9]*)", "\\2", test$args$refsnp))
      if(!is.null(Rplink.opts[['replace.bim']])) {
        o <- Rplink.opts[['replace.bim']]
        if(is.data.frame(o)) {
          bim <- o
        } else if(is.character(o) && length(o) == 1 && file.exists(o)) {
          bim <- read.bim(o, add.ext = T)
        } else {
          stop("Something wrong with your replace.bim option.")
        }
      } else {
        bim <- read.bim(ld)
      }
      bim.chr <- sub("^chr", "", bim$CHROM, ignore.case = T)
      if(!(refsnp %in% bim$ID)) {
        findsnp <- which(bim.chr == refsnp.chr & bim$POS == refsnp.pos)
        if(length(findsnp) == 1) {
          Refsnp <- bim$ID[findsnp]
          message(paste("Lead SNP assumed to be", Refsnp, "in .bim file."))
        } else {
          stop("Cannot find refsnp in .bim file.")
        }
      } else {
        Refsnp <- refsnp
      }

      cat("Running plink to obtain R2\n")
      Rplink.opts[['cmd']] <- "--r2 dprime"
      Rplink.opts[['bfile']] <- ld
      Rplink.opts[['ld.snp']] <- Refsnp
      Rplink.opts[['from.bp']] <- plink.range[1]
      Rplink.opts[['to.bp']] <- plink.range[2]
      Rplink.opts[['chr']] <- plink.chr

      if(is.null(Rplink.opts[['ld.window']])) Rplink.opts[['ld.window']] <- 10000L
      if(is.null(Rplink.opts[['ld.window.kb']])) Rplink.opts[['ld.window.kb']] <- 10000L
      if(is.null(Rplink.opts[['ld.window.r2']])) Rplink.opts[['ld.window.r2']] <- 0
      if(is.null(Rplink.opts[['silent']])) Rplink.opts[['silent']] <- !debug
      plink.ld <- do.call("plink.table", Rplink.opts)

      #### Integrate the various data sources ####
      # db = test$metal
      # db <- test$metal
      # rownames(db) <- paste0("chr", db$chr, ":", db$pos_int)

      # bim / plink.ld
      bim.chr.pos <- bim[,c("CHROM", "POS")]
      dup <- duplicated(bim.chr.pos) | duplicated(bim.chr.pos, fromLast = T)
      bim.dedup <- bim[!dup, ]
      rownames(bim.dedup) <- paste0("chr", bim.chr[!dup], ":", bim.dedup$POS)

      # plink.ld
      plink.ld$marker_A <- paste0("chr", plink.ld$CHR_A, ":", plink.ld$BP_A)
      plink.ld$marker_B <- paste0("chr", plink.ld$CHR_B, ":", plink.ld$BP_B)

      # data.to.write
      if(type == "metal") {
        if(refsnp %in% plink.ld$SNP_A) {
        # if(any(plink.ld$SNP_B %in% data.to.write[,1] & plink.ld$SNP_A %in% data.to.write[,1])) {
          # Banking on the bim file and data.to.write having the same marker names.
          plink.ld2 <- data.frame(snp1=plink.ld$SNP_A, snp2=plink.ld$SNP_B,
                                  dprime=plink.ld$DP, rsquare=plink.ld$R2)
        } else if(refsnp %in% plink.ld$marker_A) {
          plink.ld2 <- data.frame(snp1=plink.ld$marker_A, snp2=plink.ld$marker_B,
                                  dprime=plink.ld$DP, rsquare=plink.ld$R2)
        } else if(length(chr.j) && length(start.j) && length(marker.j)) {
          # Try matching by chromosome and position
          data.chr.pos <- data[, c(chr.j, start.j)]
          data.chr <- sub("^chr", "", data[, chr.j], ignore.case = T)
          # data.refsnp.chr <- data[data[, marker.j] == refsnp, chr.j]
          # data.refsnp.pos <- data[data[, marker.j] == refsnp, pos.j]

          dup <- duplicated(data.chr.pos) | duplicated(data.chr.pos, fromLast = T)
          d2 <- data.to.write[!dup, ]
          rownames(d2) <- paste0("chr", data.chr[!dup], ":", data[!dup,start.j])

          plink.ld2 <- data.frame(snp1=d2[plink.ld$marker_A, 1],
                                  snp2=d2[plink.ld$marker_B, 1],
                                  dprime=plink.ld$DP, rsquare=plink.ld$R2)
        } else {
          stop("Cannot match .bfile with data. Try specifying all of chr, pos, and marker")
        }
      } else if(type == "epacts") {
        # if(any(grep("^rs", plink.ld$SNP_B))) {
        if(refsnp %in% plink.ld$SNP_A) {
          plink.ld2 <- data.frame(snp1=plink.ld$SNP_A, snp2=plink.ld$SNP_B,
                                  dprime=plink.ld$DP, rsquare=plink.ld$R2)
        } else if(refsnp %in% plink.ld$marker_A) {
          plink.ld2 <- data.frame(snp1=plink.ld$marker_A, snp2=plink.ld$marker_B,
                                  dprime=plink.ld$DP, rsquare=plink.ld$R2)
        } else if(length(chr.j) && length(start.j) && length(marker.j)) {
          # Try matching by chromosome and position
          if(!(refsnp %in% data[,marker.j])) {
            stop("refsnp not found in marker column in data file.")
          }
          data.chr.pos <- data[, c(chr.j, start.j)]
          data.chr <- sub("^chr", "", data[, chr.j], ignore.case = T)
          dup <- duplicated(data.chr.pos) | duplicated(data.chr.pos, fromLast = T)
          d2 <- data[!dup, c(marker.j, pval.j)]
          rownames(d2) <- paste0("chr", data.chr[!dup], ":", data[!dup,start.j])
          plink.ld2 <- data.frame(snp1=d2[plink.ld$marker_A, 1],
                                  snp2=d2[plink.ld$marker_B, 1],
                                  dprime=plink.ld$DP, rsquare=plink.ld$R2)

        } else {
          stop("Cannot match .bfile with data. Try specifying all of chr, pos, and marker")
        }

      }

      plink.ld2 <- subset(plink.ld2, !is.na(snp1) & !is.na(snp2))
      ld.file <- tempfile()
      write.table.tim(plink.ld2, file=ld.file)
      options[['ld']] <- ld.file
    }
  }

  #### no.ld ####
  if(!(is.null(options[['no.ld']])) && options[['no.ld']] && length(start.j) == 0 && is.null(options[['build']])) {
    options[['build']] <- "hg19" # Just so that locuszoom doesn't complain.
  }

  #### Parse other options and run! ####
  opts1 <- options[names(options) %in% python.opts] # Python options
  opts2 <- options[!(names(options) %in% python.opts)] # Non-python (probably R) options
  cmd <- paste(cmd, parse.options(opts1))
  cmd <- paste(cmd, parse.options(opts2, pre="", sep="="))
  if(verbose) {
    cat(cmd, "\n")
  }
# browser()
  returnvalue <- system(cmd, ignore.stdout = !verbose)
  if(returnvalue > 0) stop(returnvalue)

  #### Load the objects back ####
  newdir <- list.dirs()
  stopifnot(length(newdir) == 2 && newdir[1] == ".")
  setwd(newdir[2])

  #### Load the data into the locuszoom() environment. ####
  require(grid)
  require(lattice)
  E <- attach('data4graph.Rdata', warn.conflicts = FALSE)
  all.objs <- ls(all=T, envir=E)
  assign("E", E, envir=E)
  for(i in 1:length(all.objs)) {
    obj <- get(all.objs[i], envir=E)
    if(is.function(obj)) {
      call <- paste0("environment(", all.objs[i], ") <- E")
      eval(parse(text=call), envir = E) # Change the enclosing environments of the functions from R_GlobalEnv to E
    }
  }

  #### zplot() creates globals, but we don't want them. ####
  rm.globals <- function(unwanted.globals, warn=TRUE) {
    keep <- list()
    for(i in 1:length(unwanted.globals)) {
      ug <- unwanted.globals[i]
      if(exists(ug, envir=.GlobalEnv, inherits = FALSE)) {
        if(warn) {
          stop(paste("Unfortunately, the R script over-writes",ug,
                     "in the Global environment.",
                     "Suggests renaming it to something else."))
        } else {
          keep[['ug']] <- get(ug, envir=.GlobalEnv)
          rm(list=ug, envir=.GlobalEnv)
        }
      }
    }
    return(invisible(keep))
  }

  #### Redraw the graph ####
  unwanted.globals <- c("omittedGenes")
  rm.globals(unwanted.globals, warn=T)
  if(plot) {
    invisible(capture.output(with(E,
         eval(zplot(metal,ld,recrate,refidx,nrugs=nrugs,args=args,postlude=args[['postlude']]),
              envir=E))))
  }
  tokeep <- rm.globals(unwanted.globals, warn=F)

  to.return <- with(E, list(metal=metal, ld=ld, recrate=recrate, args=args, refSnp=refSnp))
  to.return <- c(to.return, list(wdir=wdir, ld.cache=ld.cache, omittedGenes=tokeep$omittedGenes))

  return(invisible(to.return))

}
