nc_open <- function (filename, write = FALSE, readunlim = TRUE, verbose = FALSE, 
          auto_GMT = TRUE, suppress_dimvals = FALSE, return_on_error = FALSE) 
{
  safemode = FALSE
  if (verbose) 
    print(paste("nc_open: entering, ncdf4 package version", 
                nc_version()))
  if ((!is.character(filename)) || (nchar(filename) < 1)) 
    stop("Passed a filename that is NOT a string of characters!")
  rv <- list()
  if (write) 
    rv$cmode <- 1
  else rv$cmode <- 0
  rv$id <- -1
  rv$error <- -1
  rv <- .C("R_nc4_open", as.character(filename), as.integer(rv$cmode), 
           id = as.integer(rv$id), error = as.integer(rv$error), 
           PACKAGE = "ncdf4")
  # if (rv$error != 0) {
  #   if (return_on_error) {
  #     print(paste("Error in nc_open trying to open file", 
  #                 filename, "(setting rv$error TRUE and returning because return_on_error==TRUE)"))
  #     rv$error = TRUE
  #     return(rv)
  #   }
  #   else stop(paste("Error in nc_open trying to open file", 
  #                   filename, "(return_on_error=", return_on_error, ")"))
  # }
  rv$error = FALSE
  if (verbose) 
    print(paste("nc_open: back from call to R_nc4_open, ncid=", 
                rv$id))
  nc <- list(filename = filename, writable = write, id = rv$id, 
             error = rv$error)
  attr(nc, "class") <- "ncdf4"
  am_windows = (length(grep("windows", .Platform$OS.type, ignore.case = TRUE)) != 
                  0)
  am_64 = (length(grep("x64", .Platform$r_arch, ignore.case = TRUE)) != 
             0)
  nc$safemode = FALSE
  if (nc$safemode && (!is.na(safemode)) && (safemode == FALSE)) {
    print(paste("***************** W A R N I N G *****************************"))
    print(paste("You are running on Windows-64 but have specified to force the"))
    print(paste("safemode OFF. Safemode protects from KNOWN BUGS of the netcdf"))
    print(paste("library on Windows-64. If you force safemode off YOU MUST NEVER"))
    print(paste("HAVE MORE THAN ONE NETCDF FILE OPEN AT THE SAME TIME, or you"))
    print(paste("may get errors of the type that the ncdf4 operators access the"))
    print(paste("WRONG FILE. You are proceeding at your own risk!!"))
    print(paste("***************************************************************"))
  }
  if (!is.na(safemode)) 
    nc$safemode = safemode
  nc$format = ncdf4_format(nc$id)
  if (verbose) 
    print(paste("file", filename, "is format", nc$format))
  nc$is_GMT = FALSE
  if (auto_GMT) {
    dimlen_xysize = ncdim_len(nc$id, "xysize")
    dimlen_side = ncdim_len(nc$id, "side")
    varid_dimension = ncvar_id(nc$id, "dimension")
    varid_spacing = ncvar_id(nc$id, "spacing")
    varid_z = ncvar_id(nc$id, "z")
    if ((dimlen_xysize != -1) && (dimlen_side != -1) && (varid_dimension != 
                                                         -1) && (varid_spacing != -1) && (varid_z != -1)) {
      varsize_dimension = ncvar_size(nc$id, varid_dimension)
      if ((length(varsize_dimension) == 1) && (varsize_dimension[1] == 
                                               dimlen_side)) {
        varsize_spacing = ncvar_size(nc$id, varid_spacing)
        if ((length(varsize_dimension) == 1) && (varsize_dimension[1] == 
                                                 dimlen_side)) {
          varsize_z = ncvar_size(nc$id, varid_z)
          if ((length(varsize_z) == 1) && (varsize_z[1] == 
                                           dimlen_xysize)) {
            nc$is_GMT = TRUE
          }
        }
      }
    }
  }
  groups <- list()
  groups[[1]] <- nc_get_grp_info(nc$id, "", nc$format)
  if (nc$format == "NC_FORMAT_NETCDF4") {
    gg <- nc_groups_below(groups[[1]], nc$format)
    for (i in nc4_loop(1, length(gg))) groups[[1 + i]] <- gg[[i]]
  }
  nc$groups <- groups
  nc$fqgn2Rindex <- list()
  for (i in 1:length(groups)) nc$fqgn2Rindex[[groups[[i]]$fqgn]] = i
  if (verbose) {
    print("Group info:")
    for (ig in 1:length(groups)) {
      print(paste("Group", ig, ": ", "name=", groups[[ig]]$name, 
                  "id=", groups[[ig]]$id, "fqgn= \"", groups[[ig]]$fqgn, 
                  "\"", "nvars=", groups[[ig]]$nvars, "ndims=", 
                  groups[[ig]]$ndims, "dimid="))
      print(groups[[ig]]$dimid)
    }
  }
  nc$ndims <- 0
  nc$natts <- 0
  tot_nvars_inc_dimvars <- 0
  for (ig in 1:length(groups)) {
    nc$ndims <- nc$ndims + nc$groups[[ig]]$ndims
    nc$natts <- nc$natts + nc$groups[[ig]]$natts
    tot_nvars_inc_dimvars <- tot_nvars_inc_dimvars + nc$groups[[ig]]$nvars
  }
  nc$dim <- list()
  nc$unlimdimid <- -1
  dimnames <- character()
  global_dim_counter <- 0
  for (ig in 1:length(groups)) {
    for (idim in nc4_loop(1, groups[[ig]]$ndims)) {
      dimid2use <- groups[[ig]]$dimid[idim]
      if (is.na(dimid2use)) {
        print(paste("Error, got a NA as a dimid2use ... group=", 
                    groups[[ig]]$name, " which has ndims=", groups[[ig]]$ndims))
        print("Here are the dimids from the ncgroup object:")
        print(groups[[ig]]$dimid)
        if (return_on_error) {
          print(paste("Error in nc_open trying to open file", 
                      filename, "(setting rv$error TRUE and returning because return_on_error==TRUE)"))
          nc$error = TRUE
          return(nc)
        }
        else stop("Error, cannot have NAs as dimids!")
      }
      if (verbose) 
        print(paste("nc_open: getting dim info for dim number", 
                    idim, "in group \"", groups[[ig]]$name, "\" dim ID=", 
                    dimid2use))
      d <- ncdim_inq(groups[[ig]]$id, dimid2use)
      if (groups[[ig]]$name != "") 
        d$name <- paste(groups[[ig]]$fqgn, "/", d$name, 
                        sep = "")
      d$group_index <- ig
      d$group_id <- groups[[ig]]$id
      d$id <- dimid2use
      tt <- ncvar_id(groups[[ig]]$id, nc4_basename(d$name))
      d$dimvarid = ncdf4_make_id(id = tt, group_index = ig, 
                                 group_id = groups[[ig]]$id, list_index = -1, 
                                 isdimvar = TRUE)
      if (verbose) 
        print(paste(".....dim name is", d$name, "  id=", 
                    d$id, "  len=", d$len, "     dimvarid=", d$dimvarid$id))
      if (d$dimvarid$id == -1) {
        if (!suppress_dimvals) 
          d$vals <- 1:d$len
        d$units <- ""
        d$create_dimvar <- FALSE
      }
      else {
        if (verbose) 
          print(paste("nc_open: getting dimvar info for dim ", 
                      d$name))
        attv <- ncatt_get_inner(d$dimvarid$group_id, 
                                d$dimvarid$id, "units")
        if (attv$hasatt) 
          d$units <- attv$value
        else d$units <- ""
        attv <- ncatt_get_inner(d$dimvarid$group_id, 
                                d$dimvarid$id, "calendar")
        if (attv$hasatt) 
          d$calendar <- attv$value
        if (!suppress_dimvals) {
          if (d$unlim && (!readunlim)) 
            d$vals <- rep(NA, d$len)
          else d$vals <- ncvar_get_inner(d$dimvarid$group_id, 
                                         d$dimvarid$id, default_missval_ncdf4(), verbose = verbose)
        }
        d$create_dimvar <- TRUE
      }
      attr(d, "class") <- "ncdim4"
      global_dim_counter <- global_dim_counter + 1
      if (verbose) {
        print("------------------------------")
        print("Here is new dim:")
        print(paste("Global index=", global_dim_counter, 
                    "name=", d$name, "len=", d$len, "unlim=", d$unlim, 
                    "id=", d$id, "dimvarid=", d$dimvarid$id, "units=", 
                    d$units))
        print("------------------------------")
      }
      if (d$unlim && (nc$unlimdimid == -1)) 
        nc$unlimdimid = global_dim_counter
      nc$dim[[global_dim_counter]] <- d
      dimnames[global_dim_counter] <- d$name
      if (verbose) 
        print(paste(".......nc_open: done processing dim ", 
                    d$name))
    }
  }
  attr(nc$dim, "names") <- dimnames
  if (verbose) {
    print("nc_open: setting dim$<names> to:")
    print(dimnames)
  }
  if (verbose) 
    print(paste("nc_open: getting var info.  Number of vars (INCLUDING dimvars)=", 
                tot_nvars_inc_dimvars))
  nc$nvars <- 0
  nc$var <- list()
  varnames <- character()
  have_warned_noncompliant <- FALSE
  for (ig in 1:length(groups)) {
    for (ivar in nc4_loop(1, groups[[ig]]$nvars)) {
      name <- ncvar_name(groups[[ig]]$id, ivar - 1)
      if (verbose) 
        print(paste("Working on group", ig, "(of", length(groups), 
                    "), var", ivar, "(of", groups[[ig]]$nvars, 
                    "), name=", name))
      if (ncdim_id(groups[[ig]]$id, name) == -1) {
        if (verbose) 
          print(paste("nc_open var loop: will process with group id=", 
                      groups[[ig]]$id, " varid=", ivar, "  var name=", 
                      name))
        v <- ncvar_inq(groups[[ig]]$id, ivar - 1)
        attr(v, "class") <- "ncvar4"
        nc$nvars <- nc$nvars + 1
        v$group_index = ig
        v$id$list_index = nc$nvars
        if (groups[[ig]]$name != "") 
          v$name <- paste(groups[[ig]]$fqgn, "/", v$name, 
                          sep = "")
        if ((nc$format == "NC_FORMAT_NETCDF4") || (nc$format == 
                                                   "NC_FORMAT_NETCDF4_CLASSIC")) {
          chunkrv = ncvar_inq_chunking(groups[[ig]]$id, 
                                       ivar - 1, v$ndims)
          v$chunksizes = chunkrv$chunksizes
          v$storage = chunkrv$storage
          comprv = ncvar_inq_deflate(groups[[ig]]$id, 
                                     ivar - 1)
          v$shuffle = comprv$shuffle
          if (comprv$deflate == 0) 
            v$compression = NA
          else v$compression = as.integer(comprv$deflate_level)
        }
        else {
          v$chunksizes = NA
          v$storage = 1
          v$shuffle = FALSE
          v$compression = NA
        }
        v$dims <- list()
        varunlim <- FALSE
        if (v$ndims > 0) {
          for (j in 1:v$ndims) {
            dimid2find = v$dimids[j]
            matchidx = -1
            for (iidim in 1:nc$ndims) {
              if (nc$dim[[iidim]]$id == dimid2find) {
                matchidx = iidim
                break
              }
            }
            if (matchidx == -1) 
              stop(paste("internal error, did not find dim with id=", 
                         dimid2find, "in dim list!"))
            v$dim[[j]] = nc$dim[[matchidx]]
            if (v$dim[[j]]$unlim) 
              varunlim <- TRUE
            v$varsize <- append(v$varsize, v$dim[[j]]$len)
          }
        }
        v$unlim <- varunlim
        if ((nc$format != "NC_FORMAT_NETCDF4") && varunlim) 
          v$storage = 2
        found_mv <- FALSE
        v$make_missing_value <- FALSE
        mv <- ncatt_get_inner(groups[[ig]]$id, ivar - 
                                1, "missing_value")
        if (mv$hasatt) {
          found_mv <- TRUE
          v$missval <- mv$value
          v$make_missing_value <- TRUE
        }
        else {
          mv <- ncatt_get_inner(groups[[ig]]$id, ivar - 
                                  1, "_FillValue")
          if (mv$hasatt) {
            found_mv <- TRUE
            v$missval <- mv$value
            v$make_missing_value <- TRUE
          }
        }
        if (!found_mv) {
          if ((v$prec == "float") || (v$prec == "double")) 
            v$missval <- default_missval_ncdf4()
          else v$missval <- NA
        }
        if ((v$prec == "float") || (v$prec == "double")) {
          if (storage.mode(v$missval) == "character") {
            v$missval <- as.double(v$missval)
            if (!have_warned_noncompliant) {
              print(paste("WARNING file", filename, "is not compliant netCDF; variable", 
                          name, " is numeric but has a character-type missing value! This is an error!  Compensating, but you should fix the file!"))
              have_warned_noncompliant <- TRUE
            }
          }
        }
        ao <- ncatt_get_inner(groups[[ig]]$id, ivar - 
                                1, "add_offset")
        if (ao$hasatt) {
          v$hasAddOffset <- TRUE
          v$addOffset <- ao$value
        }
        else v$hasAddOffset <- FALSE
        sf <- ncatt_get_inner(groups[[ig]]$id, ivar - 
                                1, "scale_factor")
        if (sf$hasatt) {
          v$hasScaleFact <- TRUE
          v$scaleFact <- sf$value
        }
        else v$hasScaleFact <- FALSE
        nc$var[[nc$nvars]] <- v
        varnames <- append(varnames, v$name)
        if (verbose) {
          print("-----------------------")
          print("Here is new var:")
          print(paste("name=", v$name, "  group_id=", 
                      v$id$group_id, "  id=", v$id$id, "   ndims=", 
                      v$ndims, "   prec=", v$prec))
          print("size=")
          print(v$size)
          print("dimids=")
          print(v$dimids)
        }
      }
    }
  }
  if ((nc$nvars == 1) && nc$is_GMT) {
  }
  attr(nc$var, "names") <- varnames
  if (nc$safemode) {
    rv = .C("R_nc4_close", as.integer(nc$id), PACKAGE = "ncdf4")
    nc$id = -1
  }
  if (verbose) 
    print(paste("nc_open: leaving for ncid=", nc$id))
  return(nc)
}