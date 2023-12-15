source("/home/lagee/Documents/ghProjects/cqmaTools/R/util.R")
#-------------------------------------
# set up
# ATENCAO --->> NAO ESQUECER DE MODIFICAR A MASSA MOLAR DO GAS NA LINHA 200
#-------------------------------------

setwd("/home/lagee/Documents/ghProjects/cqmaTools")

library(devtools)
devtools::load_all()


#s = c('alf','rba','san','tab','tef')
s <- "rba"

for (site in s) {
  gas <- "ch4"   # ATENCAO --->> NAO ESQUECER DE MODIFICAR A MASSA MOLAR DO GAS NA LINHA 200 (CO2=12 /CO=28 /CH4=16 /N2O=44)
    data.in.path <- "/home/lagee/Documents/alber/test/briefcase"
  #file.path(data.in.path, "rba.co2_bkgTable.txt")
  #file.path(data.in.path, "RBA_briefdata.txt")
  #-------------------------------------
  # get data resulting from scripts background.R & briefcases.R
  #-------------------------------------
  briefcase.df <- read.table(file.path(paste(data.in.path,'/',toupper(site),"_briefdata.txt", sep = '')))
  profile.df <- read.table(file.path(paste(data.in.path,'/' ,site, ".", gas,"_bkgTable.txt", sep = '')))
  profbud.df <- merge(x = profile.df, y = briefcase.df,
                      by.x = c("profile", "height"),
                      by.y = c("profile", "planmts"),
                      all.x = TRUE)
  
  
  #-------------------------------------
  # FILTRO PARA PERFIS REMOVIDOS QUE NAO TENHAM FRASCOS ABAIXO DE 1500m
  
  # Reuni a data
  profbud.df["date"] <- paste(
    apply(profbud.df[ , c("year", "month", "day")] , 1 , paste , collapse = "-" ),
    sep = " "
  )
  profbud.df$date = as.Date(profbud.df$date, "%Y-%m-%d")
  
  #Filtro
  
  e_perfis = split(profbud.df, profbud.df$date)
  
  final_sem_1500 = list()
  final_no_data = list()
  
  for (ff in 1:length(e_perfis)) {
    df = do.call(rbind, e_perfis[ff])
    df_m = subset(df, df$height <= 1500)
    
    if (nrow(df_m) >= 1) {
      final_sem_1500[[ff]] <- df
    } else{
      dat <- data.frame(df$date[1])
      final_no_data[[ff]] <- dat
    }
  }
  
  final_sem_1500 = do.call(rbind, final_sem_1500)
  final_no_data = do.call(rbind, final_no_data)
  print(final_no_data)
  
  profbud.df = final_sem_1500[,-49]
  #-------------------------------------
  # MODELS OF TEMPERATURE AND PRESSURE
  # h is height
  #-------------------------------------
  ALFpres <- function(h){return(4E-6 *  h^2 - 0.1106 * h + 1004.5)}
  ALFtemp <- function(h){return(4E-7 *  h^2 - 0.0074 * h + 32.639)}
  RBApres <- function(h){return(4E-6 *  h^2 - 0.1209 * h + 1041.5)}
  RBAtemp <- function(h){return(2E-11 * h^2 - 0.006 * h + 29.996)}
  SANtemp <- function(h){return(3E-7 *  h^2 - 0.0067 * h + 29.92)}
  SANpres <- function(h){return(0    *  h^2 - 0.1054 * h + 1093.7)}
  TABtemp <- function(h){return(2E-7 *  h^2 - 0.0063 * h + 28.507)}
  TABpres <- function(h){return(4E-6 *  h^2 - 0.1121 * h + 1006.6)}
  TEFtemp <- function(h){return(2E-7 *  h^2 - 0.0063 * h + 28.507)}
  TEFpres <- function(h){return(4E-6 *  h^2 - 0.1121 * h + 1006.6)}
  #
  tempfunc <- function(h){return(rep(NA, times = length(h)))}
  presfunc <- function(h){return(rep(NA, times = length(h)))}
  if(toupper(site) == "ALF"){
    tempfunc <- ALFtemp
    presfunc <- ALFpres
  }else if(toupper(site) == "RBA"){
    tempfunc <- RBAtemp
    presfunc <- RBApres
  }else if(toupper(site) == "SAN"){
    tempfunc <- SANtemp
    presfunc <- SANpres
  }else if(toupper(site) == "TAB"){
    tempfunc <- TABtemp
    presfunc <- TABpres
  }else if(toupper(site) == "TEF"){
    tempfunc <- TEFtemp
    presfunc <- TEFpres
  }else{
    print(paste("There is no interpolation function for the given sample site: ", site, sep = ""))
  }
  #-------------------------------------
  # SITE HEIGHTS (mts)
  #-------------------------------------
  siteheights <- list()
  siteheights["ALF"] <- 250
  siteheights["RBA"] <- 153
  siteheights["SAN"] <- 152
  siteheights["TAB"] <- 188
  siteheights["TEF"] <- 188
  #-----------------------------------------------------------------------------
  # flux computations
  #-----------------------------------------------------------------------------
  # select the height
  profbud.df["heightcalc"] <- profbud.df$height
  sel <- profbud.df$maxmts != profbud.df$minmts
  sel[is.na(sel)] <- FALSE
  profbud.df$heightcalc[sel] <- profbud.df$meanmts[sel]
  # select the temperature and pressure
  profbud.df["tempcalc"] <- tempfunc(profbud.df$heightcalc)
  profbud.df["prescalc"] <- presfunc(profbud.df$heightcalc)
  # update key column names
  colnames(profbud.df)[c(40,41,42)] <- c("tempBrief", "humidityBrief", "pressureBrief")
  # temperature - fill in the gaps
  sel <- (profbud.df$tempBrief > -5) & (profbud.df$tempBrief < 35)                # valid temperature range
  sel[is.na(sel)] <- FALSE
  profbud.df$tempcalc[sel] <- profbud.df$tempBrief[sel]                           # keep the field observations that make sense
  # pressure - fill in the gaps
  sel <- (profbud.df$pressureBrief > 300) & (profbud.df$pressureBrief < 1050)     # valid pressure range
  sel[is.na(sel)] <- FALSE
  profbud.df$prescalc[sel] <- profbud.df$pressureBrief[sel]                       # keep the field observations that make sense
  
  
  
  #-----------------------------------------------------------------------------
  # chemistry
  #-----------------------------------------------------------------------------
  profbud.df["obs_bkg"] <- profbud.df$observed - profbud.df$background            # observed concetration minus backgroud
  profbud.df["airmol"]  <- (profbud.df$prescalc / 1013.25)/(0.0000820574587 * (profbud.df$tempcalc + 273))   # air molarity
  profbud.df["mmolgasmt3"]  <- profbud.df$obs_bkg * profbud.df$airmol
  # compute flux
  uprof <- as.character(unique(profbud.df$profile))                               # unique profiles
  flux.list <- lapply(uprof,
                      function(x, profbud.df, hfloor){
                        prof.df <- profbud.df[profbud.df$profile == x, ]
                        prof.df <- prof.df[!is.na(prof.df$observed),]
                        prof.df <- prof.df[order(prof.df$heightcalc, decreasing = TRUE),]
                        # time computation
                        atime <- rep(NA, times = nrow(prof.df) - 1) # date of the sample
                        btime <- rep(NA, times = nrow(prof.df) - 1) # date of the sample
                        for(i in 1:(length(prof.df$trajtime) - 1)){
                          atime[i] <- prof.df$trajtime[i]
                          btime[i] <- prof.df$trajtime[i + 1]
                        }
                        abtime <- as.data.frame(cbind(atime, btime), stringsAsFactors = FALSE)
                        abtime["abmean"] <- (abtime$atime + abtime$btime)/2
                        abtime[nrow(abtime) + 1, ] <- c(NA, NA, abtime$btime[nrow(abtime)])
                        # compute the area of each trapezoid
                        h <- rep(NA, times = nrow(prof.df) - 1)                   # height difference between two consequetive flask
                        a <- rep(NA, times = nrow(prof.df) - 1)                   # micromol gas/m3
                        b <- rep(NA, times = nrow(prof.df) - 1)                   # flask concentration
                        for(i in 1:(nrow(prof.df) - 1)){
                          h[i] <- prof.df$heightcalc[i] - prof.df$heightcalc[i + 1]
                          a[i] <- prof.df$mmolgasmt3[i]
                          b[i] <- prof.df$mmolgasmt3[i + 1]
                        }
                        h[nrow(prof.df)] <- prof.df$heightcalc[nrow(prof.df)] - hfloor
                        a[nrow(prof.df)] <- prof.df$mmolgasmt3[nrow(prof.df)]
                        b[nrow(prof.df)] <- floorconcentration(dif_obs_bkg = prof.df$obs_bkg[nrow(prof.df)],
                                                               hfloor  = hfloor,
                                                               temp = prof.df$tempcalc[nrow(prof.df)],
                                                               height = prof.df$heightcalc[nrow(prof.df)],
                                                               molair = prof.df$airmol[nrow(prof.df)])
                        # compute time of the floor
                        area.param <- as.data.frame(cbind(h, a, b))
                        prof.df["umolmt2"] <- area.param$h * (area.param$a + area.param$b)/2
                        prof.df["fluxo"] <- prof.df$umolmt2 / abtime$abmean
                        # filter columns
                        prof.df <- prof.df[, c("profile", "site", "year", "month", "day", "hour", "min.x", "flask",
                                               "eventnumber", "observed", "background", "lat", "lon", "height", "heightcalc",
                                               "tempcalc", "prescalc", "obs_bkg", "trajtime", "airmol", "mmolgasmt3", "fluxo")]
                        return(prof.df)
                      },
                      profbud.df = profbud.df,
                      hfloor = as.vector(unlist(siteheights[toupper(site)]))
  )
  #
  flux.df <- do.call("rbind", flux.list)
  flux.df["gas"] <- gas
  #
  budget <- data.frame(
    profile = unique(flux.df$profile),
    flux.mmolmt2day = rep(NA, times = length(unique(flux.df$profile))),
    stringsAsFactors = FALSE
  )
  if(sum(is.nan(flux.df$fluxo)) != length(flux.df$fluxo)){ # test if at least one flux is not missing
    budget <- aggregate(fluxo ~ profile, data = flux.df, FUN = sum, na.rm = TRUE)
  }
  names(budget) <- c("profile", "flux.umolmt2day")
  budget["gas"] <- gas
  budget["ugCmt2day"] <- budget$flux.umolmt2day * 16                            #CO2=12 /CO=28 /CH4=16 /N2O=44
  budget["gCmt2day"] <- budget$ugCmt2day / 1000000
  budget <- cbind(budget, getDataFromProfile(prof.name = as.character(budget$profile)))
  write.table(budget, paste("/home/lagee/Dropbox/DADOS LaGEE/ScriptsR/Fluxo/", toupper(gas), '/','fluxo_',site,'_',gas,'.txt', sep=''), col.names = TRUE, row.names = FALSE)
  write.table(budget, paste("/home/lagee/Documents/alber/test/Flux_results/", toupper(gas), '/','fluxo_',site,'_',gas,'.txt', sep=''), col.names = TRUE, row.names = FALSE)
  #write.table(budget, paste("/home/lagee/Dropbox/DADOS LaGEE/ScriptsR/Fluxo/CO - Carbono/",'fluxo_',site,'_',gas,'.txt', sep=''), col.names = TRUE, row.names = FALSE)
  #
  
}

