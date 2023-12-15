stop("outdated")


# compute the influence area
library(raster)

# s = c('rba','alf','san','tab','tef')
s = c('rba','alf','san','tab','tef')[5]
tt = c('real','fake')

#site = 'tef'                                                                    # modifica o site
t = 'fake'
# t = "real"

lon.range <- c(-80, -20) # range(hs.df[, "lon"])
lat.range <- c(-60, 20) # range(hs.df[, "lat"])



Plot_stations = 'nao'

for(t in tt) {
  for (site in s) {
    
    filepath <- "/home/lagee/Documents/alber/test/hysplitsimulations/all_co2"
    
    HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min", 
                          "V8", "V9", "lat", "lon", "height", "pressure") 
    
    
    # build a list of sites and years
    hsfiles <- list.files(filepath)
    siteyear.mt <- do.call(rbind, unique(lapply(hsfiles, function(x){
      spl <- strsplit(x, split = "_")[[1]]
      return(c(spl[1], spl[2]))
    })))
    
    #' Count the NAs in the neighbour of each element of the grid
    #' @param egrid  A data.frame of xcol, ycol, freq, logfreq
    #' @param n      The number of neighbors to count on each direction
    countNeig <- function(egrid, n = 1){
      r <- df2raster(egrid[,c(1,2,4)], FALSE)
      #plot(r)
      nr <- raster::focal(r, w = matrix(rep(1, (n*2+1)^2), ncol = (n*2+1)), fun = function(x){sum(is.na(x))})
      val <- nr[]
      xy <- raster::xyFromCell(nr, 1:length(val))
      nx <- raster::colFromX(nr, xy[,1])
      ny <- raster::rowFromY(nr, xy[,2])
      cbind(i = ny, j = nx, nacount = val)
    }
    
    
    # TODO: Documemnt!!!
    #' Cast a data.frame to raster
    #' @param xyv.df
    #' @param invertY
    #' @param ncols
    #' @param nrows
    df2raster <- function(xyv.df, invertY, ncols = NULL, nrows = NULL){
      if(is.null(ncols)){
        ncols <- length(range(xyv.df[,1])[1]:range(xyv.df[,1])[2])
      }
      if(is.null(nrows)){
        nrows <- length(range(xyv.df[,2])[1]:range(xyv.df[,2])[2])
      }
      r <- raster::raster(ncols = ncols, nrows = nrows)
      r_vec <- r[]
      for(i in 1:nrow(xyv.df)){
        ycol_inv <- xyv.df[i, 2]
        if(invertY == TRUE){
          ycol_inv <- 1 + max(xyv.df[,2]) - xyv.df[i, 2]
        }
        r_vec[raster::cellFromRowCol(r, ycol_inv, xyv.df[i,1])] <- xyv.df[i,3]
      }
      r[] <- r_vec
      return(r)
    }
    
    
    if (t == 'real') {
      if (site == 'tef') {
        y = c(2013, 2014, 2017)
      } else{
        if (site == 'tab') {
          y = c(2010, 2011, 2012)
        } else{ if(site == 'san'){
          y = c(2010, 2011, 2012, 2013, 2014, 2017)}
          else{
            y = c(2010, 2011, 2012, 2013, 2014, 2016, 2017)
          }
        }
      }
    }
    
    if (t == 'fake') {
      if (site == 'tef') {
        y = c(2013, 2014, 2015, 2016)
      } else{
        if (site == 'tab') {
          y = c(2012)
        }
        else{
          if (site == 'san') {
            y = c(2015, 2016)
          } else{
            y = 2015
          }
        }
      }
    }
    
    
    for (year in y) {
      siteyear <- paste0(site, "_", year)
      
      # Arquivo realizado por Lucas e Luciano no Mac programa ... com as coordenadas das estacoes
      
      if (Plot_stations == 'sim') {
        www = read.table(
          paste0(
            "/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_3500/by_year/stn_byia/",
            siteyear,
            "_PREC_stations.txt"
          ),
          stringsAsFactors = FALSE
        )
        www$V2 = as.numeric(as.character(www$V2))
        www$V3 = as.numeric(as.character(www$V3))
      } else{
      }
      
      #siteyear <- siteyear.mt[1, ]
      #compInfArea <- function(siteyear, filepath){
      
      # filter files by site and year
      #  files <- list.files(filepath, pattern = paste0(siteyear[1], "_", siteyear[2]), full.names = TRUE)
      
      if (t == 'fake') {
        filepath = paste0('/home/lagee/Dropbox/DADOS LaGEE/ScriptsR/trajetorias simuladas/',toupper(site), '/')
      } else{
        files = list.files(filepath, pattern = siteyear, full.names = TRUE)
      }
      
      # read hyspit files into a dataframe
      hs.df.n <-
        cqmaTools::files2df(
          file.vec = files,
          header = FALSE,
          skip = 7,
          cnames = HYSPLIT.COLNAMES
        )
      hs.df.n <- do.call(rbind, hs.df.n)
      
      hs.df.n = subset(hs.df.n, hs.df.n$height <= 3500)                                   # Modifica a Altitude
      
      
      
      
      
      
      tri = c(1, 2, 3)
      seq = c(0, 3, 6, 9)
      
      for (hh in seq) {
        tri.list = list()
        for (ii in tri) {
          dftp = subset(hs.df.n, hs.df.n$month == ii + hh)
          tri.list[[ii]] = dftp
        }
        
        hs.df = do.call(rbind, tri.list)
        
        # create grid (2D)
        # - WGS84
        # - Origin site coordinates
        # - xy resolution 1 degree
        ll.res <- 2                                               # grid resolution in degrees
        # grid.origin <- unlist(hs.df[1, c("lon", "lat")])
        
        if (site == 'rba') {
          grid.origin <- c(-64.74, -9.02)
        }
        if (site == 'alf') {
          grid.origin <- c(-55.78, -8.822)
        }
        if (site == 'san') {
          grid.origin <- c(-54.95, -2.85)
        }
        if (site == 'tab') {
          grid.origin <- c(-69.9, -5.74)
        }
        if (site == 'tef') {
          grid.origin <- c(-66.5, -3.68)
        }
        
        
        lon.grid <-
          sort(c(
            seq(
              from = grid.origin[1],
              to = lon.range[2],
              by = ll.res
            ),
            seq(
              from = grid.origin[1],
              to = lon.range[1],
              by = -ll.res
            )[-1]
          ))
        lat.grid <-
          sort(c(
            seq(
              from = grid.origin[2],
              to = lat.range[2],
              by = ll.res
            ),
            seq(
              from = grid.origin[2],
              to = lat.range[1],
              by = -ll.res
            )[-1]
          ))
        #ll.grid <- expand.grid(lon.grid, lat.grid)
        #colnames(ll.grid) <- c("lon", "lat")
        
        # add a column with grid ID - add the grid ID to each vertex on each trajectory
        hs.df["gridX"] <- findInterval(hs.df$lon, lon.grid)
        hs.df["gridY"] <- findInterval(hs.df$lat, lat.grid)
        hs.df["gridXY"] <- paste0(hs.df$gridX, "-", hs.df$gridY)
        
        # sum the grid ID
        trajden <-
          as.data.frame(table(hs.df$gridXY), stringsAsFactors = FALSE)
        colnames(trajden) <- c("crid", "freq")
        trajden <-
          cbind(trajden, do.call(rbind, strsplit(trajden$crid, split = "-")), stringsAsFactors = FALSE)
        colnames(trajden) <- c("crid", "freq", "xcol", "ycol")
        trajden$xcol <- as.numeric(trajden$xcol)
        trajden$ycol <- as.numeric(trajden$ycol)
        trajden <- trajden[, c("xcol", "ycol", "freq")]
        # remove extreme values
        trajden <- trajden[trajden$xcol != 0, ]
        trajden <- trajden[trajden$ycol != 0, ]
        #plot(df2raster(trajden, TRUE))
        
        
        # build grid
        egrid <- expand.grid(1:(max(trajden$xcol) - 1), 1:(max(trajden$ycol) - 1))
        colnames(egrid) <- c("xcol", "ycol")
        # joins grids
        egrid <- merge(egrid, trajden, by = c("xcol", "ycol"), all.x = TRUE)
        #plot(df2raster(egrid, TRUE))
        
        
        #---- filter ----
        #egrid[is.na(egrid$freq), "freq"] <- 0
        #egrid[egrid$freq < 2, "freq"] <- NA
        # - - - - - -
        egrid$ycol <- abs(egrid$ycol - (max(egrid$ycol) + 1)) # invert the y-axis
        egrid <- egrid[order(egrid$ycol, egrid$xcol), ] # order by lon & lat
        
        
        
        
        ####################################################
        #egrid$logfreq <- log(egrid$freq)
        egrid$logfreq <- egrid$freq
        ####################################################
        
        
        
        
        # mean(egrid$logfreq, na.rm = TRUE)
        # max(egrid$logfreq, na.rm = TRUE)
        # max(egrid$freq, na.rm = TRUE)
        # mean(egrid$freq, na.rm = TRUE)
        # hist(egrid$freq, breaks = 30)
        
        # Limite shapefile
        
        # # Chama o shapefile do Bioma com o nome de Utah
        # ####################################################
        # utah = readOGR(dsn = "/home/lagee/Documents/shapefiles/south_america", layer = "South_America")
        # utah@data$id = rownames(utah@data)
        # utah.points = fortify(utah, region="id")
        # utah.df = join(utah.points, utah@data, by="id")
        # utah@data$id = rownames(utah@data)
        # utah.points = fortify(utah, region="id")
        # utah.df = join(utah.points, utah@data, by="id")
        #
        # xxczxc = utah.df[!duplicated(utah.df[c(1,2)]),]
        #
        # plot(utah.df$long, utah.df$lat)
        #
        #
        #  m = getMap(resolution="low")
        #
        # #convert events into an sp object -- first longitude, then latitude:
        #   pts = SpatialPoints(amostral_table[c(1,2)])
        #
        # #assign CRS:
        #
        #   proj4string(pts) = proj4string(m)
        #
        # #plot points inside any of the polygons:
        #
        #   points(pts[m,], pch = 3)
        #
        # #select points inside Italy:
        #
        # it = m[m$ISO_A2 == "BR",]
        #
        # points(pts[it,], col = 'blue')
        
        
        
        ####################################################
        #Calcula o filtro
        ####################################################
        
        # amostral = length(which(!is.na(egrid$logfreq)))
        # amostral_table = egrid[order(egrid$logfreq, decreasing = TRUE),]
        # line = round(amostral * 0.3)
        #  cut = amostral_table[line,4]
        
        
        # ALF ->  2010:2014,2016 filtro = 4.7 / 2015 filtro = 4
        # RBA -> 2010:2016, filtro = 4.7
        # SAN -> 2010:2014, filtro = 4.7 / 2015, filtro = 4
        # TAB -> 2010, filtro = 4.7 / 2011, filtro 4 / 2012, filtro = 3.5
        # TEF -> 2013:2014, filtro = 4.7
        
        # filter
        #egrid$logfreq[egrid$logfreq <  (mean(egrid$logfreq, na.rm = TRUE)+1)] <- NA
        #egrid$logfreq[egrid$logfreq <  cut] <- NA                                     ############################# keep gibbest  frequencies of trajectory vertex on each cell
        
        #nacount <- as.data.frame(countNeig(egrid, n = 1))
        #nacount[,3] <- 0
        # colnames(nacount) <- c("ycol", "xcol", "naneigh")
        
        # egrid <- merge(egrid, nacount)
        # egrid$logfreq[egrid$naneigh > 4 ] <- NA                                       ############################# Remove cells with more than X neigbohrs with NAs
        #plot(df2raster(egrid[, c(1,2,3)], FALSE), main = "Freq")
        #plot(df2raster(egrid[, c(1,2,4)], FALSE), main = "LogFreq")
        #plot(df2raster(egrid[, c(1,2,5)], FALSE), main = "NAneig")
        
        
        # plot the grid
        #egrid <- egrid[with(egrid, order(ycol, xcol)), ]
        # raster.dat <- egrid$logfreq # egrid$freq # 1:length(r1[])
        
        
        
        
        
        #---- here ----
        
        r_logfreq <- df2raster(egrid[, c(1, 2, 3)], FALSE)    # 4 para log
        r1 <- raster(
          nrows = length(1:(max(trajden$ycol) - 1)),
          ncols = length(1:(max(trajden$xcol) - 1)),
          xmn = min(lon.grid),
          xmx = max(lon.grid),
          ymn = min(lat.grid),
          ymx = min(lat.grid) + (2 * length(1:(max(trajden$ycol) - 1)))
        )
        if (all(dim(r1) == dim(r_logfreq)) == FALSE) {
          warning("Raster dimensions do not match!")
          print("WARNING: Raster dimensions do not match!")
        }
        r1[] <- r_logfreq[]
        ##############################################
        # writeRaster(r1,paste("/home/lagee/Dropbox/DADOS LaGEE/ScriptsR/data_for_ARCGIS/",site,"_",year,".tif",sep=""),  options=c('TFW=YES'))
        
        
        #control
        plot(r1, asp = 1)
        points(grid.origin[1], grid.origin[2], cex = 5)
        points(hs.df$lon, hs.df$lat, cex = .01)
        ##############################################
        
        # build a convex hull
        rmat <- rasterToPoints(r1)
        chrows <- grDevices::chull(x = rmat[, "x"], y = rmat[, "y"])
        chrows <- c(chrows, chrows[1])
        plot(r1, main = paste(siteyear, collapse = " "))
        nbreaks <- 10
        #hist(log(egrid$freq), breaks = nbreaks)
        d <- r1
        intBreaks <- seq(100, 800, length.out = nbreaks)
        intBreaks = signif(intBreaks, digits = 2)
        
        xxx = data.frame(rasterToPoints(d))
        xxx$x <- round(xxx$x, digits = 2)
        xxx$y <- round(xxx$y, digits = 2)
        write.table(xxx, (
          paste0(
            "/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_3500/trimestral/",
            siteyear,
            '_',
            hs.df[1, 4],
            '_stations.txt'
          )
        ))
        
        #pdf(paste0("/home/lagee/Dropbox/DADOS LaGEE/ScriptsR/Influence_area/", siteyear, 'SEM_CORTE.pdf'))
        
        if (Plot_stations == 'sim') {
          pdf(
            paste0(
              '/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_3500/trimestral/',
              siteyear,
              '_',
              hs.df[1, 4],
              '_stations.pdf'
            )
          )
        } else{
          pdf(
            paste0(
              "/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_3500/trimestral/",
              siteyear,
              '_',
              hs.df[1, 4],
              '_stations.pdf'
            )
          )
        }
        
        par(mar = c(5, 6, 4, 2) + 0.1, mgp = c(3, 1, 0))
        
        # plot( d, xlab =expression(paste("Longitude ","(",degree,")")),
        #      ylab =expression(paste("Latitude ","(",degree,")")),
        #      col = rev(heat.colors(nbreaks)),
        #      breaks = intBreaks, main = paste(toupper(siteyear), collapse = " "))
        plot(
          d,
          xlab = expression(paste("Longitude ", "(", degree, ")")),
          ylab = expression(paste("Latitude ", "(", degree, ")")),
          xlim = lon.range,
          ylim = lat.range,
          col = rev(heat.colors(nbreaks)),
          # breaks = intBreaks,
          #axes = FALSE
          xaxs="i", yaxs="i",
          asp = 1
        )
        
        
        if (Plot_stations == 'sim') {
          points(www$V3,
                 www$V2,
                 add = TRUE,
                 pch = 8,
                 cex = 0.7)
        }
        
        # abline(v=(seq(-80,-30,1)), col="black", lty="dotted")
        # abline(h=(seq(-40,10,1)), col="black", lty="dotted")
axis(side = 1, at = seq(-80, -30, by = 5), las = 1)
        # axis(side = 3, at = seq(-80,-30, by = 5), las = 1)
axis(side = 2, at = seq(-40, 10, by = 5), las = 2)
        #axis(side = 4, at = seq(-40,10, by = 5), las = 2)
        
        
        
        tri.shr <- c("1st", "2nd", "3rd", "4th")
        
        # text(
        #   x = -40,
        #   y = -35,
        #   labels = paste(toupper(site), year, tri.shr[match(hh,seq)], sep = " "),
        #   cex = 1.5
        # )
        
        if (site == 'rba') {
          points(-64.74, -9.02, pch = 13, cex = 2)
        }
        if (site == 'alf') {
          points(-55.78, -8.82, pch = 13, cex = 2)
        }
        if (site == 'san') {
          points(-54.95, -2.85, pch = 13, cex = 2)
        }
        if (site == 'tab') {
          points(-69.9, -5.74, pch = 13, cex = 2)
        }
        if (site == 'tef') {
          points(-66.5, -3.68, pch = 13, cex = 2)
        }
        
        
        #lines(rmat[chrows,], col = "red")
        maps::map("world",
                  xlim = lon.range,
                  ylim = lat.range,
                  add = TRUE)
        
        dev.off()
        
        
      }
    }
  }
}





#   return(rmat[chrows,])
# }
# minimim value of a cell 0
# maximum value of a cell 12 flask * 14 days * 24 hours * 4 sample per hour = 16128



# analyze one site & one year
#siteyear <- siteyear.mt[1, ]

# hotspots <- list()
# for(i in 1:nrow(siteyear.mt)){
#   siteyear <- siteyear.mt[i, ]
#   hotspots[[paste0(siteyear, collapse = "-")]] <- compInfArea(siteyear, filepath)
# }





