
ggstereo = function(wulff = F,
                    show.grid = TRUE,
                    grid.col = "grey50",
                    tl = 0.05) {
  
  intrad <- 10 * pi/180
  east <- pi/2
  west <- 3 * east
  TH <- (0:360) * pi/180
  nCircles <- pi/(intrad * 2)
  paths = vector('list',nCircles) %>% purrr::set_names(1:nCircles)
  p = vector('list',nCircles * 2+1) %>% purrr::set_names(1:(nCircles*2+1))
  
  if (show.grid) {
    
    nCircles <- pi/(intrad * 2)
    trd <- 0
    plg <- 0
    for (i in 1:nCircles) {
      coneAngle <- i * intrad
      paths[[i]] <- SmallCircle(trd, plg, coneAngle, wulff = wulff)
    }
    for (i in 0:(nCircles * 2)) {
      if (i > nCircles) {
        trd <- east
        plg <- (i - nCircles) * intrad
      }
      else {
        trd <- west
        plg <- i * intrad
      }
      if (plg == east) {
        plg <- plg * (1 - .Machine$double.min)
      }
      sd <- Pole(trd, plg, 0)
      p[[i+1]] <- GreatCircle(sd[1], sd[2], wulff = wulff)
    }
    
    sc = seq(0,90,10)
    gc = tidyr::expand_grid(strike = c(0,180), dip = seq(0,90,10))
    gc = gc[c(-1,-20),]
    
    # smallc = purrr::map(sc,~RFOC::addsmallcirc(0,90,.x,add = F) %>% 
    #                       tibble::as_tibble()) %>% 
    #   dplyr::bind_rows(.id = 'c')
    # greatc1 = purrr::map(sc,~RFOC::lowplane(0,.x,PLOT = F) %>% 
    #                        tibble::as_tibble()) %>% 
    #   dplyr::bind_rows(.id = 'c')
    # greatc2 = purrr::map(sc,~RFOC::lowplane(180,.x,PLOT = F) %>% 
    #                        tibble::as_tibble()) %>% 
    #   dplyr::bind_rows(.id = 'c')
    # 
    # smallc1 = purrr::map(1:nCircles,~purrr::pluck(paths,.x,1) %>%
    #                        tibble::as_tibble()) %>%
    #   dplyr::bind_rows(.id = 'c')
    # smallc2 = purrr::map(1:nCircles,~purrr::pluck(paths,.x,2) %>%
    #                        tibble::as_tibble()) %>%
    #   dplyr::bind_rows(.id = 'c')
    # greatc = purrr::map(1:(nCircles * 2+1),~purrr::pluck(p,.x) %>%
    #                       tibble::as_tibble() %>%
    #                       purrr::set_names(c('xp','yp'))) %>%
    #   dplyr::bind_rows(.id = 'c')
    
    
    smallc = purrr::map(sc,~SmallCircleD(0,0,.x,wulff = wulff) %>% 
                          bind_rows(.id = 'path')) %>% 
      dplyr::bind_rows(.id = 'c') %>% 
      dplyr::mutate(c = paste0(c,path))
    greatc = purrr::map2(gc$strike,gc$dip,~GreatCircleD(.x,.y,wulff = wulff)) %>% 
      dplyr::bind_rows(.id = 'c')

  }
  
  outer = tibble::tibble(x = cos(TH), y = sin(TH))
  
  etiquetas = tibble::tibble(x=c(0,1.1,0,-1.1),
                             y=c(1.075,0,-1.075,0),
                             lab1=c('0','90','180','270'),
                             lab2=c('N','E','S','W'))
  
  if (show.grid) {
    ggstereo = ggplot2::ggplot() + 
      ggplot2::geom_path(aes(x,y),outer) +
      ggplot2::geom_path(aes(xp,yp,group=c),smallc,linewidth = tl,col=grid.col) +
      ggplot2::geom_path(aes(xp,yp,group=c),greatc,linewidth = tl,col=grid.col) +
      ggplot2::geom_point(aes(0,0),shape=3,size=3) + 
      ggplot2::geom_text(aes(x,y,label=lab1),etiquetas,size=5) +
      ggplot2::coord_fixed() + 
      ggplot2::theme_void()
  } else {
    ggstereo = ggplot2::ggplot() + 
      ggplot2::geom_path(aes(x,y),outer) +
      ggplot2::geom_point(aes(0,0),shape=3,size=3) + 
      ggplot2::geom_text(aes(x,y,label=lab1),etiquetas,size=5) +
      ggplot2::coord_fixed() + 
      ggplot2::theme_void()
  }
  
  ggstereo
  
}





