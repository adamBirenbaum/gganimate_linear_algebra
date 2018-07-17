
library(shiny)
library(gganimate)
library(magick)
library(paletteer)
library(dplyr)

ui <- basicPage(
  textInput("a","",value = "3"),
  textInput("b","",value = "1.5"),
  actionButton("enter","enter"),
  imageOutput("plot1"))

server <- function(input,output){
  custom_pallette <- paletteer_d("LaCroixColoR","PassionFruit")

  
  output$plot1 <- renderImage({list(src = "~/gganimate_linear_algebra/original.gif",contentType = 'image/gif')},deleteFile = F)
  observeEvent(input$enter,{
    
    
      
      a <- matrix(c(as.numeric(input$a),0),nrow = 2)
      b <- matrix(c(0,as.numeric(input$b)),nrow = 2)
      df <- add_vectors(a,b)
      text_df <- get_text_df(df)

      df2 <- transform_grid(4)
     #df3 <- data.frame(intercept = -5:5,state = 1)
    
      
      
 #      g <- ggplot(df, aes(x = x, y = y, xend = xend, yend = yend,color = factor(color))) + 
 # geom_segment(arrow = arrow(type = "closed"),size = 3)+ geom_text(data = text_df,aes(x = xx, y = yy, label = paste0("hat(",id,")")),color = "white",parse = T,size = 14) +geom_abline(mapping = aes(slope = slope, intercept = intercept),data = df2,size = 3, color = "white")+
 #            scale_color_manual(breaks = 1:3,values = c("#FC6882","#A6E000")) + add_custom_theme() + 
 #                 transition_states(state,transition_length = 10,state_length = 1,wrap = F) +
 #               ease_aes('linear') +xlim(-2,2) + ylim(-2,2)#+ adjust_grid_limits(df,make_square = T)
 #      
      g <- ggplot(df, aes(x = x, y = y, xend = xend, yend = yend,color = factor(color))) + original_grid(df,by_ones = T,make_square = T) +geom_abline(mapping = aes(slope = slope, intercept = intercept),data = df2,size = 3, color = "white")+
                    scale_color_manual(breaks = 1:3,values = c("#FC6882","#A6E000")) + add_custom_theme() + 
        geom_segment(arrow = arrow(type = "closed"),size = 3)+ geom_text(data = text_df,aes(x = xx, y = yy, label = paste0("hat(",id,")")),color = "white",parse = T,size = 14) +
        scale_color_manual(breaks = 1:3,values = c("#FC6882","#A6E000")) + add_custom_theme() +
        transition_states(state,transition_length = 10,state_length = 1,wrap = F) +
        ease_aes('linear') + adjust_grid_limits(df,make_square = T)

 
      
      gg <- animate(g, nframes = 60, fps = 10)
    


      image_write(gg,"~/gganimate_linear_algebra/testing.gif")
      output$plot1 <- renderImage(list(src = "~/gganimate_linear_algebra/testing.gif",contentType = 'image/gif'))

    })
    
  

  # tail of v1 goes to head of v2
  a <- matrix(c(1,1),nrow = 2)
  b <- matrix(c(-1,-2),nrow =2)
  
  add_vectors <- function(v1,v2){
    
    v3 <- v1 + v2
    v3_startx <- seq(from = 0, to = v3[1,1], length.out = 20)
    v3_starty <- seq(from = 0, to = v3[2,1], length.out = 20)

    

    # df <- data.frame(x = c(0,0,v2[1,1],v2[1,1],v2[1,1],0,0,0,0,0,0,0),
    #                  xend = c(1,v1[1,1],v1[1,1],v1[1,1],v1[1,1],0,v2[1,1],v2[1,1],v2[1,1],v2[1,1],v3_startx[2],v3[1,1]),
    #                  y = c(0,0,v2[2,1],v2[2,1],v2[2,1],0,0,0,0,0,0,0),
    #                  yend = c(0,v1[2,1],v2[2,1],v2[2,1],v2[2,1],1,v2[2,1],v2[2,1],v2[2,1],v2[2,1],v3_starty[2],v3[2,1]),
    #                  id = c(1,1,1,1,1,2,2,2,2,2,3,3),
    #                  state = c(1:5,1:5,4:5)
    #                  )
    df <- data.frame(x = c(0,0,v2[1,1],v2[1,1],0,0,0,0,0,0),
                     xend = c(1,v1[1,1],v3[1,1],v3[1,1],0,v2[1,1],v2[1,1],v2[1,1],v3_startx[2],v3[1,1]),
                     y = c(0,0,v2[2,1],v2[2,1],0,0,0,0,0,0),
                     yend = c(0,v1[2,1],v3[2,1],v3[2,1],1,v2[2,1],v2[2,1],v2[2,1],v3_starty[2],v3[2,1]),
                     color = c(1,1,1,1,1,1,1,1,3,3),
                     id = c(1,1,1,1,2,2,2,2,3,3),
                     state = c(1:4,1:4,3:4)
    )
    

    df
    
  }
  
  get_text_df <- function(df){
    df <- df %>% group_by(id,state) %>% mutate(xx = mean(c(x,xend)),yy = mean(c(y,yend)))
    
    df$id <- sapply(df$id, function(x) switch(x,"i","j","v","r","s"), USE.NAMES = F)
    df
    
  }
  
  original_grid <- function(df,n_grid_lines = 5, by_ones = F,make_square = F){
    xmax <- max(df$x,df$xend)
    xmin <- min(df$x,df$xend)
    ymax <- max(df$y,df$yend)
    ymin <- min(df$y,df$yend)
    
    
    if (by_ones) make_seq <- function(a,b) seq(from = a,to = b, by = 1)
    else make_seq <- function(a,b) seq(from = a,to = b, length.out = n_grid_lines)
    
    if (make_square){
      global_min <- min(xmin,ymin)
      global_max <- max(xmax,ymax)
      return(
      list(geom_vline(xintercept = make_seq(global_min,global_max),color = "white"),
           geom_hline(yintercept = make_seq(global_min,global_max),color = "white"),
           geom_vline(xintercept = 0,size = 3,color ="white"),
           geom_hline(yintercept = 0,size = 3,color = "white"))
      )
      
    }else{
      return(
        list(geom_vline(xintercept = make_seq(xmin,xmax),color = "white"),
             geom_hline(yintercept = make_seq(ymin,ymax),color = "white"),
             geom_vline(xintercept = 0,size = 3,color ="white"),
             geom_hline(yintercept = 0,size = 3,color = "white"))
        
      )
    }
  }
  


  # d <- expand.grid(1:5,-10:10,5)
  # names(d) <- c("slope","shift","global_limit")
  # 
  # get_vertical_line <- function(slope,shift,global_limit){
  # 
  #   ylim1 <- shift + global_limit / slope
  #   ylim2 <- shift - global_limit / slope
  #   data.frame(x = seq(from  = ylim1, to = ylim2, length.out = 20)) %>% mutate(y = slope *(x - shift))
  # }
  # 
  # ggplot(get_vertical_line(1000,-1,5),aes(x = x, y =y )) + geom_line() + coord_cartesian(xlim = c(-5,5),ylim = c(-5,5), expand = F)
  # 
  # 
  # 
  # all_lines <- mapply(get_vertical_line,slope =d$slope,shift = d$shift, global_limit = d$global_limit,SIMPLIFY = F)
  # all_lines <- do.call(rbind,all_lines)
  # all_lines$state <- rep(rep(1:5,each = 20),times = 21)
  # all_lines$group <- rep(1:21,each = 20)
  # 
  # g <- ggplot() +
  #   geom_line(data = all_lines, aes(x = x, y = y,group = group)) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F) +
  #   transition_states(state,transition_length = 1000,state_length = 1,wrap = F) +
  #   ease_aes('linear')
  # animate(g,nframes = 400, fps = 100)
  # 
  get_vertical_line <- function(slope,shift,global_limit,state){
    nsize <- 100
    if (slope == 0){
      ylim1 <- -global_limit
      ylim2 <- global_limit
      
      df <-  data.frame(x = seq(from  = ylim1, to = ylim2, length.out = nsize)) %>% mutate(y = shift,
                                                                                           state = state)
    }else if (is.infinite(slope)){
      df <-  data.frame(x = rep(shift,nsize), y = seq(from = -global_limit, to = global_limit, length.out = nsize),
                        state = state)
      
    }else{
      ylim1 <- shift + global_limit / slope
      ylim2 <- shift - global_limit / slope
      df <- data.frame(x = seq(from  = ylim1, to = ylim2, length.out = nsize)) %>% mutate(y = slope *(x - shift),
                                                                                          state = state)
    }
    
    df
    
  }
  
  transform_grid <- function(nstates){
    A = matrix(c(1,1,-2,1),nrow = 2)
    
    

      slope1 <- A[2,1] / A[1,1]
      slope2 <- A[2,2] / A[1,2]
      slope1_seq <- seq(from = 0, to = slope1, length.out = nstates)
      #slope2_seq <- seq(from = 10, to = slope2, length.out = nstates)
      slope2_seq <- (seq(from = (10)^(1/9), to = sign(slope2)*(abs(slope2))^(1/9), length.out = nstates))^(9)
      
      int_seq <- seq(from = -5, to = 5, length.out = 5)
      xdf <- expand.grid(slope1_seq,int_seq)
      xdf$state <- 1:4
      ydf <- expand.grid(slope2_seq,int_seq)
      ydf$state <- 1:4
      
      df2 <- rbind(xdf,ydf)
      names(df2) <- c("slope","intercept","state")  
      # data.frame(slope = rep(c(0,slope1,Inf,slope2),each = 5),
        #                 intercept = rep(int_seq,times = 4),
        #                 state = rep(c(1,4),times = 10)),
             

      df2
  }
  
  adjust_grid_limits <- function(df,scale = .1,make_square = F){
    xmax <- max(df$x,df$xend)
    xmin <- min(df$x,df$xend)
    ymax <- max(df$y,df$yend)
    ymin <- min(df$y,df$yend)
    
    if (make_square){
      global_min <- min(xmin,ymin)
      global_max <- max(xmax,ymax)
      return(list(xlim(global_min - abs(global_min)*scale,global_max + abs(global_max)*scale),ylim(global_min - abs(global_min)*scale,global_max + abs(global_max)*scale)))
    } else return(list(xlim(xmin - xmin*scale,xmax + xmax*scale),ylim(ymin - ymin*scale,ymax + ymax*scale)))
    
    
    
  }
  
  add_custom_theme <- function(){
    list(theme(panel.background = element_rect(fill = "#172869"),plot.background = element_rect(fill = "#172869"), axis.text = element_text(colour = "white"),
          panel.grid = element_blank()),xlab(""),ylab(""),guides(color = F))
  }
  
  A <- matrix(c(3,2,2,1),nrow = 2)
  

  make_grid_df <- function(A){
    integer_factor <- -50:50
    nn <- length(integer_factor)
    
    slope_v1 <- A[2,1] / A[1,1]
    slope_v2 <- A[2,2] / A[1,2]
    
    v1_df <- do.call(rbind,lapply(integer_factor * A[1,1],FUN = get_vertical_line,slope = slope_v2,global_limit = 5,state = 2))
    v1_df$group <- rep(1:nn,each = 100)
    v2_df <- do.call(rbind,lapply(integer_factor * A[2,2],FUN = get_vertical_line,slope = slope_v1,global_limit = 5,state = 2))
    v2_df$group <- rep(c(nn+1):(nn*2),each = 100)
    
    A0 <- matrix(c(1,0,0,1),nrow = 2)
    slope_v1 <- A0[2,1] / A0[1,1]
    slope_v2 <- A0[2,2] / A0[1,2]
    v01_df <-  do.call(rbind,lapply(integer_factor * A0[1,1],FUN = get_vertical_line,slope = slope_v2,global_limit = 5,state = 1))
    v01_df$group <- rep(1:nn,each = 100)
    v02_df <-  do.call(rbind,lapply(integer_factor * A0[2,2],FUN = get_vertical_line,slope = slope_v1,global_limit = 5,state = 1))
    v02_df$group <- rep(c(nn+1):(nn*2),each = 100)
    
    
    dd <- rbind(v01_df,v02_df,v1_df,v2_df)
    
    g <- ggplot(dd,aes(x = x,y = y, group = group + 3)) + geom_line(color = "#FC6882",size = 2) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F)+
      add_custom_theme()+theme(axis.text = element_blank(),
                               plot.margin = margin(0,0,0,0,"null"))+
      transition_states(state,transition_length = 100,state_length = 1,wrap = F)+
      ease_aes("linear")
    
    
    animate(g,nframes = 100, fps = 25)
    
  }

}

shinyApp(ui = ui, server = server)