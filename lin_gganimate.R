
library(shiny)
library(gganimate)
library(magick)
library(paletteer)
library(dplyr)


### RULES

## states must be of equal length
## stuff doesn't work well when the data is given in the first ggplot(), instead keep it blank and at data, layer by layer


ui <- basicPage(
  textInput("a","",value = "3"),
  textInput("b","",value = "1.5"),
  actionButton("enter","enter"),
  imageOutput("plot1"))

server <- function(input,output){
  custom_pallette <- paletteer_d("LaCroixColoR","PassionFruit")

  
  output$plot1 <- renderImage({list(src = "~/gganimate_linear_algebra/original.gif",contentType = 'image/gif')},deleteFile = F)
  observeEvent(input$enter,{
    
    #A <- matrix(c(3,2,2,1),nrow = 2)
      a <- matrix(c(3,2),nrow = 2)
      b <- matrix(c(2,1),nrow = 2)
      a <- matrix(c(as.numeric(input$a),0),nrow = 2)
      b <- matrix(c(0,as.numeric(input$b)),nrow = 2)
      
      df <- add_vectors(a,b)
      text_df <- get_text_df(df)

      df2 <- transform_grid(4)
     #df3 <- data.frame(intercept = -5:5,state = 1)
    
      
      v1 <- make_vector(1,0)
      v2 <- make_vector(0,1)
      
      
      dd <- custom_vector_transformation(v0 = v1,make_vector(3,0))
      dd2 <- custom_vector_transformation(v0 = v2, make_vector(0,2))
      dd2$group <- 2
      dd <- rbind(dd,dd2)
      dd <- pause_animation(dd,2,last_state = T)
      delete_states(dd,group = 2,state = 3:4)
      
      
      dd3 <- add_vectors(make_vector(0,2),make_vector(3,0))
      dd3$state <- dd3$state + 1
      dd <- rbind(dd,dd3)
      
      g <- ggplot() + original_grid(dd,by_ones = T)+ geom_segment(data = dd, aes(x = x, y = y, xend = xend, yend = yend),arrow = arrow(type = "closed"),size = 3,color = "#A6E000")  +
        transition_states(state,transition_length = 10,state_length = 1,wrap = F) +
        ease_aes('linear') +  add_custom_theme() 
      
     animate(g, nframes = 100, fps = 25)
      
      v3 <- make_vector(0,1)
      
      
      
      dadd <- add_vectors(v1,v3)
    
      g <- ggplot() + original_grid(dadd)+ geom_segment(data = dadd, aes(x = x, y = y, xend = xend, yend = yend),arrow = arrow(type = "closed"),size = 3,color = "#A6E000")  +
        transition_states(state,transition_length = 10,state_length = 1,wrap = F) +
        ease_aes('linear') + adjust_grid_limits(dadd,make_square = T) + add_custom_theme() 
         
      gg <- animate(g, nframes = 100, fps = 25)
      
      
      
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
  
  custom_vector_transformation <- function(v0,v,num_states = 2){
    element1 <- seq(from = v0[1,1], to = v[1,1], length.out = num_states)
    element2 <- seq(from = v0[2,1], to = v[2,1], length.out = num_states)
    
    df <- data.frame(x = integer(0), xend = integer(0),y = integer(0),yend = integer(0),group = integer(0),state = integer(0))
    for (i in 1:num_states){
      
      new_v <- make_vector(element1[i],element2[i])
      
      df <- rbind(df,data.frame(x = 0, xend = new_v[1,1], y = 0, yend = new_v[2,1], group = 1, state = i)    )
      
      
      
    }
    df
    
  }
  
  add_vectors <- function(v1,v2,num_states = 2){
    
    v3 <- v1 + v2
    # v3_startx <- seq(from = 0, to = v3[1,1], length.out = num_states)
    # v3_starty <- seq(from = 0, to = v3[2,1], length.out = num_states)
    # 
    # 
    # 
    # df <- data.frame(x = c(0,v2[1,1],v2[1,1],0,0,0,0,0,0),
    #                  xend = c(1,v3[1,1],v3[1,1],0,v2[1,1],v2[1,1],v2[1,1],v3_startx[2],v3[1,1]),
    #                  y = c(0,v2[2,1],v2[2,1],0,0,0,0,0,0),
    #                  yend = c(0,v3[2,1],v3[2,1],1,v2[2,1],v2[2,1],v2[2,1],v3_starty[2],v3[2,1]),
    #                  color = c(1,1,1,1,1,1,1,3,3),
    #                  id = c(1,1,1,2,2,2,2,3,3),
    #                  state = c(1:3,1:4,3:4)
    # )
    # 
    
    list_of_vectors <- list(v1,v2)
    list_of_transformed_vectors <- list(v1,v3)
    
    
    df <- data.frame(x = integer(0), xend = integer(0),y = integer(0),yend = integer(0),group = integer(0),state = integer(0))
    for (i in 1:length(list_of_vectors)){
      element1 <- seq(from = list_of_vectors[[i]][1,1], to = list_of_transformed_vectors[[i]][1,1], length.out = num_states)
      element2 <- seq(from = list_of_vectors[[i]][2,1], to = list_of_transformed_vectors[[i]][2,1], length.out = num_states)
      
      if (i == 2){
        x <- seq(from = 0, to = v1[1,1], length.out = num_states)
        y <- seq(from = 0, to = v1[2,1], length.out = num_states)
      }else{
        x <- rep(0,num_states)
        y <-rep(0,num_states)
      }
      
      for (j in 1:num_states){
        
        new_v <- matrix(c(element1[j],element2[j]),nrow = 2)

        df <- rbind(df,data.frame(x = x[j], xend = new_v[1,1], y = y[j], yend = new_v[2,1], group = i, state = j)    )
        
        
        
      }
      
      
    }
    
    sum_v <- custom_vector_transformation(v3/10,v3,num_states = num_states)
    sum_v$state <- sum_v$state + num_states - 1
    df <- pause_animation(df,1,last_state = T)
    df <- rbind(df,sum_v)
    # df <- data.frame(x = c(0,0,v2[1,1],v2[1,1],v2[1,1],0,0,0,0,0,0,0),
    #                  xend = c(1,v1[1,1],v1[1,1],v1[1,1],v1[1,1],0,v2[1,1],v2[1,1],v2[1,1],v2[1,1],v3_startx[2],v3[1,1]),
    #                  y = c(0,0,v2[2,1],v2[2,1],v2[2,1],0,0,0,0,0,0,0),
    #                  yend = c(0,v1[2,1],v2[2,1],v2[2,1],v2[2,1],1,v2[2,1],v2[2,1],v2[2,1],v2[2,1],v3_starty[2],v3[2,1]),
    #                  id = c(1,1,1,1,1,2,2,2,2,2,3,3),
    #                  state = c(1:5,1:5,4:5)
    #                  )
    
    #below is most recent try
    # df <- data.frame(x = c(0,0,v2[1,1],v2[1,1],0,0,0,0,0,0),
    #                  xend = c(1,v1[1,1],v3[1,1],v3[1,1],0,v2[1,1],v2[1,1],v2[1,1],v3_startx[2],v3[1,1]),
    #                  y = c(0,0,v2[2,1],v2[2,1],0,0,0,0,0,0),
    #                  yend = c(0,v1[2,1],v3[2,1],v3[2,1],1,v2[2,1],v2[2,1],v2[2,1],v3_starty[2],v3[2,1]),
    #                  color = c(1,1,1,1,1,1,1,1,3,3),
    #                  id = c(1,1,1,1,2,2,2,2,3,3),
    #                  state = c(1:4,1:4,3:4)
    #)
    

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
      if (ymin == 0 && ymax == 0){
        ymin <- -1
        ymax <- 1
      }else if (xmin == 0 && xmax ==0){
        xmin <- -1
        xmax <- 1
      }
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
  # get_grid_line <- function(slope,shift,global_limit){
  # 
  #   ylim1 <- shift + global_limit / slope
  #   ylim2 <- shift - global_limit / slope
  #   data.frame(x = seq(from  = ylim1, to = ylim2, length.out = 20)) %>% mutate(y = slope *(x - shift))
  # }
  # 
  # ggplot(get_grid_line(1000,-1,5),aes(x = x, y =y )) + geom_line() + coord_cartesian(xlim = c(-5,5),ylim = c(-5,5), expand = F)
  # 
  # 
  # 
  # all_lines <- mapply(get_grid_line,slope =d$slope,shift = d$shift, global_limit = d$global_limit,SIMPLIFY = F)
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
  get_grid_line <- function(slope,shift,global_limit,state,integer_factor){
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
    
    if (integer_factor == 0) df$color <- 1
    else df$color <- 0
    
    df
    
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
    list(theme(axis.text = element_blank(),panel.background = element_rect(fill = "#172869"),plot.background = element_rect(fill = "#172869"),
          panel.grid = element_blank()),xlab(""),ylab(""),guides(color = F))
  }
  
  A <- matrix(c(-3,1,1,1),nrow = 2)
  
  
  
  dd <- animate_grid_from_transformation(A)
  dd <- pause_animation(dd,1)
  dd <- pause_animation(dd,1,last_state = T)
  dd2 <- animate_grid_from_transformation(solve(A),A0 = A)
  dd2 <- pause_animation(dd2,1,last_state = T)
  
  dd <- stack_animations(dd,dd2)

  g <- ggplot() + geom_line(data = dd,aes(x = x,y = y, group = group,color = factor(color)),size = 2) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F)+
    add_custom_theme()+theme(axis.text = element_blank())+ scale_color_manual(breaks = 0:1,values = c("#FC6882","white"))+
    geom_segment(data = basis_df, aes(x = x, y =y , yend = yend, xend = xend),color = "cyan",size = 3,arrow = arrow(type = "open")) +# adjust_grid_limits(basis_df)+
    transition_states(state,transition_length = 1,state_length = 1,wrap = F)+
    ease_aes("linear")
  animate(g,nframes = 200, fps = 25)
  
  
  basis_df <- transform_vector(A,2,v1,v2,v3,make_vector(-1,2))
  
  
  ggplot() + geom_segment(data = df, aes(x = x, y =y , yend = yend, xend = xend)) + transition_states(state,transition_length = 1, state_length = 1) + ease_aes("linear")
  

  
  stack_animations <- function(...){  ## must have the same row - state density
    list_of_animations <- list(...)
    first_animation <- list_of_animations[[1]]
    num_animations <- length(list_of_animations)
    rows_per_state <- nrow(first_animation) / length(unique(first_animation$state))
    
    df <- do.call(rbind,list_of_animations)
    total_states <- nrow(df) / rows_per_state
    df$state <- rep(1:total_states,each = rows_per_state)
    df
    
  }
  
  delete_states <- function(df,group,state){
    df %>% filter(group != group & state != state)
  }
  
  pause_animation <- function(df, pause_num_states,custom_state = NULL,first_state = T, last_state = F){
    df <- df %>% arrange(state)

    rows_per_state <- nrow(df) / length(unique(df$state))
    num_states <- df$state[nrow(df)]
    

    if (last_state){
        state <- num_states
    }else if (first_state){
        state <- 1
        
    }else{
      state <- custom_state
    }
    

    before_frame <- df[df$state < state,]

    given_state_df <- df[df$state == state,]
    after_frame <- df[df$state > state,]
    
    new_given_df <- do.call(rbind,rep(list(given_state_df), pause_num_states + 1))
    df <- rbind(before_frame,new_given_df,after_frame)
    df$state <- rep(1:(num_states + pause_num_states),each = rows_per_state)
    
    df
    

  }
  

  nstates <- function(x){
    length(unique(x$state))
  }
  
  transform_vector <- function(A,num_states = 2,...){
    list_of_vectors <- list(...)
    list_of_transformed_vectors <- lapply(list_of_vectors,function(x) A %*% x)

    
    df <- data.frame(x = integer(0), xend = integer(0),y = integer(0),yend = integer(0),group = integer(0),state = integer(0))
    for (i in 1:length(list_of_vectors)){
      element1 <- seq(from = list_of_vectors[[i]][1,1], to = list_of_transformed_vectors[[i]][1,1], length.out = num_states)
      element2 <- seq(from = list_of_vectors[[i]][2,1], to = list_of_transformed_vectors[[i]][2,1], length.out = num_states)
      for (j in 1:num_states){

        new_v <- matrix(c(element1[j],element2[j]),nrow = 2)
        df <- rbind(df,data.frame(x = 0, xend = new_v[1,1], y = 0, yend = new_v[2,1], group = i, state = j)    )
        
        
        
      }
      
      
    }
    
    df
  }
  
  
  animate_grid_from_transformation <- function(A,num_states = 2, A0 = diag(2)){
    df_list <- list()
    
    transformed_A <- A0 %*% A
    
    element1 <- seq(from = A0[1,1], to = transformed_A[1,1], length.out = num_states)
    element2 <- seq(from = A0[2,1], to = transformed_A[2,1], length.out = num_states)
    element3 <- seq(from = A0[1,2], to = transformed_A[1,2], length.out = num_states)
    element4 <- seq(from = A0[2,2], to = transformed_A[2,2], length.out = num_states)
    for (i in 1:num_states){
      new_A <- matrix(c(element1[i],element2[i],element3[i],element4[i]),nrow = 2)
      df_list <- c(df_list,list(make_grid_df(new_A,i)))
    }
    do.call(rbind,df_list)
    
  }
    

  make_vector <- function(a,b) matrix(c(a,b), nrow = 2)
  
  make_grid_df <- function(A,state){
    integer_factor <- -50:50
     integer_factor <- integer_factor[integer_factor!=0]
     integer_factor <- c(integer_factor,0)
    
    nn <- length(integer_factor)
    
    slope_v1 <- A[2,1] / A[1,1]
    slope_v2 <- A[2,2] / A[1,2]

 
    v1_df <- do.call(rbind,mapply(get_grid_line,integer_factor * A[1,1],integer_factor,MoreArgs = list(slope = slope_v2,global_limit = 5,state = state),SIMPLIFY = F))
    v1_df$group <- rep(1:nn,each = 100)
    
    v2_df <- do.call(rbind,mapply(get_grid_line,integer_factor * A[2,2],integer_factor,MoreArgs = list(slope = slope_v1,global_limit = 5,state = state),SIMPLIFY = F))
    v2_df$group <- rep(c(nn+1):(nn*2),each = 100)
    dd <- rbind(v1_df,v2_df)
    
    
    ### Reorders the zero integer groups (equivalent to the axes) so that they overlap all other lines
    max_group <- max(dd$group)
    zero_groups <- unique(dd$group[dd$color == 1])

    dd$group[dd$group == zero_groups[1]] <- max_group + 1
    dd$group[dd$group == zero_groups[2]] <- max_group + 2
    # A0 <- matrix(c(1,0,0,1),nrow = 2)
    # slope_v1 <- A0[2,1] / A0[1,1]
    # slope_v2 <- A0[2,2] / A0[1,2]
    # v01_df <-  do.call(rbind,lapply(integer_factor * A0[1,1],FUN = get_grid_line,slope = slope_v2,global_limit = 5,state = 1))
    # v01_df$group <- rep(1:nn,each = 100)
    # v02_df <-  do.call(rbind,lapply(integer_factor * A0[2,2],FUN = get_grid_line,slope = slope_v1,global_limit = 5,state = 1))
    # v02_df$group <- rep(c(nn+1):(nn*2),each = 100)
    # 
    
    # dd <- rbind(v01_df,v02_df,v1_df,v2_df)
    # 
    # g <- ggplot(dd,aes(x = x,y = y, group = group + 3)) + geom_line(color = "#FC6882",size = 2) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F)+
    #   add_custom_theme()+theme(axis.text = element_blank(),
    #                            plot.margin = margin(0,0,0,0,"null"))+
    #   transition_states(state,transition_length = 100,state_length = 1,wrap = F)+
    #   ease_aes("linear")
    # 
    # 
    # animate(g,nframes = 100, fps = 25)
    # dd2 <- dd
    # dd2$state <- c(rep(3,20200),rep(4,20200))
    # dd3 <- rbind(dd,dd2)
    # 
    # g <- ggplot() +geom_segment(data = df, aes(x = x, y = y, xend = xend, yend = yend,color = factor(color)),arrow = arrow(type = "closed"),size = 3) + 
    #   scale_color_manual(breaks = 1:3,values = c("#FC6882","#A6E000")) + add_custom_theme() + 
    # geom_text(data = text_df,aes(x = xx, y = yy, label = paste0("hat(",id,")")),color = "white",parse = T,size = 14) +
    #   geom_line(data = dd, aes(x = x,y = y, group = group + 3),color = "#FC6882",size = 2) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F) + 
    #   theme(axis.text = element_blank(),
    #                                                                                                plot.margin = margin(0,0,0,0,"null"))+
    #   transition_states(state,transition_length = 10,state_length = 1,wrap = F) +
    #   ease_aes('linear') #+ adjust_grid_limits(df,make_square = T)
    # 
    
    dd
  }

}

shinyApp(ui = ui, server = server)