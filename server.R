path_to_gifs <- ifelse(Sys.info()['nodename'] =="adamubuntu", "~/gganimate_linear_algebra/gifs/", "/var/www/adambirenbaum.com/public/project/linear-algebra-tutorial/gifs/")



server <- function(input, output) {
  
  math_wrap <- function(id,text,...){
    output[[id]] <- renderUI(
      tagList(
        list(...),
        withMathJax(
          text
        )
        
        
      )
    )
  }
  
  math_wrap("vector_basic_math_title", 
            h2("$$\\vec{v} \\text{ is the vector addition of the scaled basis vectors: }\\hat{i} \\text{,} \\hat{j}$$"))
  
  math_wrap("vector_basic_math_v",
            h3("$$\\vec{v} = $$"),
            br(),
            br())
  
  output$vector_basic_math_vector_text <- renderUI({
    i <- input$vector_basic_v1
    j <- input$vector_basic_v2
    tagList(
      br(),
      br())
      withMathJax(
        h3(paste0("$$\\text{} \\vec{v}= ",i,"\\hat{i}\\text{ + }",j,"\\hat{j}
                      \\text{ = }",i,"\\begin{bmatrix} 1 \\\\ 0 \\end{bmatrix} \\text{ + }",
                  j,"\\begin{bmatrix} 0 \\\\ 1 \\end{bmatrix} \\text{ = } 
                  \\begin{bmatrix}",i,"\\\\",j,"\\end{bmatrix}$$")
      )
      
    )
  })
  
  
  ggplot_coord_axes_theme <- function(){
    list(geom_vline(xintercept = 0,linetype = "dotted"), geom_hline(yintercept = 0,linetype = "dotted"), theme_minimal())
  }
  
  make_A <- function(x,...) UseMethod("make_A")
  make_A.eigen <- function(x,...){
    A <-  x$vectors
    
    structure(A, class = c("eigen","matrix"))
  }
  
  make_A.default <- function(x,...){
    
    A <- unlist(list(x,...))
    A <- matrix(A,2,2)
    
    if (is.na(A[1,1])){
      A <- matrix(c(1.2,-1,.6,-1),nrow=2)
    }
    
    structure(A, class = c("animate_matrix","matrix"))
  }
  
  make_v <- function(x,...) UseMethod("make_v")
  
  make_v.default <- function(x,...) {
    v <- unlist(list(x,...))
    v <- matrix(v,nrow = 2)
    if (length(v) == 0){
      v <- matrix(c(4,6))
    }
    structure(v,class = c("animate_vector","matrix"))
    
  }
  
  
  split_into_frames_df <- function(x,...) UseMethod("split_into_frames_df")
  
  split_into_frames_df.animate_matrix <- function(A,n_frames = 10){
    el1 <- seq(from = 1, to = A[1,1], length.out = n_frames)
    el2 <- seq(from = 0, to = A[2,1], length.out = n_frames)
    el3 <- seq(from = 0, to = A[1,2], length.out = n_frames)
    el4 <- seq(from = 1, to = A[2,2], length.out = n_frames)
    init <- replicate(n_frames * 2,0)
    df <- data.frame(x = 0, xend = init, y = 0, yend = init,frames = 0)
    
    for (i in 1:n_frames){
      new_A <- matrix(c(el1[i],el2[i],el3[i],el4[i]),2,2)
      row_vec <- c(i * 2 - 1,i * 2)
      df[row_vec,] <- data.frame(x = 0, xend = c(new_A[1,1],new_A[1,2]), y = 0, yend = c(new_A[2,1],new_A[2,2]),frames = i)
    }
    df
    
  }
  
  split_into_frames_df.animate_vector <- function(v,A = NULL,n_frames = 10,add = F, v2 = NULL, static = F){
    if (static){
      return(data.frame(x = rep(x = 0, n_frames), xend = v[1,1],y = 0, yend = v[2,1], frames  = 1))
    }
    
    if (!add){
      
      if (is.null(A)){
        el1 <- seq(from =0, to = v[1,1], length.out = n_frames)
        el2 <- seq(from = 0, to = v[2,1], length.out = n_frames)
        
      }else{
        transformed_v <- A %*% v
        el1 <- seq(from = v[1,1], to = transformed_v[1,1], length.out = n_frames)
        el2 <- seq(from = v[2,1], to = transformed_v[2,1], length.out = n_frames)
        
      }
      

      init <- replicate(n_frames,0)
      df <- data.frame(x = 0, xend = init, y = 0, yend = init,frames = 0)
      
      for (i in 1:n_frames){
        new_A <- matrix(c(el1[i],el2[i]),nrow = 2)
        df[i,] <- data.frame(x = 0, xend = new_A[1,1], y = 0, yend = new_A[2,1],frames = i)
      }
      
    }else{
      transformed_v <- v + v2
      df <- data.frame(
        x = seq(from = 0, to = v2[1,1], length.out = n_frames),
        y = seq(from = 0, to = v2[2,1], length.out = n_frames),
        xend = seq(from = v[1,1], to = transformed_v[1,1], length.out = n_frames),
        yend = seq(from = v[2,1], to = transformed_v[2,1], length.out = n_frames),
        frames = 1:n_frames
      )
      # el1 <- seq(from = v[1,1], to = transformed_v[1,1], length.out = n_frames)
      # el2 <- seq(from = v[2,1], to = transformed_v[2,1], length.out = n_frames)
      # 
      # init <- replicate(n_frames,0)
      # df <- data.frame(x = v[1,1], xend = init, y = v[2,1], yend = init,frames = 0)
      # 
      # for (i in 1:n_frames){
      #   new_A <- matrix(c(el1[i],el2[i]),nrow = 2)
      #   df[i,] <- data.frame(x = v[1,1], xend = new_A[1,1], y = v[2,1], yend = new_A[2,1],frames = i)
      # }
    }

    df
    
    
  }

  
  combine_frames_by_section <- function(frames_df,sections,text = "",n_frames = 10){

    
    if (length(frames_df) != length(sections)) stop("Number of sections must equal the number of frame data frames passed")
     number_of_sections <- length(unlist(sections))

    df <- data.frame(x = integer(0),xend = integer(0), y = integer(0), yend = integer(0), frames = integer(0),text = integer(0))
    
    for (i in 1:length(sections)){
        current_section <- sections[[i]]
        for (j in 1:length(current_section)){
          current_df <- frames_df[[i]]
          current_df$frames <- ((current_section[j] - 1)*n_frames + 1):((current_section[j] - 1)*n_frames + 10)
          
          if (!is.null(text)) current_df$text <- text[i]
          
          df <- rbind(df, current_df)
          
        }
      
    }
    
    
    # df <- do.call(rbind,list(...))
    # df$text <- rep(text,each = nrow(df) / length(text))
    df
  }
  
  observeEvent(input$tabs,{
    if (isolate(input$tabs) == "vector_basic"){
      
      output$vector_basic_gif <- renderImage({list(src = paste0(path_to_gifs,"vector_basic.gif"),contentType = 'image/gif')},deleteFile = F)
    }
    
  })
  
  
  observeEvent(input$vector_basic_plot_button,{
   
     
      x_component <- isolate(as.numeric(input$vector_basic_v1))
      y_component <- isolate(as.numeric(input$vector_basic_v2))
  
      y_vector_anim <- new_animation_vector(0,1,group = 2) %>% 
        pause_animation(1) %>% 
        animate_scalar(y_component) %>% 
        pause_animation(1)
      
      x_vector_anim <- new_animation_vector(1,0) %>% 
        pause_animation(1) %>% 
        animate_scalar(x_component) %>% 
        pause_animation(1) %>% 
        animate_addition(y_vector_anim) %>% 
        pause_animation(1)
      
      combined_anim <- combine_animations(x_vector_anim,y_vector_anim ,simultaneous = T,consec_delay = F)
      
      
      
      addition_vector_anim <- new_animation_vector(x_component,y_component)
      scaled_addition_vector_anim  <- scalar_multiplication(addition_vector_anim,1/10)
      addition_vector_anim <- add_frames(scaled_addition_vector_anim,addition_vector_anim) %>% pause_animation(3)
      combined_anim <- combine_animations(combined_anim,addition_vector_anim ,simultaneous = F,consec_delay = F)
      
      g <- ggplot() +original_grid(combined_anim ,by_ones = T) + geom_segment(data = combined_anim , aes(x = x, y = y, xend = xend, yend = yend),arrow = arrow(type = "closed"),size = 3,color = "#A6E000")  +
        transition_states(state,transition_length = 10,state_length = 1,wrap = F) +
        ease_aes('linear') + adjust_grid_limits(combined_anim ,make_square = T) + add_custom_theme() 
      
      gg <- animate(g,nframes = 50, fps = 10)
      image_write(gg,paste0(path_to_gifs,"vector_basic_temp.gif"))
      output$vector_basic_gif <- renderImage(list(src = paste0(path_to_gifs,"vector_basic_temp.gif"),contentType = 'image/gif'),deleteFile = T)
      
      

      

      
  
    
  })
  #   From first iteration of gganimate lin algebra.  need grid stuff from this
  # 
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
  # 
  # 
  # 
  # # d <- expand.grid(1:5,-10:10,5)
  # # names(d) <- c("slope","shift","global_limit")
  # # 
  # # get_grid_line <- function(slope,shift,global_limit){
  # # 
  # #   ylim1 <- shift + global_limit / slope
  # #   ylim2 <- shift - global_limit / slope
  # #   data.frame(x = seq(from  = ylim1, to = ylim2, length.out = 20)) %>% mutate(y = slope *(x - shift))
  # # }
  # # 
  # # ggplot(get_grid_line(1000,-1,5),aes(x = x, y =y )) + geom_line() + coord_cartesian(xlim = c(-5,5),ylim = c(-5,5), expand = F)
  # # 
  # # 
  # # 
  # # all_lines <- mapply(get_grid_line,slope =d$slope,shift = d$shift, global_limit = d$global_limit,SIMPLIFY = F)
  # # all_lines <- do.call(rbind,all_lines)
  # # all_lines$state <- rep(rep(1:5,each = 20),times = 21)
  # # all_lines$group <- rep(1:21,each = 20)
  # # 
  # # g <- ggplot() +
  # #   geom_line(data = all_lines, aes(x = x, y = y,group = group)) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F) +
  # #   transition_states(state,transition_length = 1000,state_length = 1,wrap = F) +
  # #   ease_aes('linear')
  # # animate(g,nframes = 400, fps = 100)
  # # 
  # get_grid_line <- function(slope,shift,global_limit,state,integer_factor){
  #   nsize <- 100
  #   if (slope == 0){
  #     ylim1 <- -global_limit
  #     ylim2 <- global_limit
  #     
  #     df <-  data.frame(x = seq(from  = ylim1, to = ylim2, length.out = nsize)) %>% mutate(y = shift,
  #                                                                                          state = state)
  #   }else if (is.infinite(slope)){
  #     df <-  data.frame(x = rep(shift,nsize), y = seq(from = -global_limit, to = global_limit, length.out = nsize),
  #                       state = state)
  #     
  #   }else{
  #     ylim1 <- shift + global_limit / slope
  #     ylim2 <- shift - global_limit / slope
  #     df <- data.frame(x = seq(from  = ylim1, to = ylim2, length.out = nsize)) %>% mutate(y = slope *(x - shift),
  #                                                                                         state = state)
  #   }
  #   
  #   if (integer_factor == 0) df$color <- 1
  #   else df$color <- 0
  #   
  #   df
  #   
  # }
  # 
  # 
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
  # 
  add_custom_theme <- function(){
    list(theme(axis.text = element_blank(),panel.background = element_rect(fill = "#172869"),plot.background = element_rect(fill = "#172869"),
               panel.grid = element_blank()),xlab(""),ylab(""),guides(color = F))
  }
  # 
  # A <- matrix(c(-3,1,1,1),nrow = 2)
  # 
  # 
  # 
  # dd <- animate_grid_from_transformation(A)
  # dd <- pause_animation(dd,1)
  # dd <- pause_animation(dd,1,last_state = T)
  # dd2 <- animate_grid_from_transformation(solve(A),A0 = A)
  # dd2 <- pause_animation(dd2,1,last_state = T)
  # 
  # dd <- stack_animations(dd,dd2)
  # 
  # g <- ggplot() + geom_line(data = dd,aes(x = x,y = y, group = group,color = factor(color)),size = 2) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F)+
  #   add_custom_theme()+theme(axis.text = element_blank())+ scale_color_manual(breaks = 0:1,values = c("#FC6882","white"))+
  #   geom_segment(data = basis_df, aes(x = x, y =y , yend = yend, xend = xend),color = "cyan",size = 3,arrow = arrow(type = "open")) +# adjust_grid_limits(basis_df)+
  #   transition_states(state,transition_length = 1,state_length = 1,wrap = F)+
  #   ease_aes("linear")
  # animate(g,nframes = 200, fps = 25)
  # 
  # 
  # basis_df <- transform_vector(A,2,v1,v2,v3,make_vector(-1,2))
  # 
  # 
  # ggplot() + geom_segment(data = df, aes(x = x, y =y , yend = yend, xend = xend)) + transition_states(state,transition_length = 1, state_length = 1) + ease_aes("linear")
  # 
  # 
  # 
  # stack_animations <- function(...){  ## must have the same row - state density
  #   list_of_animations <- list(...)
  #   first_animation <- list_of_animations[[1]]
  #   num_animations <- length(list_of_animations)
  #   rows_per_state <- nrow(first_animation) / length(unique(first_animation$state))
  #   
  #   df <- do.call(rbind,list_of_animations)
  #   total_states <- nrow(df) / rows_per_state
  #   df$state <- rep(1:total_states,each = rows_per_state)
  #   df
  #   
  # }
  # 
  # delete_states <- function(df,groups,states){
  #   df %>% filter(!(group %in% groups & state %in% states))
  # }
  # 
  # pause_animation <- function(df, pause_num_states,custom_state = NULL,first_state = T, last_state = F){
  #   df <- df %>% arrange(state)
  #   
  #   rows_per_state <- nrow(df) / length(unique(df$state))
  #   num_states <- df$state[nrow(df)]
  #   
  #   
  #   if (last_state){
  #     state <- num_states
  #   }else if (first_state){
  #     state <- 1
  #     
  #   }else{
  #     state <- custom_state
  #   }
  #   
  #   
  #   before_frame <- df[df$state < state,]
  #   
  #   given_state_df <- df[df$state == state,]
  #   after_frame <- df[df$state > state,]
  #   
  #   new_given_df <- do.call(rbind,rep(list(given_state_df), pause_num_states + 1))
  #   df <- rbind(before_frame,new_given_df,after_frame)
  #   df$state <- rep(1:(num_states + pause_num_states),each = rows_per_state)
  #   
  #   df
  #   
  #   
  # }
  # 
  # 
  # nstates <- function(x){
  #   length(unique(x$state))
  # }
  # 
  # transform_vector <- function(A,num_states = 2,...){
  #   list_of_vectors <- list(...)
  #   list_of_transformed_vectors <- lapply(list_of_vectors,function(x) A %*% x)
  #   
  #   
  #   df <- data.frame(x = integer(0), xend = integer(0),y = integer(0),yend = integer(0),group = integer(0),state = integer(0))
  #   for (i in 1:length(list_of_vectors)){
  #     element1 <- seq(from = list_of_vectors[[i]][1,1], to = list_of_transformed_vectors[[i]][1,1], length.out = num_states)
  #     element2 <- seq(from = list_of_vectors[[i]][2,1], to = list_of_transformed_vectors[[i]][2,1], length.out = num_states)
  #     for (j in 1:num_states){
  #       
  #       new_v <- matrix(c(element1[j],element2[j]),nrow = 2)
  #       df <- rbind(df,data.frame(x = 0, xend = new_v[1,1], y = 0, yend = new_v[2,1], group = i, state = j)    )
  #       
  #       
  #       
  #     }
  #     
  #     
  #   }
  #   
  #   df
  # }
  # 
  # 
  # animate_grid_from_transformation <- function(A,num_states = 2, A0 = diag(2)){
  #   df_list <- list()
  #   
  #   transformed_A <- A0 %*% A
  #   
  #   element1 <- seq(from = A0[1,1], to = transformed_A[1,1], length.out = num_states)
  #   element2 <- seq(from = A0[2,1], to = transformed_A[2,1], length.out = num_states)
  #   element3 <- seq(from = A0[1,2], to = transformed_A[1,2], length.out = num_states)
  #   element4 <- seq(from = A0[2,2], to = transformed_A[2,2], length.out = num_states)
  #   for (i in 1:num_states){
  #     new_A <- matrix(c(element1[i],element2[i],element3[i],element4[i]),nrow = 2)
  #     df_list <- c(df_list,list(make_grid_df(new_A,i)))
  #   }
  #   do.call(rbind,df_list)
  #   
  # }
  # 
  # 
  # make_vector <- function(a,b) matrix(c(a,b), nrow = 2)
  # 
  # make_grid_df <- function(A,state){
  #   integer_factor <- -50:50
  #   integer_factor <- integer_factor[integer_factor!=0]
  #   integer_factor <- c(integer_factor,0)
  #   
  #   nn <- length(integer_factor)
  #   
  #   slope_v1 <- A[2,1] / A[1,1]
  #   slope_v2 <- A[2,2] / A[1,2]
  #   
  #   
  #   v1_df <- do.call(rbind,mapply(get_grid_line,integer_factor * A[1,1],integer_factor,MoreArgs = list(slope = slope_v2,global_limit = 5,state = state),SIMPLIFY = F))
  #   v1_df$group <- rep(1:nn,each = 100)
  #   
  #   v2_df <- do.call(rbind,mapply(get_grid_line,integer_factor * A[2,2],integer_factor,MoreArgs = list(slope = slope_v1,global_limit = 5,state = state),SIMPLIFY = F))
  #   v2_df$group <- rep(c(nn+1):(nn*2),each = 100)
  #   dd <- rbind(v1_df,v2_df)
  #   
  #   
  #   ### Reorders the zero integer groups (equivalent to the axes) so that they overlap all other lines
  #   max_group <- max(dd$group)
  #   zero_groups <- unique(dd$group[dd$color == 1])
  #   
  #   dd$group[dd$group == zero_groups[1]] <- max_group + 1
  #   dd$group[dd$group == zero_groups[2]] <- max_group + 2
  #   # A0 <- matrix(c(1,0,0,1),nrow = 2)
  #   # slope_v1 <- A0[2,1] / A0[1,1]
  #   # slope_v2 <- A0[2,2] / A0[1,2]
  #   # v01_df <-  do.call(rbind,lapply(integer_factor * A0[1,1],FUN = get_grid_line,slope = slope_v2,global_limit = 5,state = 1))
  #   # v01_df$group <- rep(1:nn,each = 100)
  #   # v02_df <-  do.call(rbind,lapply(integer_factor * A0[2,2],FUN = get_grid_line,slope = slope_v1,global_limit = 5,state = 1))
  #   # v02_df$group <- rep(c(nn+1):(nn*2),each = 100)
  #   # 
  #   
  #   # dd <- rbind(v01_df,v02_df,v1_df,v2_df)
  #   # 
  #   # g <- ggplot(dd,aes(x = x,y = y, group = group + 3)) + geom_line(color = "#FC6882",size = 2) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F)+
  #   #   add_custom_theme()+theme(axis.text = element_blank(),
  #   #                            plot.margin = margin(0,0,0,0,"null"))+
  #   #   transition_states(state,transition_length = 100,state_length = 1,wrap = F)+
  #   #   ease_aes("linear")
  #   # 
  #   # 
  #   # animate(g,nframes = 100, fps = 25)
  #   # dd2 <- dd
  #   # dd2$state <- c(rep(3,20200),rep(4,20200))
  #   # dd3 <- rbind(dd,dd2)
  #   # 
  #   # g <- ggplot() +geom_segment(data = df, aes(x = x, y = y, xend = xend, yend = yend,color = factor(color)),arrow = arrow(type = "closed"),size = 3) + 
  #   #   scale_color_manual(breaks = 1:3,values = c("#FC6882","#A6E000")) + add_custom_theme() + 
  #   # geom_text(data = text_df,aes(x = xx, y = yy, label = paste0("hat(",id,")")),color = "white",parse = T,size = 14) +
  #   #   geom_line(data = dd, aes(x = x,y = y, group = group + 3),color = "#FC6882",size = 2) + coord_cartesian(xlim = c(-5,5),ylim=c(-5,5),expand = F) + 
  #   #   theme(axis.text = element_blank(),
  #   #                                                                                                plot.margin = margin(0,0,0,0,"null"))+
  #   #   transition_states(state,transition_length = 10,state_length = 1,wrap = F) +
  #   #   ease_aes('linear') #+ adjust_grid_limits(df,make_square = T)
  #   # 
  #   
  #   dd
  # }
  
  
  
  ##########################
  ##########################
  ##########################
  #########################
  
  
  
  
  
  new_animation_vector <- function(x,...) UseMethod("new_animation_vector")
  
  
  new_animation_vector.default <- function(x,y,xshift = 0, yshift = 0,group = 1, state = 1){
    
    animation_df <- data.frame(x = xshift, xend = x, y = yshift, yend = y, group = group, state = state )
    
    structure(animation_df, class = c("animation_vector","data.frame"))
  }
  
  scalar_multiplication <- function(x,...) UseMethod("scalar_multiplication")
  
  
  scalar_multiplication.animation_vector <- function(v,s){
    
    v <- data.frame(v[,1:4] * s, group = v$group, state = v$state)
    attributes(v)$class <- c("animation_vector","data.frame")
    v
  }
  
  shift_vector <- function(x,...) UseMethod("shift_vector")
  
  shift_vector.numeric <- function(xshift,yshift,v){
    v$x <- v$x + xshift
    v$y <- v$y + yshift
    
    v$xend <- v$xend + xshift
    v$yend <- v$yend + yshift
    
    v
  }
  
  shift_vector.animation_vector <- function(vector_to_move, shift_by_vector){
    vector_to_move$x <- vector_to_move$x + shift_by_vector$xend
    vector_to_move$y <- vector_to_move$y + shift_by_vector$yend
    
    vector_to_move$xend <- vector_to_move$xend + shift_by_vector$xend
    vector_to_move$yend <- vector_to_move$yend + shift_by_vector$yend
    vector_to_move
    
  }
  
  
  
  
  add_frames <- function(x,...) UseMethod("add_frames")
  
  add_frames.animation_vector <- function(v0,v1,num_frames = 2,start_at_current = T, manual_start_state = NULL){
    
    if (start_at_current){
      new_v0 <- current_state(v0)
      current_state <- new_v0$state[1]
      states <- (1:num_frames) + current_state - 1
    }else{
      new_v0 <- v0[v0$state == manual_start_state]
      states <- (1:num_frames) + manual_start_state - 1
    } 
    
    
    x <- seq(from = new_v0$x, to = v1$x, length.out = num_frames)
    xend <- seq(from = new_v0$xend, to = v1$xend, length.out = num_frames)
    y <- seq(from = new_v0$y, to = v1$y, length.out = num_frames)
    yend <- seq(from = new_v0$yend, to = v1$yend, length.out = num_frames)
    
    group <- v0$group[1]
    
    new_v0 <- data.frame(x = x, xend = xend, y = y, yend = yend, group = group, state = states)
    new_v0 <- new_v0[-1,] # first frame is already in v0
    rbind(v0,new_v0)
  }
  
  current_state <- function(x,...) UseMethod("current_state")
  
  current_state.animation_vector <- function(x){
    i <- order(x$state)
    x <- x[i,]
    x[nrow(x),]
    
    
    
  } 
  
  
  animate_addition <- function(x,...) UseMethod("animate_addition")
  
  animate_addition.animation_vector <- function(vector_to_move, shift_by_vector,num_frames = 2,start_at_current = T, manual_start_state = NULL){
    
    shift_by_vector <- shift_by_vector[nrow(shift_by_vector),]   # shifting by the most recent state of shift vector
    
    shifted_vector <- shift_vector(current_state(vector_to_move), current_state(shift_by_vector))
    
    vector_to_move <- add_frames(vector_to_move, shifted_vector,num_frames,start_at_current,manual_start_state)
    
    
    vector_to_move
  }
  
  animate_scalar <- function(x,...) UseMethod("animate_scalar")
  
  animate_scalar.animation_vector <- function(v,s,num_frames = 2, start_at_current = T, manual_start_state = NULL){
    v2 <- scalar_multiplication(current_state(v),s)
    v <- add_frames(v,v2,num_frames,start_at_current,manual_start_state )
    v
  }
  
  
  add_vector <- function(x,...)  UseMethod("add_vector")
  
  add_vector.numeric <- function(s,v){
    v$xend <- v$xend + s
    v$yend <- v$yend + s
    v
  }
  
  add_vector.animation_vector <- function(vector_to_return,vector_to_add){
    vector_to_return$x <- vector_to_return$x + vector_to_add$xend
    vector_to_return$y <- vector_to_return$y + vector_to_add$yend
    
    vector_to_return$xend <- vector_to_return$xend + vector_to_add$xend
    vector_to_return$yend <- vector_to_return$yend + vector_to_add$yend
    
    
    vector_to_return
  }
  
  
  
  pause_animation <- function(x,...) UseMethod("pause_animation")
  
  pause_animation.animation_vector <- function(v,pause_length,custom_state = NULL,current_state = T){
    if (pause_length == 0) return(v)
    
    if (current_state){
      state <- max(v$state)
    }else{
      state <- custom_state
    }
    
    
    before_frame <- v[v$state < state,]
    
    given_state_df <- v[v$state == state,]
    after_frame <- v[v$state > state,]
    
    new_given_df <- do.call(rbind,rep(list(given_state_df), pause_length + 1))
    new_v <- rbind(before_frame,new_given_df,after_frame)
    new_v$state <- (v$state[1]):(nrow(new_v) + v$state[1] - 1)
    
    new_v
    
    
    
  }
  
  combine_animations <- function(x,...) UseMethod("combine_animations")
  
  combine_animations.animation_vector <- function(v,...,simultaneous = T,pause_if_unequal =  T,consec_delay = F){
    
    animation_list <- list(v,...)
    
    
    n_animations <- length(animation_list) 
    
    
    
    if (simultaneous){
      
      
      if (pause_if_unequal){
        max_state_in_each <- sapply(animation_list,function(x) max(x$state),USE.NAMES = F)
        overall_max <- max(max_state_in_each)
        rep_0 <- rep(0,overall_max * n_animations)
        
        combined_animation <- data.frame(x = rep_0, xend = rep_0, y = rep_0, yend = rep_0, group = rep_0, state = rep_0)
        for (i in 1:n_animations){
          current_animation <- animation_list[[i]]
          current_max <- max_state_in_each[i]
          beg_ind <- overall_max * (i - 1) + 1
          end_ind <- overall_max * i
          if (current_max == overall_max) combined_animation[beg_ind:end_ind,] <- current_animation
          else{
            current_animation <- current_animation %>%  pause_animation(overall_max - current_max)
            combined_animation[beg_ind:end_ind,] <- current_animation
          }
          
        }
        
      }else{
        combined_animation <- do.call(rbind,animation_list)
      }
      
    }else{  # consecutive
      
      size_of_each <- sapply(animation_list,function(x) length(x$state),USE.NAMES = F)
      
      if (consec_delay) rep_0 <- rep(0,sum(size_of_each))
      else rep_0 <- rep(0,sum(size_of_each) - n_animations + 1)
      
      combined_animation <- data.frame(x = rep_0, xend = rep_0, y = rep_0, yend = rep_0, group = rep_0, state = rep_0)
      for (i in 1:n_animations){
        previous_state <- max(combined_animation$state)
        current_animation <- animation_list[[i]]
        if (consec_delay) states <- current_animation$state + previous_state
        else states <- current_animation$state + previous_state - 1 + ifelse(i==1,1,0)
        current_animation$state <- states
        
        beg_ind <- min(which(combined_animation$state == 0))
        end_ind <- beg_ind + size_of_each[i] - 1
        
        combined_animation[beg_ind:end_ind,] <- current_animation
      }
      
    }
    
    
    attributes(combined_animation)$class <- c("combined_animation","data.frame")
    combined_animation
  }
  
  combine_animations.combined_animation <- function(D,...,simultaneous = T,pause_if_unequal = T,consec_delay = F){
    animation_list <- list(...)
    n_animations <- length(animation_list)
    
    D_broken_out <- lapply(unique(D$group), function(x) D[D$group == x,])
    overall_max <- max(sapply(D_broken_out,function(x) max(x$state)))
    
    if (simultaneous){
      if (pause_if_unequal){
        max_state_in_each <- sapply(animation_list,function(x) max(x$state),USE.NAMES = F)
        
        rep_0 <- rep(0,overall_max * n_animations)
        
        combined_animation <- data.frame(x = rep_0, xend = rep_0, y = rep_0, yend = rep_0, group = rep_0, state = rep_0)
        for (i in 1:n_animations){
          current_animation <- animation_list[[i]]
          current_max <- max_state_in_each[i]
          beg_ind <- overall_max * (i - 1) + 1
          end_ind <- overall_max * i
          if (current_max == overall_max) combined_animation[beg_ind:end_ind,] <- current_animation
          else{
            current_animation <- current_animation %>%  pause_animation(overall_max - current_max)
            combined_animation[beg_ind:end_ind,] <- current_animation
          }
          
        }
        
        combined_animation <- rbind(D,combined_animation)
        
        
      }else{
        combined_animation <- rbind(D,do.call(rbind,animation_list))
      }
      
    }else{
      
      if (pause_if_unequal){
        size_of_each <- sapply(animation_list,function(x) length(x$state),USE.NAMES = F)
        if (consec_delay) longest_animation <- max(size_of_each)
        else longest_animation <- max(size_of_each) -1
        
        
        
        D <- lapply(D_broken_out,function(x){       
          attributes(x)$class <- c("animation_vector","data.frame")
          pause_animation(x,longest_animation,current_state = F,custom_state = max(x$state))
        })
        D <- do.call(rbind,D)
        
        
      }
      combined_animation <- D
      for (i in 1:n_animations){
        current_animation <- animation_list[[i]]
        
        if (consec_delay) states <- (overall_max + 1):(nrow(current_animation) + overall_max)
        else states <- (overall_max):(nrow(current_animation) + overall_max - 1)
        
        current_animation$state <- states
        combined_animation <- rbind(combined_animation, current_animation)
        
      }
      
      
      
      
      
    }
    attributes(combined_animation)$class <- c("combined_animation","data.frame")
    # v <- D_broken_out[[1]]
    # attributes(v)$class <- c("animation_vector","data.frame")
    # D_broken_out <- D_broken_out[-1]
    # D_broken_out <- c(D_broken_out, animation_list)
    # combine_animations(v,D_broken_out,simultaneous = simultaneous,pause_if_unequal = pause_if_unequal)
    # 
    combined_animation
  }
  
  
  
  
  
  quick_anim <- function(g) animate(g, nframes = 20, fps = 5)
  
  
  
  
  
  ##########################
  ##########################
  ##########################
  ##########################
  
  
  
  
  
  
  
  
  
  
  
  

}
