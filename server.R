server <- function(input, output) {
  
  output$ui_trans_vector1 <- renderUI(textInput("trans_vector1","",4))
  output$ui_trans_vector2 <- renderUI(textInput("trans_vector2","",6))
  
  output$ui_trans_matrix1 <- renderUI(textInput("trans_matrix1","",1.2))
  output$ui_trans_matrix2 <- renderUI(textInput("trans_matrix2","",-1))
  output$ui_trans_matrix3 <- renderUI(textInput("trans_matrix3","",.6))
  output$ui_trans_matrix4 <- renderUI(textInput("trans_matrix4","",-1))
  
  rref <- function(A, tol=sqrt(.Machine$double.eps),verbose=FALSE,
                   fractions=FALSE){
    ## A: coefficient matrix
    ## tol: tolerance for checking for 0 pivot
    ## verbose: if TRUE, print intermediate steps
    ## fractions: try to express nonintegers as rational numbers
    ## Written by John Fox
    # Modified by Geoffrey Brent 2014-12-17 to fix a bug
    if (fractions) {
      mass <- require(MASS)
      if (!mass) stop("fractions=TRUE needs MASS package")
    }
    if ((!is.matrix(A)) || (!is.numeric(A)))
      stop("argument must be a numeric matrix")
    n <- nrow(A)
    m <- ncol(A)
    x.position<-1
    y.position<-1
    # change loop:
    while((x.position<=m) & (y.position<=n)){
      col <- A[,x.position]
      col[1:n < y.position] <- 0
      # find maximum pivot in current column at or below current row
      which <- which.max(abs(col))
      pivot <- col[which]
      if (abs(pivot) <= tol) x.position<-x.position+1     # check for 0 pivot
      else{
        if (which > y.position) { A[c(y.position,which),]<-A[c(which,y.position),] } # exchange rows
        A[y.position,]<-A[y.position,]/pivot # pivot
        row <-A[y.position,]
        A <- A - outer(A[,x.position],row) # sweep
        A[y.position,]<-row # restore current row
        if (verbose)
          if (fractions) print(fractions(A))
        else print(round(A,round(abs(log(tol,10)))))
        x.position<-x.position+1
        y.position<-y.position+1
      }
    }
    for (i in 1:n)
      if (max(abs(A[i,1:m])) <= tol)
        A[c(i,n),] <- A[c(n,i),] # 0 rows to bottom
    if (fractions) fractions (A)
    else round(A, round(abs(log(tol,10))))
  }
  
  
  output$ui_system_matrix_input <- renderUI({
    
    nrow <- as.numeric(input$system_nrow)
    ncol <- as.numeric(input$system_ncol)
    
    if (!is.null(isolate(input$system_matrix_11))){
      A <- get_input_matrix("system")
      A[A == "NULL"] <- ""
    }else{
      A <- matrix(c(-2,1,2,1,2,4,2,3),nrow = nrow, ncol = ncol)
      #A <- matrix(data = replicate(nrow*ncol,""),nrow = nrow, ncol = ncol)
    }
    var_string <- c("x","y","z")
    var_string <- c(var_string[1:(ncol - 1)], "")
    op_string <- c(replicate(ncol - 2, " + ")," = ","")
    lapply(1:ncol, function(j){
      column(width = 3,
             lapply(1:nrow, function(i){
               fluidRow(
                 splitLayout(
                   cellWidths = c("50%","20%","30%"),
                   textInput(paste0("system_matrix_",i,j),"",value = A[i,j]),
                   
                   tagList(
                     br(),
                     h4(var_string[j])
                   ),
                   tagList(
                     br(),
                     h4(op_string[j])
                   )
                   
                   
                 )
                 
                 
               )
               
             })
      )
      
    } ) 
    
    
  })
  
  output$ui_space_matrix_input <- renderUI({
    
    nrow <- as.numeric(input$space_nrow)
    ncol <- as.numeric(input$space_ncol)
    
    if (!is.null(isolate(input$space_matrix_11))){
      A <- get_input_matrix("space")
      A[A == "NULL"] <- ""
    }else{
      A <- matrix(data = replicate(nrow*ncol,""),nrow = nrow, ncol = ncol)
    }
    lapply(1:ncol, function(j){
      column(width = 3,
             lapply(1:nrow, function(i){
               textInput(paste0("space_matrix_",i,j),label = "",value = A[i,j])
               
             })
      )
      
    } ) 
    
    
  })
  
  make_A <- function(x,...) UseMethod("make_A")
  
  make_v <- function(x,...) UseMethod("make_v")
  
  
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
  
  make_v.default <- function(x,...) {
    v <- unlist(list(x,...))
    v <- matrix(v,nrow = 2)
    if (length(v) == 0){
      v <- matrix(c(4,6))
    }
    structure(v,class = c("animate_vector","matrix"))
    
  }
  
  make_grid <- function(v){
    new_grid <- expand.grid(v,v)
    names(new_grid) <- c("x","y")
    structure(t(new_grid), class = c("animate_grid","matrix"))
    
  }
  
  make_color_grid <- function(v){
    new_grid <- expand.grid(v,v)
    names(new_grid) <- c("x","y")
    structure(t(new_grid), class = c("animate_color_grid","matrix"))
    
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
  
  split_into_frames_df.animate_vector <- function(v,A,n_frames = 10){
    transformed_v <- A %*% v
    el1 <- seq(from = v[1,1], to = transformed_v[1,1], length.out = n_frames)
    el2 <- seq(from = v[2,1], to = transformed_v[2,1], length.out = n_frames)
    
    init <- replicate(n_frames,0)
    df <- data.frame(x = 0, xend = init, y = 0, yend = init,frames = 0)
    
    for (i in 1:n_frames){
      new_A <- matrix(c(el1[i],el2[i]),nrow = 2)
      df[i,] <- data.frame(x = 0, xend = new_A[1,1], y = 0, yend = new_A[2,1],frames = i)
    }
    df
    
  }
  
  split_into_frames_df.eigen <- function(eigen_A,A,n_frames = 10){
    
    end_A <- A %*% eigen_A
    el1 <- seq(from = eigen_A[1,1], to = end_A[1,1], length.out = n_frames)
    el2 <- seq(from = eigen_A[2,1], to = end_A[2,1], length.out = n_frames)
    el3 <- seq(from = eigen_A[1,2], to = end_A[1,2], length.out = n_frames)
    el4 <- seq(from = eigen_A[2,2], to = end_A[2,2], length.out = n_frames)
    init <- replicate(n_frames * 2,0)
    df <- data.frame(x = 0, xend = init, y = 0, yend = init,frames = 0)
    
    for (i in 1:n_frames){
      new_A <- matrix(c(el1[i],el2[i],el3[i],el4[i]),2,2)
      row_vec <- c(i * 2 - 1,i * 2)
      df[row_vec,] <- data.frame(x = 0, xend = c(new_A[1,1],new_A[1,2]), y = 0, yend = c(new_A[2,1],new_A[2,2]),frames = i)
    }
    df
    
  }
  
  
  split_into_frames_df.animate_grid <- function(new_grid,A, n_frames = 10){
    
    n_points <- dim(new_grid)[2]
    
    
    
    total_rows <- n_frames * n_points
    
    
    df <- data.frame(x = replicate(total_rows,0),y = 0, frames = 0)
    last_grid <- A %*% new_grid
    
    list_of_matrix <- mapply(function(x,y) seq(from = x, to = y, length.out = n_frames), x = new_grid, y = last_grid)
    list_of_matrix  <- lapply(1:n_frames, function(x) list_of_matrix [x,])
    list_of_matrix  <- lapply(list_of_matrix , function(x) matrix(x,nrow = 2))
    
    
    for (i in 1:n_frames){
      row_vec <- (n_points*(i-1)+1):(n_points*i)
      m <- data.frame(t(list_of_matrix[[i]]),i)
      
      df[row_vec,] <- m
      
    }
    df
    
  }
  
  split_into_frames_df.animate_color_grid <- function(new_grid,A,n_frames = 10){
    
    df_grid <- split_into_frames_df.animate_grid(new_grid,A,n_frames)
    unique_df_colors <- df_grid %>% filter(frames == 10) %>% distinct() %>% mutate(color = 1:nrow(.))
    
    factor_colors <- df_grid %>% filter(frames == 10) %>% left_join(unique_df_colors) %>% select(color) %>% unlist() %>% unname() %>% as.factor()
    df_grid$colors <- factor_colors
    df_grid
    
  }
  
  frozen_matrix_to_df <- function(v,n_frames){
    data.frame(x = replicate(n_frames,0), 
               y = 0, 
               xend = v[1,1],
               yend = v[2,1],
               frames = 1:n_frames)
    
  }
  
  
  
  
  get_max_coordinate <- function(df1,...){
    df2 <- list(...)
    max_df1 <- max(df1$x,df1$y,df1$xend,df1$yend)
    min_df1 <- min(df1$x,df1$y,df1$xend,df1$yend)
    max_df1 <- max(abs(max_df1),abs(min_df1))
    if (length(df2) > 0){
      max_df2 <- 0
      for (i in 1:length(df2)){
        df <- df2[[i]]
        new_max <- max(df$x,df$y,df$xend,df$yend)
        new_min <- min(df$x,df$y,df$xend,df$yend)
        new_max <- max(abs(new_max),abs(new_min))
        if (max_df2 < new_max) max_df2 <- new_max
      }
      
      return(max(max_df1,max_df2))
    }
    
    max_df1
    
  }
  
  equalize_axes <- function(x,...) UseMethod("equalize_axes")
  
  
  equalize_axes.plotly <- function(p,show_legend,df1,constant = 1.2,...){
    max_val <- get_max_coordinate(df1,...) * constant
    p %>% 
      layout(showlegend = show_legend,xaxis = list(range = c(-max_val, max_val)),
             yaxis = list(range = c(-max_val, max_val)))
  }
  
  equalize_axes.default <- function(df1,constant = 1.2,...){
    max_val <- get_max_coordinate(df1,...) * constant
    list(xlim(-max_val,max_val),ylim(-max_val,max_val))
    
  }
  
  
  
  
  ### when using the color grid, must have a color aesthetic in the segment or for anything else
  
  eigen_A <- reactive({
    c(as.numeric(input$matrix1),as.numeric(input$matrix2),as.numeric(input$matrix3),as.numeric(input$matrix4))
  })
  
  trans_A <- reactive({
    make_A(c(as.numeric(input$trans_matrix1),as.numeric(input$trans_matrix2),as.numeric(input$trans_matrix3),as.numeric(input$trans_matrix4)))
    
  })
  
  trans_v <- reactive(make_v(c(as.numeric(input$trans_vector1),as.numeric(input$trans_vector2))))
  output_v <- reactive(trans_A() %*% trans_v())
  
  det_A <- reactive({
    c(as.numeric(input$det_matrix1),as.numeric(input$det_matrix2),as.numeric(input$det_matrix3),as.numeric(input$det_matrix4))
  })
  
  det_v1 <- reactive(make_v(c(as.numeric(input$input$det_matrix1),as.numeric(input$input$det_matrix1))))
  det_v2 <- reactive(make_v(c(as.numeric(input$input$det_matrix3),as.numeric(input$input$det_matrix4))))
  
  
  observeEvent(input$set_transformed_vector,{
    output_v <- isolate(output_v())
    output$ui_trans_vector1 <- renderUI(textInput("trans_vector1","",output_v[1]))
    output$ui_trans_vector2 <- renderUI(textInput("trans_vector2","",output_v[2]))
    
  }) 
  
  observeEvent(input$inverse_trans_A,{
    
    A <- trans_A()
    inv_A <- solve(A)
    print(inv_A)
    output$ui_trans_matrix1 <- renderUI(textInput("trans_matrix1","",round(inv_A[1,1],10)))
    output$ui_trans_matrix2 <- renderUI(textInput("trans_matrix2","",round(inv_A[2,1],10)))
    output$ui_trans_matrix3 <- renderUI(textInput("trans_matrix2","",round(inv_A[1,2],10)))
    output$ui_trans_matrix4 <- renderUI(textInput("trans_matrix4","",round(inv_A[2,2],10)))
    
    
  })
  
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    
    v <- seq(from = -10, to = 10, length.out = 21)
    new_grid <- make_grid(v)
    
    A <- make_A(eigen_A())
    
    df_grid <- split_into_frames_df(new_grid,A)
    
    
    A_eigen <- make_A(eigen(A))
    eigen_df <- split_into_frames_df(A_eigen,A)
    
    
    
    p <- ggplot(df_grid, aes(x = x, y = y,frame = frames)) + geom_point() + theme_minimal() +
      geom_segment(data = eigen_df,aes(x = x, y = y, xend = xend, yend = yend),size = 1, color = "green") 
    
    
    p <- ggplotly(p) %>% equalize_axes(F,eigen_df,constant = 2)
    
    p
  })
  
  
  output$trans_determinant <- renderText(paste0("Determinant = ",det(trans_A())))
  
  output$trans_math_print <- renderUI({
    
    if (input$trans_box == "Transform Vector"){
      
      trans_v <- output_v()
      
      withMathJax(
        h4(paste0('$$
                  \\begin{bmatrix}
                  ',input$trans_matrix1,' & ',input$trans_matrix3,' \\\\
                  ',input$trans_matrix2,' & ',input$trans_matrix4,'  \\\\
                  \\end{bmatrix}
                  \\begin{bmatrix}
                  ',input$trans_vector1,' \\\\
                  ',input$trans_vector2,' \\\\
                  \\end{bmatrix} = \\begin{bmatrix}
                  ',trans_v[1],' \\\\
                  ',trans_v[2],' \\\\
                  \\end{bmatrix} 
                  
                  $$'))
        )
      
      
    }
    
    
    
  })
  
  output$det_plot <- renderPlotly({
    # browser()
    v <- seq(from = -10, to = 10, length.out = 21)
    new_grid <- make_grid(v)
    
    A <- make_A(det_A())
    df_grid <- split_into_frames_df(new_grid,A)
    
    v01 <- make_v(1,0)
    v02 <- make_v(0,1)
    
    v_df1 <- split_into_frames_df(v01,A)
    v_df2 <- split_into_frames_df(v02,A)
    
    v_df1$g <- 1
    v_df2$g <- 2
    v_df2$color <- "red"
    v_df1$color <- "red"
    v_df11 <- data.frame(x = v_df1$xend, xend = v_df1$xend + v_df2$xend,
                         y = v_df1$yend, yend = v_df1$yend + v_df2$yend,
                         frames = 1:10,g = 3,color = "black")
    
    v_df22 <- data.frame(x = v_df2$xend, xend = v_df1$xend + v_df2$xend,
                         y = v_df2$yend, yend = v_df1$yend + v_df2$yend,
                         frames = 1:10,g = 4, color = "black")
    
    v_df <- rbind(v_df1,v_df2,v_df11,v_df22)  
    
    det_vec <- 1:10
    for (i in 1:10){
      det_vec[i] <- det(matrix(c(v_df1$xend[i], v_df1$yend[i], v_df2$xend[i], v_df2$yend[i]),2,2))
    }
    
    det_text_df <- v_df
    det_text_df$text <- round(det_vec,2)
    det_text_df <- det_text_df %>% group_by(frames) %>% mutate(xave = mean(c(x,xend)),
                                                               yave = mean(c(y,yend)))
    
    g <- ggplot(data = v_df, aes(x = x, y = y, xend = xend, yend = yend,frame = frames,group = factor(g),linetype = color)) + 
      geom_segment() + scale_linetype_manual(values = c("red" = "solid", "black" = "dotted"))+
      geom_text(data = det_text_df, aes(x = xave, y = yave, label = text), color = "black")
    ggplotly(g) #%>% equalize_axes(F, v_df)
    
    
    
    
  })
  
  
  output$trans_plot <- renderPlotly({
    
    
    A <- trans_A()
    
    grid_v <- seq(from = -10, to = 10, length.out = 21)
    new_grid <- make_grid(grid_v)
    
    
    
    df_grid <- split_into_frames_df(new_grid,A)
    
    v <- trans_v()
    v_df <- split_into_frames_df(v,A)
    
    if (input$trans_colors){
      colors <- df_grid %>% filter(frames == 10) %>%  group_indices(x,y)
      df_grid$colors <- colors
      
      p <- ggplot(df_grid, aes(x = x, y = y,frame = frames,color = colors)) + scale_colour_gradientn(colours = rainbow(100),guide = F) + 
        geom_point() + theme_minimal() +
        geom_segment(data = v_df,aes(x = x, y = y, xend = xend, yend = yend),color = "red",linetype = 3,size = 1) #+
      
      
    }else{
      p <- ggplot(df_grid, aes(x = x, y = y,frame = frames)) +
        geom_point() + theme_minimal() +
        geom_segment(data = v_df,aes(x = x, y = y, xend = xend, yend = yend),color = "red",linetype = 3,size = 1) #+
      
    }
    
    
    
    
    
    p <- ggplotly(p) %>% equalize_axes(F,v_df)
    
    p
    
  })
  
  get_input_matrix <- function(tab_string){
    
    nrow <- input[[paste0(tab_string,"_nrow")]]
    ncol <- input[[paste0(tab_string,"_ncol")]]
    
    A <- sapply(1:ncol,function(j){
      sapply(1:nrow, function(i){
        as.numeric(input[[paste0(tab_string,"_matrix_",i,j)]])
      },USE.NAMES = F)
    },USE.NAMES = F)
    
    A
    
  }
  
  ggplot_coord_axes_theme <- function(){
    list(geom_vline(xintercept = 0,linetype = "dotted"), geom_hline(yintercept = 0,linetype = "dotted"), theme_minimal())
  }
  
  
  scale_df <- function(df,scale = 10){
    
    ncol <- ncol(df) - 1
    
    max_val_for_each_ind <- df %>% group_by(ind) %>% 
      dplyr::summarise_all(function(x) max(abs(x))) %>% 
      select(-ind) %>% 
      apply(1,max)
    
    scale_constants <- max_val_for_each_ind / scale
    
    num_ind <- length(unique(df$ind))
    
    for (i in 1:num_ind){
      df[df$ind == i,1:ncol] <- df[df$ind == i,1:ncol] / scale_constants[i]
    }
    
    df
  }
  mag <- function(v){
    sqrt(sum(v^2))
  }
  
  observeEvent(input$space_plot_button,{
    
    A <- get_input_matrix("space")
    
    nrow <- as.numeric(isolate(input$space_nrow))
    ncol <- as.numeric(isolate(input$space_ncol))
    
    
    # parametric variable
    t <- -5:5
    vec_space_df <- switch(nrow,
                           data.frame(x = replicate(5*ncol,0)),
                           data.frame(x = replicate(5*ncol,0), y = replicate(5*ncol,0)),
                           data.frame(x = replicate(5*ncol,0), y = replicate(5*ncol,0), z = replicate(5*ncol,0))
    )
    # scales data frame so max value is 10
    
    vec_space_df$ind <- rep(1:ncol,each = 5)
    
    
    
    scale_constant <- apply(A,2, function(v) 10 / mag(v))
    t <- lapply(scale_constant, function(x) seq(from = -x, to = x, length.out = 5))
    for (j in 1:ncol){
      df_row_ind <- ((j - 1) * 5 + 1):(j * 5)
      for (i in 1:nrow){
        vec_space <- t[[j]] * A[i,j]   
        vec_space_df[df_row_ind,i] <- vec_space
        
      }
    }
    
    #vec_space_df <- scale_df(vec_space_df)
    
    
    #with(data = vec_space_df, x[ind == 1] + x[ind == 2])
    #col_space_df <- data.frame( )
    if (nrow == 1) vec_space_df$y <- 0
    
    if (nrow < 3){
      
      g <- ggplot(vec_space_df, aes(x = x, y = y, group = factor(ind), color = factor(ind)),size = 1.3) + geom_line() +  
        ggplot_coord_axes_theme() + scale_color_discrete(name = "vectors",labels = paste0("v",1:ncol)) + equalize_axes(vec_space_df) 
      
      # browser()
      g$layers <- rev(g$layers)
      g <- ggplotly(g) %>% equalize_axes(T, vec_space_df)
      
    }else{
      g <- plot_ly(vec_space_df, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',color = ~factor(ind))
      
    }
    
    output$space_plot <- renderPlotly(g)
    
  })
  
  
  make_geom_line_from_system <- function(A_row){
    if(length(A_row) == 3){
      x_range <- seq(from = -20, to = 20, length.out = 20)
      
      d <- data.frame(x = x_range, y = (-1*A_row[1]*x_range + A_row[3])/A_row[2])
      g <- geom_line(data = d, aes(x = x, y = y ))
      
    }else{
      x_range <- seq(from = -20, to = 20, length.out = 20)
      y_range <- seq(from = -20, to = 20, length.out = 20)
      region <- expand.grid(x = x_range, y = y_range)
      col_ind <<- col_ind + 1
      z <- matrix((-1*A_row[1] * region$x - 1*A_row[2]*region$y + A_row[4])/A_row[3],20,20)
      list(surface3d(x_range,y_range,z,col = c("red","blue","green","purple")[col_ind],alpha = 1))
      
    }
    
  }
  
  no_solution <- function(A_row){
    n <- length(A_row)
    all(A_row[1:(n - 1)] == 0) && A_row[n] != 0
  }
  
  all_solution <- function(A_row){
    all(A_row == 0)
  }
  
  pivot_column <- function(A_row){
    which(A_row == 1)[1]
  }
  
  get_solution_type <- function(A_rref){
    
    nvar <- ncol(A_rref) - 1
    neq <- nrow(A_rref)
    
    
    if (any(apply(A_rref, 1, no_solution))){
      solution_type <- "No Solution:"
      
      if (nvar == 2){
        if (!any(apply(A_rref, 1, all_solution))){
          return(paste0(solution_type,": Lines don't intersect"))
        }else{
          return(paste0(solution_type,": Lines are parallel"))
        }
      }else{
        return(paste0(solution_type,": Planes are parallel"))
      }
    }
    
    if (any(apply(A_rref, 1, all_solution))){
      num_all <- sum(any(apply(A_rref, 1, all_solution))) # number of rows in rref all zeros
      if (num_all >= neq - 1){
        solution_type <- "Infinite Solutions:"
        if (nvar == 2){
          return(paste0(solution_type,": Lines overlap"))
        }else{
          return(paste0(solution_type,": Planes overlap"))
        }
        
        
      } 
    }
    # at this point, either
    #     one solution:
    #                    A) with equal variables and equations neq = nvar
    #               or   B) more equations than variables (and lucky, 3 lines intersect at one point)  neq > nvar
    # 
    #     many solutions on a line when variables = 3  neq < nvar
    
    if (neq == nvar){
      
      if(nvar == 2){
        return("One Solution:  Lines intersect at a point")
      }else{
        return("One Solution:  Planes intersect at a point")
      }
    }else if (neq > nvar){
      return("One Solution:  Lines intersect at a point")
    }else{
      return("Infinite Solutions: Planes intersect on a line")
    }
    
  }
  
  plot_solution <- function(A_rref){
    # only return plot if has a solution and not all (same line or plane)
    if (any(apply(A_rref, 1, no_solution))){
      return(NULL)
    }
    
    if (any(apply(A_rref, 1, all_solution))){
      num_all <- sum(any(apply(A_rref, 1, all_solution))) # number of rows in rref all zeros
      if (num_all >= nrow(A_rref) - 1){
        return(NULL)
      } 
    }
    
    # at this point, either
    #     one solution:
    #                    A) with equal variables and equations
    #               or   B) more equations than variables (and lucky, 3 lines intersect at one point)
    # 
    #     many solutions on a line when variables = 3
    
    nvar <- ncol(A_rref) - 1
    neq <- nrow(A_rref)
    
    # one solution
    # A) and B) in one chunk
    if (nvar == neq || neq > nvar){
      if (nvar == 2){
        d <- data.frame(x = A_rref[1,3], y = A_rref[2,3])
        solution_plot <- geom_point(data = d, aes(x = x, y = y),size = 2, color = "red")
        
      }else{
        solution_plot <- rgl.spheres(x = A_rref[1,4], y = A_rref[2,4], z = A_rref[3,4], r = 3, color = "yellow" )
        
      }
      
      return(solution_plot = solution_plot)
    }
    
    # line of solutions connecting planes
    #shouldn't need this if (should be the last case)
    if (neq < nvar){
      # first column is always a pivot.  2nd usually is, but not always
      pivots <- apply(A_rref,1,pivot_column)
      free_col <- nvar[!nvar %in% pivots]
      if (free_col == 1) stop("The free column can be one!")
      
      t <- seq(from = -20, to = 20, length.out = 20)
      x <- (-1*A_rref[1,free_col]*t + A_rref[1,4]) / A_rref[1,1]
      y <- (-1*A_rref[2,free_col]*t + A_rref[2,4]) / A_rref[2,2]
      
      if (free_col == 2){
        z <- y
        y <- t
      } else z <- t
      
      solution_plot <- segments3d(x = x, y = y, z = z)
      return(solution_plot = solution_plot)
    }
    
    
    stop("Some case you haven't thought of dummy")
    
    
    
    
  }
  
  
  
  observeEvent(input$system_plot_button,{
    try(rgl.close())
    
    
    A <- get_input_matrix("system")
    
    A_rref <- rref(A)
    
    
    nrow <- as.numeric(isolate(input$system_nrow))
    ncol <- as.numeric(isolate(input$system_ncol))
    
    if (ncol == 4){
      output$ui_system_plot <- renderUI({
        rgl::rglwidgetOutput("system_plot",width = "100%",height = "600px")
      })
      
    }else{
      output$ui_system_plot <- renderUI({
        plotOutput("system_plot",width = "100%",height = "600px")
      })
      
    }
    
    
    #A <- matrix(sample(-5:5,8,replace = T), nrow = 2)
    col_ind <<-0
    print(get_solution_type(A_rref))
    
    if (ncol == 4){
      output$system_plot <- rgl::renderRglwidget({
        
        
        
        
        
        axes3d()
        apply(A, 1, make_geom_line_from_system)
        
        plot_solution(A_rref)
        title3d(xlab = "x",ylab = "y",zlab = "z")
        
        rglwidget()
        #c(axes3d(),,,solution_type_and_plot(A_rref))
        
        
        
        
      })
      
    }else{
      
      g <- apply(A, 1, make_geom_line_from_system)
      output$system_plot <- renderPlot({
        ggplot() + g + plot_solution(A_rref) +  ggplot_coord_axes_theme()
      })
      
    }
    output$ui_system_math_print <- renderUI({
      
      matrix_string <- "$$
      \\begin{bmatrix}"
      for (i in 1:nrow){
        for (j in 1:ncol){
          matrix_string <- c(matrix_string, isolate(input[[paste0("system_matrix_",i,j)]]))
          if (j == ncol){
            matrix_string <- c(matrix_string, ' \\\\')
          }else{
            matrix_string <- c(matrix_string, ' &')
          }
        }
      }
      matrix_string <- c(matrix_string,'\\end{bmatrix}
                         $$')
      
      # 
      # sapply(1:nrow,function(i){
      #   sapply(1:ncol, function(j){
      #     if (j < ncol){
      #       c(input[[paste0("system_matrix_",i,j)]],' & ')
      #     }else{
      #       c(input[[paste0("system_matrix_",i,j)]],' \\\\')
      #     }
      #   },USE.NAMES = F)
      # },USE.NAMES = F)
      # 
      # 
      # withMathJax(
      #   h4(paste0('$$
      #             \\begin{bmatrix}
      #             ',input$trans_matrix1,' & ',input$trans_matrix3,' \\\\
      #             ',input$trans_matrix2,' & ',input$trans_matrix4,'  \\\\
      #             \\end{bmatrix}
      #             \\begin{bmatrix}
      #             ',input$trans_vector1,' \\\\
      #             ',input$trans_vector2,' \\\\
      #             \\end{bmatrix} = \\begin{bmatrix}
      #             ',trans_v[1],' \\\\
      #             ',trans_v[2],' \\\\
      #             \\end{bmatrix} 
      #             
      #             $$'))
      #   )
      
      
      withMathJax(
        h4(paste0(matrix_string,collapse = ""))
      )
    })
    
    # 
    # system_lines
    # # parametric variable
    # t <- -5:5
    # vec_system_df <- switch(nrow,
    #                        data.frame(x = replicate(5*ncol,0)),
    #                        data.frame(x = replicate(5*ncol,0), y = replicate(5*ncol,0)),
    #                        data.frame(x = replicate(5*ncol,0), y = replicate(5*ncol,0), z = replicate(5*ncol,0))
    # )
    # # scales data frame so max value is 10
    # 
    # vec_system_df$ind <- rep(1:ncol,each = 5)
    # 
    # 
    # 
    # scale_constant <- apply(A,2, function(v) 10 / mag(v))
    # t <- lapply(scale_constant, function(x) seq(from = -x, to = x, length.out = 5))
    # for (j in 1:ncol){
    #   df_row_ind <- ((j - 1) * 5 + 1):(j * 5)
    #   for (i in 1:nrow){
    #     vec_system <- t[[j]] * A[i,j]   
    #     vec_system_df[df_row_ind,i] <- vec_system
    #     
    #   }
    # }
    # 
    # #vec_system_df <- scale_df(vec_system_df)
    # 
    # 
    # #with(data = vec_system_df, x[ind == 1] + x[ind == 2])
    # #col_system_df <- data.frame( )
    # if (nrow == 1) vec_system_df$y <- 0
    # 
    # if (nrow < 3){
    #   
    #   g <- ggplot(vec_system_df, aes(x = x, y = y, group = factor(ind), color = factor(ind)),size = 1.3) + geom_line() +  
    #     ggplot_coord_axes_theme() + scale_color_discrete(name = "vectors",labels = paste0("v",1:ncol)) + equalize_axes(vec_system_df) 
    #   
    #   # browser()
    #   g$layers <- rev(g$layers)
    #   g <- ggplotly(g) %>% equalize_axes(T, vec_system_df)
    #   
    # }else{
    #   g <- plot_ly(vec_system_df, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',color = ~factor(ind))
    #   
    # }
    # 
    # output$system_plot <- renderPlotly(g)
    # 
})
  
  
  
}
