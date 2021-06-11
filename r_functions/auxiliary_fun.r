plt_norm_fun <- function(mp = 0, sigma = 1, n = 1, 
                         alpha_sl = 0.05, zc = c(-1.96, 1.96), 
                         distribution = 'Z',
                         fill_area = 'outer',
                         alternative = 'none',
                         label_limits = 'none',
                         from_alpha = TRUE,
                         ylim = c(0, 0.5))
{
  # Function to plot Student t distribution with filled areas 
  #       @distribution: 'Z', 'X', 'Xbar'
  #       @fill_area: 'outer', 'inner'
  #       @alternative = 'none', 'lower', 'upper', 'two-sided'
  #       @label_limits = 'none', 'character', 'numeric'
  
  color_outer <- '#d14143'
  color_inner <- '#296e4a'
  sigma <- ifelse(distribution == 'Xbar', yes = sigma/sqrt(n), no = sigma)
  df <- data.frame(x = c(mp - 4*sigma, mp + 4*sigma))
  params <- list(mean = mp, sd = sigma)
  title <- NULL
  if(distribution == "Xbar"){
    x_label = expression(bar('X'))
  } else if (distribution == "Z" | distribution == 'X'){
    x_label = distribution
  }
  
  Linf <- min(df$x)
  Lsup <- max(df$x)
  area_lower <- area_upper <- area_c <- ''
  if (alternative == 'lower'){
    if (from_alpha){
      area_lower <- alpha_sl
      Linf <- qnorm(p = area_lower, mean = mp, sd = sigma)
    } else {
      Linf <- zc[1] #-abs(zc)
      area_lower <- pnorm(q = Linf, mean = mp, sd = sigma, lower.tail = TRUE)
      
    }
    area_lower <- round(area_lower,4)
    area_c = 1 - area_lower
  } else if (alternative == 'upper'){
    if (from_alpha){
      area_upper <- alpha_sl
      Lsup <- qnorm(p = 1-area_upper, mean = mp, sd = sigma)
    } else {
      Lsup <- zc[2] #abs(zc)
      area_upper <- pnorm(q = Lsup, mean = mp, sd = sigma, lower.tail = FALSE)
    }
    area_upper <- round(area_upper,4)
    area_c = 1 - area_upper
  } else if (alternative == 'two-sided'){
    if (from_alpha){
      area_lower <- area_upper <- alpha_sl/2
      Linf <- qnorm(p = area_lower, mean = mp, sd = sigma)
      Lsup <- qnorm(p = 1 - area_upper, mean = mp, sd = sigma)
    } else {
      Linf <- zc[1] #-abs(zc)
      Lsup <- zc[2] #abs(zc)
      area_lower <- pnorm(q = Linf, mean = mp, sd = sigma, lower.tail = TRUE)
      area_upper <- pnorm(q = Lsup, mean = mp, sd = sigma, lower.tail = FALSE)
    }
    area_lower <- round(area_lower,4)
    area_upper <- round(area_upper,4)
    area_c = 1 - (area_lower + area_upper)
  } else if (alternative == 'none'){
    title = ''
  }
  
  if (label_limits == 'none'){
    breaks = seq(min(df$x), max(df$x), by = 0.5)
    labels = round(breaks, 2)
  } else if (label_limits == 'numeric'){
    breaks = c(Linf, mp, Lsup)
    labels = round(c(Linf, mp, Lsup),2)
  } else if (label_limits == 'character'){
    breaks = c(Linf, mp, Lsup)
    labels = expression('L'['inf'], '0', 'L'['sup'])
  }
  
  plt_dnorm <- ggplot(data = df, 
                   mapping = aes(x = x)) +
    stat_function(fun = dnorm, args = params)
  
  if (fill_area == 'outer'){
    plt_dnorm <- plt_dnorm + 
      geom_area(stat = "function", fun = dnorm,
                args = params,
                fill = color_outer,
                xlim = c(min(df$x), Linf)) +
      geom_area(stat = "function", fun = dnorm,
                args = params,
                fill = color_outer,
                xlim = c(Lsup, max(df$x)))
  } else if (fill_area == 'inner'){
    if (alternative == 'two-sided'){
      plt_dnorm <- plt_dnorm + 
        geom_area(stat = "function", fun = dnorm,
                  args = params,
                  fill = color_inner,
                  xlim = c(Linf, Lsup))
    } else if (alternative == 'upper'){
      plt_dnorm <- plt_dnorm +
        geom_area(stat = "function", fun = dnorm,
                  args = params,
                  fill = color_inner,
                  xlim = c(min(df$x), Lsup))
    } else if (alternative == 'lower'){
      plt_dnorm <- plt_dnorm +
        geom_area(stat = "function", fun = dnorm,
                  args = params,
                  fill = color_inner,
                  xlim = c(Linf, max(df$x)))
    }
  }
  
  plt <- plt_dnorm +
    annotate('text', x = c((min(df$x) + Linf)/2, 0, (max(df$x) + Lsup)/2), 
             y = c(0.1, 0.2, 0.1), 
             label = c(area_lower, area_c, area_upper), size = 8) +
    labs(x = x_label, y = '',
         title = title,
         subtitle = bquote(mu == .(round(mp,2)) ~ '; ' ~ sigma == .(round(sigma,2)))) +
    ylim(ylim) +
    scale_x_continuous(breaks = breaks, 
                       limits = range(df$x), 
                       labels = labels) +
    theme_classic(base_size = 20) +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 20))
  
  return(list(plt = plt, 
              area_lower = area_lower, area_upper = area_upper, area_c = area_c,
              Linf = Linf, Lsup = Lsup))
  
  
}

# plt_norm_fun(mp = 0, sigma = 1, n = 1,
#              alpha_sl = 0.1, zc = c(-1.96, 1.96),
#              distribution = 'Xbar',
#              fill_area = 'inner',
#              alternative = 'upper',
#              label_limits = 'none',
#              from_alpha = T,
#              ylim = c(0, 0.7))

#-------------------------------------------------------------
plt_dt_fun <- function(gl = 29, fill_area, 
                       alpha_sl = 0.05, alternative,
                       label_limits = 'none',
                       from_alpha = TRUE,
                       tc, ylim = c(0, 0.5))
  {
  # Function to plot Student t distribution with filled areas 
  #       @fill_area: outer', 'inner'
  #       @alternative = 'none', 'lower', 'upper', 'two-sided'
  #       @label_limits = 'none', 'character', 'numeric'
  
  color_outer <- '#d14143'
  color_inner <- '#296e4a'
  df <- data.frame(x = c(-4,4))
  params <- list(df = gl)
  title <- NULL
  
  
  Linf <- min(df$x)
  Lsup <- max(df$x)
  area_lower <- area_upper <- area_c <- ''
  if (alternative == 'lower'){
    if (from_alpha){
      area_lower <- alpha_sl
      Linf <- qt(p = area_lower, df = gl)
    } else {
      Linf <- -abs(tc)
      area_lower <- pt(q = Linf, df = gl, lower.tail = TRUE)
      
    }
    area_lower <- round(area_lower,4)
    area_c = 1 - area_lower
  } else if (alternative == 'upper'){
    if (from_alpha){
      area_upper <- alpha_sl
      Lsup <- qt(p = 1-area_upper, df = gl)
    } else {
      Lsup <- abs(tc)
      area_upper <- pt(q = Lsup, df = gl, lower.tail = FALSE)
    }
    area_upper <- round(area_upper,4)
    area_c = 1 - area_upper
  } else if (alternative == 'two-sided'){
    if (from_alpha){
      area_lower <- area_upper <- alpha_sl/2
      Linf <- qt(p = area_lower, df = gl)
      Lsup <- qt(p = 1 - area_upper, df = gl)
    } else {
      Linf <- -abs(tc)
      Lsup <- abs(tc)
      area_lower <- pt(q = Linf, df = gl, lower.tail = TRUE)
      area_upper <- pt(q = Lsup, df = gl, lower.tail = FALSE)
    }
    area_lower <- round(area_lower,4)
    area_upper <- round(area_upper,4)
    area_c = 1 - (area_lower + area_upper)
  } else if (alternative == 'none'){
    title = ''
  }
  
  if (label_limits == 'none'){
    breaks = seq(min(df$x), max(df$x), by = 0.5)
    labels = round(breaks, 2)
  } else if (label_limits == 'numeric'){
    breaks = c(Linf, 0, Lsup)
    labels = round(c(Linf, 0, Lsup),2)
  } else if (label_limits == 'character'){
    breaks = c(Linf, 0, Lsup)
    labels = expression('L'['inf'], '0', 'L'['sup'])
  }

  plt_dt <- ggplot(data = df, 
                   mapping = aes(x = x)) +
    stat_function(fun = dt, args = params)
  
  if (fill_area == 'outer'){
    plt_dt <- plt_dt + 
      geom_area(stat = "function", fun = dt,
                args = params,
                fill = color_outer,
                xlim = c(min(df$x), Linf)) +
      geom_area(stat = "function", fun = dt,
                args = params,
                fill = color_outer,
                xlim = c(Lsup, max(df$x)))
  } else if (fill_area == 'inner'){
    if (alternative == 'two-sided'){
      plt_dt <- plt_dt + 
        geom_area(stat = "function", fun = dt,
                  args = params,
                  fill = color_inner,
                  xlim = c(Linf, Lsup))
      } else if (alternative == 'upper'){
        plt_dt <- plt_dt +
          geom_area(stat = "function", fun = dt,
                    args = params,
                    fill = color_inner,
                    xlim = c(min(df$x), Lsup))
        } else if (alternative == 'lower'){
      plt_dt <- plt_dt +
        geom_area(stat = "function", fun = dt,
                  args = params,
                  fill = color_inner,
                  xlim = c(Linf, max(df$x)))
    }
  }
    
  plt <- plt_dt +
    annotate('text', x = c((min(df$x) + Linf)/2, 0, (max(df$x) + Lsup)/2), 
             y = c(0.1, 0.2, 0.1), 
             label = c(area_lower, area_c, area_upper), size = 8) +
    labs(x = 't', y = '',
         title = title,
         subtitle = bquote('graus de liberdade' == .(round(gl,2)))) +
    ylim(ylim) +
    scale_x_continuous(breaks = breaks, 
                       limits = range(df$x), 
                       labels = labels) +
    theme_classic(base_size = 20) +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 20))
  
  return(list(plt = plt, 
              area_lower = area_lower, area_upper = area_upper, area_c = area_c,
              Linf = Linf, Lsup = Lsup))
    
    
 }

# plt_dt_fun(gl = 30, alpha_sl = 0.05,
#            from_alpha = F, tc = 0.5,
#            alternative = 'upper',
#            fill_area = 'inner',
#            label_limits = 'numeric')

#-------------------------------------------------------------
