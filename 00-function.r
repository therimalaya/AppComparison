## Print a vector
catvec <- function(vec) {
  v1 <- paste(vec[-length(vec)], collapse = ", ")
  return(paste(v1, vec[length(vec)], sep = " and "))
}

## Convert list of parameters to a character ----
list2chr <- function(lst){
  paste(map_chr(lst, function(x) {
    paste(x, collapse = ", ")
  }), collapse = "; ")
}

## Get integer from a text removing all the strings ----
get_integer <- function(vec) {
  ret <- gsub("^\\D+", "", vec) %>% as.numeric()
  return(ret)
}

## Extract single design from a design table ready for simulation ----
get_design <- function(design_tbl, which = 1L){
  design_tbl %>% slice(which) %>% transpose %>% .[[1]]
}

## Simulate data from suppled design paramters ----
simulate <- function(design) {
  do.call(simrel, design)
}

## Extract true values from the simulated object ----
get_true_value <- function(sim_obj, what = c("coef", "minerror", "sigma_x", "sigma_y")) {
  what <- match.arg(what, what)
  switch(
    what,
    coef = {
      out <- as.matrix(rbind(sim_obj$beta0, sim_obj$beta))
      dimnames(out) <- list(
        c("(Intercept)", paste0("X", 1:(nrow(out) - 1))),
        paste0("Y", 1:ncol(out))
      )
      return(out)
    },
    minerror = sim_obj$minerror,
    sigma_x = sim_obj$Sigma[-c(1:sim_obj$m), -c(1:sim_obj$m)],
    sigma_x = sim_obj$Sigma[c(1:sim_obj$m), c(1:sim_obj$m)]
  )
}

## Get data from simulated object, if need_pc than x will the principal components ----
get_data <- function(sim_obj, need_pc = FALSE, prop = ifelse(need_pc, 0.95, NULL), ncomp = NULL) {
  Y <- sim_obj$Y
  if (need_pc) {
    pc.a <- prcomp(sim_obj$X)
    nc <- max(which(cumsum(pc.a$sdev/sum(pc.a$sdev)) < prop))
    if (!is.null(ncomp)) nc <- max(ncomp, nc)
    X <- pc.a$x[, 1:nc]
    out <- data.frame(y = I(Y), x = I(X))
    attr(out, "rotation") <- pc.a$rotation[, 1:nc]
  } else {
    X <- sim_obj$X
    out <- data.frame(y = I(Y), x = I(X))
  }
  class(out) <- append(class(out), "simrel_data")
  return(out)
}

## Fit a model with data and method supplied ----
fit_model <- function(data, method = c("PCR", "PLS1", "PLS2", "Xenv", "Yenv", "Ridge", "Lasso", "Senv")) {
  method <- match.arg(method, method)
  if (method %in% c("Ridge", "Lasso")) require(doParallel)
  fit <- switch(
    method,
    PCR   = pcr(y ~ x, data = data, ncomp = 10),
    PLS2  = plsr(y ~ x, data = data, ncomp = 10),
    Xenv  = map(1:min(10, ncol(data$x)), function(nc) with(data, try(xenv(x, y, u = nc)))),
    Yenv  = map(1:ncol(data$y), function(nc) with(data, try(env(x, y, u = nc)))),
    Senv = map(1:10, function(nc) with(data, try(stenv(x, y, q = nc, u = 2)))),
    Ridge = cv.glmnet(data$x, data$y, family = "mgaussian", alpha = 0, parallel = TRUE, nlambda = 50),
    Lasso = cv.glmnet(data$x, data$y, family = "mgaussian", alpha = 1, parallel = TRUE, nlambda = 50),
    PLS1  = map(1:ncol(data$y), function(i){
        dta <- data.frame(y = data$y[, i], x = data$x)
        plsr(y ~ x, data = dta)
      })
  )
  class(fit) <- append(class(fit), "fitted_model")
  return(fit)
}

## get Estimated coefficients ----
get_beta <- function(method = c('pcr', 'pls', 'cppls', 'mvr', 'xenv',
                                'cpls', "ridge", "lasso", "senv",
                                'try-error', "glmnet", "cv.glmnet",
                                "yenv", "senv", "pls1", 'pls2')){
  mthd <- match.arg(method)
  if (mthd %in% c('pcr', 'pls', 'cppls', 'cpls', 'pls2')) method <- 'mvr'
  if (mthd %in% c("senv", "stenv")) method <- "senv"
  if (mthd %in% c("glmnet", "cv.glmnet", "ridge", "lasso")) method <- "glmnet"
  if (mthd %in% c("try-error")) return(method)
  switch(method,
         mvr    = {
           coefs <- function(mdl, ncomp = 10, intercept = TRUE)
           {
             coef <- drop(coef(mdl, intercept = intercept, ncomp = 1:ncomp))
             dimnames(coef)[[3]] <- paste0("Comp", 1:dim(coef)[3])
             return(coef)
           }
         },
         pls1   = {
           coefs <- function(mdl, ncomp = 10, intercept = TRUE)
           {
             out <- sapply(mdl, coef, intercept = intercept,
                           ncomp = 1:ncomp, simplify = 'array')
             out <- unname(aperm(drop(out), c(1, 3, 2)))
             dimnames(out) <- list(
               paste0("X", 1:dim(out)[1]),
               paste0("Y", 1:dim(out)[2]),
               paste0("Comp", 1:dim(out)[3])
             )
             return(out)
           }
         },
         xenv   = {
           coefs <- function(mdl, intercept = TRUE)
           {
             out <- sapply(mdl, function(obj) {
               if (intercept) rbind(c(unname(obj$mu)), unname(obj$beta))
               else unname(obj$beta)
             }, simplify = 'array')
             dx <- dim(out)[1]
             dy <- dim(out)[2]
             dz <- dim(out)[3]
             dnx <- if (intercept) c("Intercept", paste0("X", 1:(dx - 1))) else paste0("X", 1:dx)
             dny <- paste0("Y", 1:dy)
             dnz <- paste0("Comp", 1:dz)
             dimnames(out) <- list(dnx, dny, dnz)
             return(out)
           }
         },
         yenv   = {
           coefs <- function(mdl, intercept = TRUE)
           {
             out <- sapply(mdl, function(obj) {
               unname(t(obj$beta))
             }, simplify = 'array')
             if (intercept) {
               out <- abind::abind(sapply(mdl, "[[", "alpha"), out, along = 1)
             }
             dimnames(out) <- list(
               paste0("X", 1:dim(out)[1]),
               paste0("Y", 1:dim(out)[2]),
               paste0("Comp", 1:dim(out)[3])
             )
             return(out)
           }
         },
         senv   = {
           coefs <- function(mdl, intercept = TRUE)
           {
             out <- sapply(mdl, function(obj) {
               unname(obj$beta)
             }, simplify = 'array')
             if (intercept) {
               out <- abind::abind(sapply(mdl, "[[", "alpha"), out, along = 1)
             }
             dimnames(out) <- list(
               paste0("X", 1:dim(out)[1]),
               paste0("Y", 1:dim(out)[2]),
               paste0("Comp", 1:dim(out)[3])
             )
             return(out)
           }
         },
         glmnet = {
           coefs <- function(mdl, intercept = TRUE)
           {
             o <- options(scipen = 999)
             coef <- sapply(mdl$glmnet.fit$beta, as.matrix, simplify = 'array')
             if (intercept) {
               beta0 <- mdl$glmnet.fit$a0
               coef <- abind::abind(t(beta0), coef, along = 1)
               dimnames(coef)[[1]][1] <- "Intercept"
             }
             out <- aperm(coef, c(1, 3, 2))
             dimnames(out)[[3]] <- round(mdl$glmnet.fit$lambda, 5)
             attr(out, "lambda") <- mdl$glmnet.fit$lambda
             attr(out, "lambda.min") <- mdl$lambda.min
             attr(out, "lambda.1se") <- mdl$lambda.1se
             options(o)
             return(out)
           }
         })
  return(coefs)
}
get_est_beta <- function(fit, model_name, beta_fun = get_beta(tolower(model_name)), rotation_mat = NULL, intercept = TRUE) {
  if (!is.null(rotation_mat)) {
    out <- beta_fun(fit, intercept = intercept)
    tmp <- array(dim = c(dim(rotation_mat)[1], dim(out)[-1]))
    for (idx in seq.int(dim(out)[3])) {
      tmp[, , idx] <- rotation_mat %*% out[, , idx]
    }
    dnames <- dimnames(out)
    dnames[[1]] <- paste0("X", 1:dim(tmp)[1])
    dimnames(tmp) <- dnames
    out <- tmp
  } else {
    out <- beta_fun(fit, intercept = intercept)
  }
  class(out) <- append(class(out), "estimated_coefficient")
  return(out)
}

## Compute prediction and estimation error ----
getPredErr <- function(coef, minerr, trueBeta, sigma){
  out <- map(0:dim(coef)[3], function(cmp){
    if (cmp == 0) {
      bmat <- matrix(0, nrow = nrow(coef[, , cmp + 1]), ncol = ncol(coef[, , cmp + 1]))
    } else {
      bmat <- coef[, , cmp]
    }
    if (dim(sigma)[1] + 1 == nrow(bmat)) sigma <- rbind(0, cbind(0, sigma))
    err_out <- t(bmat - trueBeta) %*% sigma %*% (bmat - trueBeta)
    diag(err_out + minerr)
  })
  names(out) <- c("0", dimnames(coef)[[3]])
  ret <- reshape2::melt(out)
  names(ret) <-   c("Pred_Error", "Tuning_Param")
  ret <- ret %>%
    mutate_at("Tuning_Param", get_integer) %>%
    group_by(Tuning_Param) %>%
    mutate(Response = 1:n()) %>%
    ungroup()
  class(ret) <- append(class(ret), "prediction_error")
  return(ret)
}
getEstErr <- function(coef, trueBeta){
  out <- map(0:dim(coef)[3], function(cmp){
    if (cmp == 0) {
      bmat <- matrix(0, nrow = nrow(coef[, , cmp + 1]),
                     ncol = ncol(coef[, , cmp + 1]))
    } else {
      bmat <- coef[, , cmp]
    }
    err_out <- t(bmat - trueBeta) %*% (bmat - trueBeta)
    diag(err_out)
  })
  names(out) <- c("0", dimnames(coef)[[3]])
  ret <- reshape2::melt(out) %>%
    `names<-`(c("Est_Error", "Tuning_Param"))
  ret <- ret %>%
    group_by(Tuning_Param) %>%
    mutate(Response = 1:n()) %>%
    ungroup() %>%
    mutate_at("Tuning_Param", get_integer)
  class(ret) <- append(class(ret), "estimation_error")
  return(ret)
}
compute_intercept <- function(B, Xmeans, Ymeans) {
  dB <- dim(B)
  dB[1] <- dB[1] + 1
  dnB <- dimnames(B)
  dnB[[1]] <- c("(Intercept)", dnB[[1]])
  BInt <- array(dim = dB, dimnames = dnB)
  BInt[-1, , ] <- B
  for (i in 1:dim(B)[3])
    BInt[1, , i] <- Ymeans - Xmeans %*% B[, , i]
  B <- BInt
  return(B)
}
coef_errors <- function(sim_obj, est_method, ...) {
  dta <- sim_obj %>%
    get_data(...)
  rot <- attr(dta, "rotation")
  Ymeans <- colMeans(sim_obj$Y)
  Xmeans <- colMeans(sim_obj$X)
  coef <- dta %>%
    fit_model(est_method) %>%
    get_est_beta(tolower(est_method), rotation_mat = rot, intercept = FALSE)
  coef <- compute_intercept(coef, Xmeans, Ymeans)

  ## True and Estimated Coefficient Data Frame ----
  coef_df <- reshape2::melt(coef, varnames = c("Predictor", "Response", "Components"), value.name = "Estimated")
  true_coef <- get_true_value(sim_obj, "coef")
  true_coef_df <- reshape2::melt(true_coef, varnames = c("Predictor", "Response"), value.name = "True")
  coef_df <- coef_df %>% spread(Components, Estimated) %>% left_join(true_coef_df, by = c("Predictor", "Response"))
  class(coef_df) <- append(class(coef_df), "coefficient_df")

  ## Prediction Error ----
  pred_err <- coef %>%
    getPredErr(
      minerr = get_true_value(sim_obj, "minerror"),
      trueBeta = get_true_value(sim_obj, "coef"),
      sigma = get_true_value(sim_obj, "sigma_x")
    )
  class(pred_err) <- append(class(pred_err), "prediction_error_df")

  ## Estimation Error ----
  est_err <- coef %>%
    getEstErr(trueBeta = get_true_value(sim_obj, "coef"))
  class(est_err) <- append(class(est_err), "estimation_error_df")

  out <- list(
    coefficients = coef_df,
    prediction_error = pred_err,
    estimation_error = est_err
  )
  attr(out, "Method") <- est_method
  attr(out, "Sim_Properties") <- list(
    n = sim_obj$n,
    p = sim_obj$p,
    q = sim_obj$q,
    m = sim_obj$m,
    relpos = sim_obj$relpos,
    ypos = sim_obj$ypos,
    gamma = sim_obj$gamma,
    eta = sim_obj$eta,
    R2 = sim_obj$R2,
    ntest = sim_obj$ntes,
    muX = sim_obj$muX,
    muY = sim_obj$muY,
    type = sim_obj$type
  )
  class(out) <- append(class(out), "coefficient_error")
  return(out)
}

## Complete the following function making it pure but compatible with impure functions -----------
## Coefficient Plot ----
coef_df <- function(coef_mat) {
  ret <- coef_mat %>%
    gather(Tuning_Param, Coef, -c(1:2, ncol(coef_mat))) %>%
    mutate_at("Tuning_Param", get_integer) %>%
    mutate_if(is.factor, as.character) %>%
    as_tibble()
  return(ret)
}
coef_plot <- function(coef_error, ncomp = NULL, err_type = "prediction") {
  not_nested <- "coefficient_error" %in% class(coef_error)
  if (not_nested) coef_error <- list(coef_error)
  ncomp <- if (length(ncomp) == 1 & !is.null(ncomp)) 1:ncomp else 1:5
  method <- map_chr(coef_error, attr, "Method") %>% unique()
  is_shrinkage <- method %in% c("Ridge", "Lasso")
  if (!not_nested) n_rep <- length(coef_error)

  coef_mat <- map(coef_error, 'coefficients')
  coef <- map_df(coef_mat, coef_df, .id = "Replication")

  group_vars <- c("Predictor", "Response", "Est_type")

  if (is_shrinkage) {
    error_type <- switch(
      err_type,
      prediction = "prediction_error",
      estimation = "estimation_error")

    err_dta <- map_df(coef_error, error_type, .id = "Replication")
    names(err_dta)[2] <- "Error"

    tp <- err_dta %>%
      group_by(Replication, Response) %>%
      top_n(-10, wt = Error) %>%
      ungroup()

    dta <- coef %>%
      filter(Tuning_Param %in% unique(tp$Tuning_Param)) %>%
      mutate_at(vars(Predictor, Response), get_integer) %>%
      mutate(Predictor = ifelse(is.na(Predictor), 0, Predictor)) %>%
      mutate_at(vars(Predictor, Response), as.integer)

  } else {
    coef <- coef %>%
      mutate_at(vars(Predictor, Response), ~get_integer(.x)) %>%
      mutate(Predictor = ifelse(is.na(Predictor), 0, Predictor)) %>%
      mutate_at(vars(Predictor, Response, Tuning_Param), as.integer)

    group_vars <- append(group_vars, "Tuning_Param")
    dta <- coef %>% filter(Tuning_Param %in% ncomp)
  }

  dta <- dta %>%
    gather(Est_type, Est_value, True, Coef) %>%
    group_by_at(group_vars) %>%
    rename(Component = Tuning_Param)

  dta_avg <- dta %>%
    summarize_at("Est_value", mean) %>%
    ungroup() %>%
    mutate(Est_type = case_when(
      Est_type == "True" ~ "True",
      Est_type == "Coef" ~ "Estimated")
    )

  facet_form <- if (!is_shrinkage) {
    as.formula(Response ~ Component)
  } else {
    as.formula(Response ~ .)
  }

  prm <- attr(coef_error[[1]], "Sim_Properties")
  sub_title <- paste0("p:", prm$p, " gamma:", prm$gamma,
                      " eta:", prm$eta, " R2:", prm$R2)
  title <- paste("Method:", method)
  if (!not_nested) title <- paste(title, "Replicates:", n_rep)

  plt <- dta_avg %>%
    ggplot(aes(Predictor, Est_value, color = Est_type, group = Est_type)) +
    geom_line(aes(linetype = Est_type)) +
    geom_point(shape = 21, size = 0.7, color = "black", stroke = 0.5) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(family = "mono")) +
    labs(y = "Coefficient", color = NULL, linetype = NULL) +
    facet_grid(facet_form, labeller = label_both) +
    ggtitle(title, subtitle = sub_title) +
    scale_color_brewer(palette = "Set1")
  return(plt)
}
## Error Plot
err_plot <- function(coef_error, error_type = "Prediction", ncomp = NULL) {
  not_nested <- "coefficient_error" %in% class(coef_error)
  if (not_nested) coef_error <- list(coef_error)

  METHOD <- map_chr(coef_error, attr, "Method") %>% unique()
  is_shrinkage <- METHOD %in% c("Ridge", "Lasso")
  error_df <- map_df(coef_error, .id = "Replication",
                     paste(tolower(error_type), "error", sep = "_"))
  if (!is.null(ncomp) & !is_shrinkage) {
    if (length(ncomp) == 1) ncomp <- 1:ncomp
    error_df <- error_df %>% filter(Tuning_Param %in% ncomp)
  }
  dta <- error_df %>%
    mutate(Response = paste0("Y", Response))
  names(dta)[2] <- "Error"

  prms <- attr(coef_error[[1]], "Sim_Properties")
  lbls <- prms[c('p', 'eta', 'gamma', 'R2')]

  dgn_lbl <- with(lbls, paste0("p:", p, " eta:", eta, " gamma:", gamma, " R2:", R2))

  lbl <- dta %>%
    group_by(Response, Tuning_Param) %>%
    summarize_at(2, mean) %>%
    group_by(Response) %>%
    summarize(Tuning_Param = Tuning_Param[which.min(Error)],
              Error = min(Error)) %>%
    mutate(label = paste0(Response, " = ", round(Error, 3), " (", Tuning_Param, ")")) %>%
    arrange(Error)

  x_lab <- if (is_shrinkage) "Lambda" else "Number of Components"

  if (!is_shrinkage) {
    dta <- dta %>%
      mutate(Tuning_Param = as.factor(as.integer(Tuning_Param)))
  }

  plt <- dta %>%
    ggplot(aes(Tuning_Param, Error, fill = Response)) +
    stat_summary(fun.y = mean, geom = "line", size = 0.8,
                 aes(group = Response, color = Response))
  if (not_nested) {
    plt <- plt + stat_summary(fun.y = mean, geom = "point",  size = 1, shape = 21,
                 aes(group = Response, fill = Response),
                 stroke = 0.2)
  }
  if (!is_shrinkage) {
    plt <- plt +
      stat_summary(fun.y = mean, geom = "point",  size = 2, shape = 21,
                              aes(group = Response, fill = Response))
  }
  if (!not_nested & !is_shrinkage) {
    plt <- plt + geom_point(aes(color = Response),
               position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.2),
               alpha = 0.1) +
      geom_boxplot(alpha = 0.2, color = "grey70", size = 0.3)
  }
  plt <- plt + labs(x = x_lab, y = paste(error_type, "Error"),
         color = "Response", fill = "Response") +
    geom_text(data = lbl, aes(label = label, color = Response),
              x = Inf, y = Inf, hjust = 1, vjust = seq(2, 8, 2),
              family = "mono") +
    ggtitle(paste0(paste(error_type, "Error Plot:: "), "Method: ", METHOD),
            subtitle = dgn_lbl) +
    theme_light() +
    theme(plot.subtitle = element_text(family = "mono"),
          legend.position = "bottom")

  return(plt)
}

# ## ---- Very IMPURE Functions ----
# ## Coefficient Plot ----
# get_coef <- function(design, method, path = "scripts/robj/coef-error") {
#   out <- map_df(design, function(dgn){
#     fname <- paste0(file.path(path, "dgn-"), dgn, "-", tolower(method), ".Rdata")
#     load(fname)
#     is_shrinkage <- method %in% c("Ridge", "Lasso")
#     out <- map_df(out, function(x) {
#       ret <- x[["coefficients"]]
#       ret <- gather(ret, Tuning_Param, Coef, -c(1:2, ncol(ret)))
#       ret <- ret %>%
#         mutate_at("Tuning_Param", if (is_shrinkage) as.numeric else get_integer) %>%
#         mutate_if(is.factor, as.character)
#     }, .id = "Replication")
#     return(out)
#   }, .id = "Design")
#   return(as_tibble(out))
# }
# coef_plot <- function(design, method, ncomp = ifelse(method == "Yenv", 3, 7), 
#                       error_type = "prediction", path = 'scripts/robj/coef-error',
#                       design_chr = design_chr) {
#   ncomp <- if (length(ncomp) == 1) 1:ncomp
#   dgn <- as.character(design)
#   coef <- get_coef(dgn, method, path = path) %>%
#     mutate_at(vars(Predictor, Response), ~as.integer(get_integer(.x)))
# 
#   is_shrinkage <- method %in% c("Ridge", "Lasso")
#   group_vars <- c("Design", "Predictor", "Response", "Est_type")
# 
#   if (is_shrinkage) {
#     err_dta_chr <- switch(error_type, prediction = "pred_error", estimation = "est_error")
#     if (!exists(err_dta_chr)) stop("Load prediction and estimation error data frame first!")
#     err_dta <- get(err_dta_chr)
#     tp <- err_dta %>%
#       filter(Method == method, Design == dgn) %>%
#       group_by(Replication, Response) %>%
#       top_n(-1, wt = get(names(err_dta)[4])) %>%
#       ungroup() %>%
#       select(Tuning_Param) %>%
#       distinct() %>%
#       .[[1]]
#     dta <- coef %>%
#       filter(Tuning_Param %in% tp)
#   } else {
#     group_vars <- append(group_vars, "Tuning_Param")
#     dta <- coef %>%
#       filter(Tuning_Param %in% ncomp)
#   }
# 
#   dta <- dta %>%
#     gather(Est_type, Est_value, True, Coef) %>%
#     group_by_at(group_vars) %>%
#     rename(Component = Tuning_Param)
# 
#   dta_avg <- dta %>%
#     summarize_at("Est_value", mean) %>%
#     ungroup() %>%
#     mutate(Est_type = case_when(
#       Est_type == "True" ~ "True",
#       Est_type == "Coef" ~ "Estimated")
#     )
# 
#   facet_form <- if (!is_shrinkage) {
#     as.formula(Response ~ Component)
#   } else {
#     as.formula(Response ~ .)
#   }
#   sub_title <- with(design_chr %>% slice(as.numeric(design)), {
#     paste0("p: ", p, ", relpos: ", relpos,
#            ", gamma: ", gamma, ", eta: ", eta, ", R2: ", R2)
#   })
# 
#   plt <- dta_avg %>%
#     ggplot(aes(Predictor, Est_value, color = Est_type, group = Est_type)) +
#     geom_line() + geom_point(shape = 21, size = 0.7) +
#     theme(legend.position = "bottom",
#           plot.subtitle = element_text(family = "mono")) +
#     labs(y = "Coefficient", color = NULL) +
#     facet_grid(facet_form, labeller = label_both) +
#     ggtitle(paste("Coefficient plot:: ", paste("Design:", design, "Method:", method)),
#             sub_title)
#   return(plt)
# }
# 
# ## Error Plot ----
# ## Estimation Error Plot ----------------------
# get_err_plot <- function(design, method, flip_facet = FALSE) {
#   dgn <- as.character(design)
#   is_shrinkage <- tolower(method) %in% c("ridge", "lasso")
#   x_lab <- ifelse(!(is_shrinkage), "Number of Component", "Lambda")
#   dta <- est_error %>%
#     filter(Design == dgn, Method == method) %>%
#     left_join(pred_error %>% filter(Design == dgn, Method == method),
#               by = c("Design", "Method", "Replication", "Tuning_Param", "Response")) %>%
#     rename(Prediction = Pred_Error, Estimation = Est_Error) %>%
#     gather(Error_type, Error, Estimation, Prediction) %>%
#     mutate(Response = factor(Response))
#   group_vars <- c("Design", "Method", "Response", "Error_type")
#   if (!is_shrinkage) group_vars <- append(group_vars, "Tuning_Param")
#   dta_avg <- dta %>%
#     group_by(Design, Method, Response, Tuning_Param, Error_type) %>%
#     summarize(Error = mean(Error))
#   dta_min <- dta_avg %>%
#     group_by(Design, Method, Response, Error_type) %>%
#     filter(Error == min(Error)) %>%
#     arrange(Error) %>%
#     mutate(label = paste0("Y", Response, ": ", round(Error, 3)))
#   dgn_lbl <- design_chr %>% slice(design) %>%
#     select(p, eta, gamma, R2) %>% as.data.frame()
#   dgn_lbl <- paste(paste(names(dgn_lbl), dgn_lbl, sep = ": "), collapse = " ")
#   facet_formula <- if (flip_facet) as.formula(. ~ Error_type) else as.formula(Error_type ~ .)
#   v_just <- rep(seq(1, design_chr$m[design] * 2, 2), 2)
#   plt <- dta_avg %>%
#     ggplot(aes(Tuning_Param, Error, color = Response, group = Response)) +
#     facet_grid(facet_formula, scales = 'free_y') +
#     geom_line() +
#     labs(x = x_lab, color = "Response") +
#     theme(legend.position = "bottom",
#           plot.subtitle = element_text(family = "mono")) +
#     ggtitle(paste0("Estimation and Perediction Error: ", method),
#             subtitle = dgn_lbl)
#   if (!is_shrinkage) {
#     plt <- plt + geom_point()  +
#       scale_x_continuous(breaks = 0:10)
#   }
#   plt <- plt +
#     geom_text(data = dta_min, x = Inf, y = Inf, show.legend = FALSE,
#               aes(label = label, color = Response),
#               inherit.aes = FALSE, hjust = 1, vjust = v_just) +
#     geom_point(data = dta_min, fill = "white", shape = 21)
#   return(plt)
# }
# ## Effect Plot ----
# eff_plot <- function(term, model, title = "Prediction Error", show_errorbar = FALSE){
#   trms <- strsplit(term, ":")[[1]]
#   order <- length(trms)
#   possible_terms <- unlist(combinat::permn(trms, fun = paste0, collapse = ":"))
#   term_labels <- attr(model$terms, "term.labels")
#   term <- possible_terms[possible_terms %in% term_labels]
#   eff_df <- map_df(suppressMessages(effects::effect(term, model)),
#                    as.data.frame, .id = "Response") %>%
#     as.tibble()
#   if ("Method" %in% trms) {
#     mthd_idx <- which(trms %in% "Method")
#     trms <- c(trms[mthd_idx], trms[-mthd_idx])
#   }
#   facet_formula <- if (length(trms) == 3) {
#     paste(c("Response", trms[-c(1:2)]), collapse = " ~ ")
#   } else if (length(trms) == 2) {
#     paste(c(".", c("Response", trms[-c(1:2)])), collapse = " ~ ")
#   } else {
#     paste(c("Response", paste(trms[-c(1:2)], collapse = " + ")), collapse = " ~ ")
#   }
#   sub_title <- as.character(model$call)[2]
#   plt <- eff_df %>%
#     ggplot(aes(reorder(get(trms[1]), fit), fit,
#                color = get(trms[2]), group = get(trms[2]))) +
#     geom_line() + geom_point(size = 0.8) +
#     theme(legend.position = "bottom",
#           plot.subtitle = element_text(family = "mono")) +
#     labs(x = trms[1], y = "Effect", color = trms[2]) +
#     ggtitle(paste("Effect Plot:", title), subtitle = sub_title)
# 
#   if (show_errorbar) plt <- plt +
#     geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.1))
#   if (length(trms) > 1) plt <- plt +
#     facet_grid(facet_formula, labeller = label_both)
#   if ("Method" %in% trms) plt <- plt +
#     theme(axis.text.x = element_text(angle = 30, hjust = 1))
# 
#   return(plt)
# }
