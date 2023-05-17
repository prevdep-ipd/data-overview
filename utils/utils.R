# # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                 #
#     UTILITY FUNCTIONS                           #
#                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # #



# getIPD ----------------------------------------------------------------------

# This function requires the study shorthand (study), i.e. "buntrock2015", and
# a Github personal access token (ask Mathias)

getIPD = function(study, token){
  
  # Requirements
  require(gh)
  require(base64enc)
  require(jsonlite)
  require(readr)
  require(magrittr)
  
  # Define path
  ghBase = "GET https://api.github.com/repos/prevdep-ipd/data/contents/tidy/"
  suffix = "/data.json"
  path = paste0(ghBase, study, suffix)
  
  # Send API request
  req = try({gh::gh(path, 
                    .token = token,
                    .accept = "application/vnd.github.raw")}, 
            silent = TRUE)
  
  if (identical(class(req)[1], "try-error"))
    stop(study, " could not be retrieved from Github. Either the entry does not exist, ",
         "or the personal access token is not valid.")
  
  # Make pretty
  req %>% 
    rawToChar() %>% jsonlite::fromJSON() %>% 
    apply(2, function(x){
      suppressWarnings({
        readr::parse_guess(x, locale = readr::locale(decimal_mark = "."))
      }) -> col
      ifelse(is.null(attr(col, "problems")), return(col), return(x))
    }, simplify = FALSE) %>%
    as.data.frame() -> dataClean
  
  return(dataClean)
  
}


# getIPDInfo ------------------------------------------------------------------

# This function requires the metadata key, i.e. "rob", and
# a Github personal access token (ask Mathias)

getIPDInfo = function(key, token){
  
  # Requirements
  require(gh)
  require(base64enc)
  require(jsonlite)
  require(readr)
  require(magrittr)
  
  # Define path
  ghBase = "GET https://api.github.com/repos/prevdep-ipd/data/contents/tidy/"
  suffix = "/metadata.json"
  
  # Send API request to get available folders
  req = try({gh::gh(ghBase, .token = token)}, silent = TRUE)
  
  # Get shorthands
  shorthands = vapply(req, "[[", "", "name")
  rm(req)
  
  # Get specific metadata
  paths = paste0(ghBase, shorthands, suffix)
  lapply(as.list(paths), function(x){
    try({gh::gh(x, .token = token)}, silent = TRUE) %>% 
      {base64enc::base64decode(.$content)} %>% 
      rawToChar() %>% jsonlite::fromJSON() %>% 
      {list(author = .[["author"]], key = .[[key]])}
  }) %>% 
    {x = lapply(., function(y) y$key);
     names(x) = lapply(., function(y) y$author); x} %>%  
    return()
  
}

# getIPDStudies ---------------------------------------------------------------

getIPDStudies = function(token){
  
  # Requirements
  require(gh)
  require(base64enc)
  require(jsonlite)
  require(readr)
  require(magrittr)
  
  # Define path
  ghBase = "GET https://api.github.com/repos/prevdep-ipd/data/contents/tidy/"
  
  # Send API request to get available folders
  req = try({gh::gh(ghBase, .token = token)}, silent = TRUE)
  shorthands = vapply(req, "[[", "", "name")
  
  return(shorthands)
  
}


# getIPDKeys ------------------------------------------------------------------

# This function requires a Github personal access token (ask Mathias)

getIPDKeys = function(token){
  
  # Requirements
  require(gh)
  require(base64enc)
  require(jsonlite)
  require(readr)
  require(magrittr)
  require(tibble)
  
  # Define path
  ghBase = "GET https://api.github.com/repos/prevdep-ipd/data/contents/tidy/"
  suffix = "/metadata.json"
  
  # Send API request to get available folders
  req = try({gh::gh(ghBase, .token = token)}, silent = TRUE)
  
  # Get shorthands
  shorthands = vapply(req, "[[", "", "name")
  rm(req)
  
  # Get specific metadata
  paths = paste0(ghBase, shorthands, suffix)
  lapply(as.list(paths), function(x){
    try({gh::gh(x, .token = token)}, silent = TRUE) %>% 
      {base64enc::base64decode(.$content)} %>% 
      rawToChar() %>% jsonlite::fromJSON() %>% 
      {list(author = .[["author"]], keys = names(.))}
  }) %>% 
    {x = lapply(., function(y) y$key);
    names(x) = lapply(., function(y) y$author); x} %>%  
    return()

}

extract.facs = function(x){
  
  reslist = list()
  for (i in 1:length(x)){
    
    name = x[[i]]$descript
    value = as.character(x[[i]]$values$value)
    freq = x[[i]]$values$frequency
    prop = freq/sum(freq)
    
    reslist[[i]] = data.frame(name, value, freq, prop)
    
  }
  
  res = do.call(rbind, reslist)
  return(res)
  
}



eff.extractor = function(m, bgroup.name = "group1", model.name = "model"){
  
  m = mitml::testEstimates(m, var.comp = T)
  est = m$estimates
  
  return(
    data.frame(
      name = model.name,
      b = est[bgroup.name, "Estimate"],
      b.lower = confint(m)[bgroup.name, 1],
      b.upper = confint(m)[bgroup.name, 2],
      t = est[bgroup.name, "t.value"],
      p = est[bgroup.name, 5],
      tau.intercept = m$var.comp[1],
      tau.slope = m$var.comp[3]))
  
}


mod.extractor = function(m, mod.name = "age", model.name = "model"){
  
  m = mitml::testEstimates(m, var.comp = T)
  est = m$estimates
  rows = rownames(est)[grep(mod.name, rownames(est))]
  
  return(
    data.frame(
      name = model.name,
      b = est[rows, "Estimate"],
      b.lower = confint(m)[rows, 1],
      b.upper = confint(m)[rows, 2],
      t = est[rows, "t.value"],
      p = est[rows, 5],
      tau.intercept = m$var.comp[1],
      tau.slope = m$var.comp[3]))
  
}


# Reliable Change Index
# Define RCI function
rci = function(pre, post, data, rtt, sdNorm){
  if ("rci" %in% names(data)) {warning("variable rci already exists in data frame and will be overwritten")}
  difference <- data[,post]-data[,pre]
  Serr <- sdNorm*sqrt(1-rtt)
  Sdiff <- sqrt((2*Serr^2))
  rci <- difference/Sdiff
  return(rci)
}


# CESD Common Metrics backtransformation
# load("./utils/cm.cesd.rda")
# trans.cesd = approxfun(y = cm.cesd$Sum.Scores, x = cm.cesd$Theta, method = "linear", rule = 2)


# Backtransformation of factor dummy vars
backtrans.dummy = function(varname, dat){

  varname = varname
  dat = dat

  # Select dummy columns
  cols = dat %>% dplyr::select(grep(varname, colnames(.))) %>% 
    map_dfr(function(x) as.numeric(x)-1)

  # Calculate zero class variable and order
  cols[[paste0(varname, ".0")]] = 1 - rowSums(cols)
  cols = cols[,order(colnames(cols))]

  # Get levels from variable names
  levs = unlist(strsplit(colnames(cols), '[.]'))[2 * (1:ncol(cols))]

  # Merge
  res = factor(as.matrix(as.data.frame(cols)) %*% 1:ncol(cols))

  levels(res) = as.numeric(levels(res))-1

  return(res)

}


# Coord Radar
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}



# Accuracy Cutoff CV
acc_cutoff = function(y, ypred){
  
  n = length(y)
  y = as.numeric(as.character(y))
  ypred = as.numeric(as.character(ypred))
  
  res = list()
  for (i in 1:100/100){
    
    x = i*100
    acc = sum(ifelse(ypred > i, 1, 0) == y)/n
    
    res[[x]] = data.frame(thresh = i,
                          accuracy = acc)
    
  }
  
  res.df = do.call(rbind, res)
  res.df = res.df[order(-res.df$accuracy),]
  
  return(res.df)
  
}


CV.acc_cutoff = function(model, X, y, k = 10){
  
  require(dplyr)
  
  if(class(sl_l) != "SuperLearner"){
    stop("class of model must be 'SuperLearner'")
  }
  
  # Define split folds
  ksize = ceiling(nrow(X)*(1/k))
  fold = sample(rep(1:k, times = ksize))[1:nrow(X)]
  
  cv.list = list()
  # Define CV loop
  for (i in 1:k){
    
    data = X[fold != i, ]
    val_dat = X[fold == i, ]
    y_i = y[fold != i]
    y_e = y[fold == i]
    
    # Get internal estimate
    ypred_i = SuperLearner::predict.SuperLearner(model, newdata = data)$pred %>% as.numeric()
    thresh = acc_cutoff(y_i, ypred_i)[1,1]
    acc_internal = acc_cutoff(y_i, ypred_i)[1,2]
    
    # Get external estimate
    ypred_e = SuperLearner::predict.SuperLearner(model, newdata = val_dat)$pred %>% as.numeric()
    acc_external = sum(ifelse(ypred_e > thresh, 1, 0) == y_e)/length(ypred_e)
    
    cv.list[[i]] = data.frame(fold = i,
                              thresh, acc_internal, acc_external)
    
  }
  
  cv.runs = do.call(rbind, cv.list)
  
  return(list(cv.runs = cv.runs,
              results = data.frame(tresh = mean(cv.runs$thresh),
                                   tresh.sd = sd(cv.runs$thresh),
                                   acc_internal = mean(cv.runs$acc_internal),
                                   acc_internal.sd = sd(cv.runs$acc_internal),
                                   acc_external = mean(cv.runs$acc_external),
                                   acc_external.sd = sd(cv.runs$acc_external))))
  
}


# Function for iml
f_pred = function(model, newdata){
  
  preds = SuperLearner::predict.SuperLearner(model, 
                                             newdata = newdata, 
                                             onlySL = TRUE)
  return(data.frame("response" = as.numeric(preds$pred),
                    "nonresponse" = 1-as.numeric(preds$pred)))
  
}


# Function to extract descriptives
skimReport = function(data, round = FALSE){
  
  require(skimr)
  require(dplyr)
  require(purrr)
  x = skim(data)
  N = nrow(data)
  
  # Summarize Factors
  with(x, {
    skim_type == "factor" -> fac.mask
    vars = skim_variable[fac.mask]
    n.unique = factor.n_unique[fac.mask]
    strsplit(factor.top_counts, "\\, |\\:")[fac.mask] %>% 
      purrr::map(function(x){
        as.numeric(x) -> x
        data.frame(category = x[seq(1, length(x), by = 2)],
                   count = x[seq(2, length(x), by = 2)]) %>% 
          dplyr::mutate(percent = count/N)
      }) %>% 
      {names(.) = vars;.} %>% 
      map_df(~as.data.frame(.), .id = "variable")
  }) -> factors
  
  # Summarize Numeric
  with(x, {
    skim_type == "numeric" -> num.mask
    data.frame(variable = skim_variable[num.mask],
               mean = numeric.mean[num.mask],
               sd = numeric.sd[num.mask],
               n = N-x$n_missing[num.mask],
               n.missing = n_missing[num.mask],
               perc.missing = n_missing[num.mask]/N)
  }) -> numerics
  
  if (round == TRUE){
    within(factors, {
      percent = round(percent*100, 2)
    }) -> factors
    within(numerics, {
      mean = round(mean, 2)
      sd = round(sd, 2)
      perc.missing = round(perc.missing*100, 2)
    }) -> numerics
  }
  
  
  dat = list(factors = factors,
             numerics = numerics)
  class(dat) = c("list", "skimReport")
  
  return(dat)
  
}

print.skimReport = function(x){
  cat("Categorial Variables \n ------------------ \n")
  cat("\n")
  print(x$factors)
  cat("\n")
  cat("Numeric Variables \n ------------------ \n")
  cat("\n")
  print(x$numerics)
} 


# Find unidentified cases and structural zeros (imputation)
find.unidentified = function(label, data, use.glm=TRUE, strict.glm=use.glm, tol=1e-5){
  # label = name of variable in data (character)
  # data = data set (data.frame)
  
  data <- data[!is.na(data[,label]),]
  preds <- setdiff(colnames(data),label)
  is.cat <- is.factor(data[,label])
  
  # structural zeros
  z <- colSums(data[,preds]) == 0
  
  fml <- formula(paste(label, "~", paste(preds, collapse="+")))
  if(use.glm){
    fit <- glm(fml, data=data, family=if(is.cat) binomial() else gaussian())
  }else{
    if(is.cat) data[,label] <- as.numeric(data[,label])-1
    fit <- lm(fml, data=data)
  }
  # linear depencies without zeros
  na <- is.na(coef(fit)[-1])
  ld <- na & !z
  if(is.cat && strict.glm){ # filter cor. effects in GLMs not showing in LM
    crm <- cov2cor(vcov(fit)[-1,-1][!na,!na])
    ind <- lower.tri(crm) & (abs(crm) > 1-tol)
    ld.cat <- rowSums(ind) > 0
  }else{
    ld.cat <- NULL
  }
  if(any(ld.cat)) ld[which(ld.cat)] <- TRUE
  
  # get names
  out <- names(z)[z|ld]
  if(any(z)) attr(out, "struct.zero") <- names(z)[z]
  if(any(ld.cat)) attr(out, "perfect.cor.glm") <- names(ld.cat)[ld.cat]
  return(out)
  
}


extract_percs = function(x){
  
  x = x
  
  # Define total Ns
  ngroup = c(implist[[1]][implist[[1]]$ipd_group == 1,] %>% nrow() %>% rep(2),
             implist[[1]][implist[[1]]$ipd_group == 2,] %>% nrow() %>% rep(2))
  ntotal = implist[[1]] %>% nrow() %>% rep(4)
  
  # Extract Ns & Calulcate Percentages
  implist %>% 
    map(~ group_by(., ipd_group, !!sym(x)) %>% 
          dplyr::summarise(n = n()) %>%
          as.data.frame() %>% 
          mutate_if(is.factor, as.numeric)) %>% 
    purrr::reduce(`+`)/length(implist) -> distribution
  
  distribution %>% 
    mutate(n.perc.group = n/ngroup,
           n.perc.total = n/ntotal) -> distribution
  
  return(distribution)
  
}

