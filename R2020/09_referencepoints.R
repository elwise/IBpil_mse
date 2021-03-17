function (fit, bio.years = c(-5, -1) + FLCore::dims(fit$stk)$maxyear, 
          bio.const = FALSE, sel.years = c(-5, -1) + FLCore::dims(fit$stk)$maxyear, 
          sel.const = FALSE, Fscan = seq(0, 2, len = 40), Fcv = 0, 
          Fphi = 0, SSBcv = 0, rhologRec = TRUE, Blim, Bpa, recruitment.trim = c(3, 
                                                                                 -3), Btrigger = 0, Nrun = 200, process.error = TRUE, 
          verbose = TRUE, extreme.trim = c(0, 1), R.initial = mean(fit$rby$rec), 
          keep.sims = FALSE) 
{
  if (abs(Fphi) >= 1) 
    stop("Fphi, the autocorelation parameter for log F should be between (-1, 1)")
  if (diff(recruitment.trim) > 0) 
    stop("recruitment truncation must be given as c(high, low)")
  if (verbose) 
    icesTAF::msg("Setting up...")
  btyr1 <- bio.years[1]
  btyr2 <- bio.years[2]
  slyr1 <- sel.years[1]
  slyr2 <- sel.years[2]
  keep <- min(Nrun, 50)
  SR <- fit$sr.sto
  data <- fit$rby[, c("rec", "ssb", "year")]
  stk <- fit$stk
  stk.win <- FLCore::window(stk, start = btyr1, end = btyr2)
  stk.winsel <- FLCore::window(stk, start = slyr1, end = slyr2)
  littleHelper <- function(x, i) {
    x2 <- x
    x2[i] <- NA
    x2[] <- apply(x2, 1, mean, na.rm = TRUE)
    x[i] <- x2[i]
    return(x)
  }
  west <- matrix(FLCore::stock.wt(stk.win), ncol = btyr2 - 
                   btyr1 + 1)
  i <- west == 0
  if (any(i)) 
    west <- littleHelper(west, i)
  weca <- matrix(FLCore::catch.wt(stk.win), ncol = btyr2 - 
                   btyr1 + 1)
  i <- weca == 0
  if (any(i)) 
    weca <- littleHelper(weca, i)
  wela <- matrix(FLCore::landings.wt(stk.win), ncol = btyr2 - 
                   btyr1 + 1)
  if (any(i)) 
    wela <- littleHelper(wela, i)
  Mat <- matrix(FLCore::mat(stk.win), ncol = btyr2 - btyr1 + 
                  1)
  M <- matrix(FLCore::m(stk.win), ncol = btyr2 - btyr1 + 1)
  landings <- matrix(FLCore::landings.n(stk.winsel), ncol = slyr2 - 
                       slyr1 + 1)
  catch <- matrix(FLCore::catch.n(stk.winsel), ncol = slyr2 - 
                    slyr1 + 1)
  sel <- matrix(FLCore::harvest(stk.winsel), ncol = slyr2 - 
                  slyr1 + 1)
  Fbar <- matrix(FLCore::fbar(stk.winsel), ncol = slyr2 - slyr1 + 
                   1)
  sel <- sweep(sel, 2, Fbar, "/")
  if (sel.const == TRUE) {
    sel[] <- apply(sel, 1, mean)
    landings[] <- apply(landings, 1, mean)
    catch[] <- apply(catch, 1, mean)
  }
  if (bio.const == TRUE) {
    west[] <- apply(west, 1, mean)
    weca[] <- apply(weca, 1, mean)
    wela[] <- apply(wela, 1, mean)
    Mat[] <- apply(Mat, 1, mean)
    M[] <- apply(M, 1, mean)
  }
  land.cat = landings/catch
  i <- is.na(land.cat)
  if (any(i)) 
    land.cat[i] <- 1
  Fprop <- apply(FLCore::harvest.spwn(stk.winsel), 1, mean)[drop = TRUE]
  Mprop <- apply(FLCore::m.spwn(stk.win), 1, mean)[drop = TRUE]
  Nmod <- nrow(SR)
  NF <- length(Fscan)
  ages <- FLCore::dims(stk)$age
  ssb_lag <- fit$rby$ssb_lag[1]
  ssby <- Ferr <- array(0, c(Nrun, Nmod), dimnames = list(year = 1:Nrun, 
                                                          iter = 1:Nmod))
  Ny <- Fy <- WSy <- WCy <- Cy <- Wy <- Wl <- Ry <- array(0, 
                                                          c(ages, Nrun, Nmod), dimnames = list(age = (range(stk)[1]:range(stk)[2]), 
                                                                                               year = 1:Nrun, iter = 1:Nmod))
  Ferr[1, ] <- stats::rnorm(n = Nmod, mean = 0, sd = 1) * Fcv/sqrt(1 - 
                                                                     Fphi^2)
  for (j in 2:Nrun) Ferr[j, ] <- Fphi * Ferr[j - 1, ] + Fcv * 
    stats::rnorm(n = Nmod, mean = 0, sd = 1)
  SSBerr <- matrix(stats::rnorm(n = Nrun * Nmod, mean = 0, 
                                sd = 1), ncol = Nmod) * SSBcv
  rsam <- array(sample(1:ncol(weca), Nrun * Nmod, TRUE), c(Nrun, 
                                                           Nmod))
  rsamsel <- array(sample(1:ncol(sel), Nrun * Nmod, TRUE), 
                   c(Nrun, Nmod))
  Wy[] <- c(weca[, c(rsam)])
  Wl[] <- c(wela[, c(rsam)])
  Ry[] <- c(land.cat[, c(rsamsel)])
  R <- R.initial
  ssbs <- cats <- lans <- recs <- array(0, c(7, NF))
  ferr <- ssbsa <- catsa <- lansa <- recsa <- array(0, c(NF, 
                                                         keep, Nmod))
  begin <- Nrun - keep + 1
  resids = array(stats::rnorm(Nmod * (Nrun + 1), 0, SR$cv), 
                 c(Nmod, Nrun + 1))
  if (rhologRec == TRUE) {
    fittedlogRec <- do.call(cbind, lapply(c(1:nrow(fit$sr.sto)), 
                                          function(i) {
                                            FUN <- match.fun(fit$sr.sto$model[i])
                                            FUN(fit$sr.sto[i, ], fit$rby$ssb)
                                          }))
    rhologRec <- apply(log(fit$rby$rec) - fittedlogRec, 2, 
                       function(x) {
                         stats::cor(x[-length(x)], x[-1])
                       })
  }
  if (is.numeric(rhologRec)) {
    for (j in 2:(Nrun + 1)) {
      resids[, j] <- rhologRec * resids[, j - 1] + resids[, 
                                                          j] * sqrt(1 - rhologRec^2)
    }
  }
  lims = t(array(SR$cv, c(Nmod, 2))) * recruitment.trim
  for (k in 1:Nmod) {
    resids[k, resids[k, ] > lims[1, k]] = lims[1, k]
  }
  for (k in 1:Nmod) {
    resids[k, resids[k, ] < lims[2, k]] = lims[2, k]
  }
  if (verbose) 
    icesTAF::msg("Running forward simulations.")
  if (verbose) 
    loader(0)
  for (i in 1:NF) {
    Fbar <- Fscan[i]
    Zpre <- Fbar * sel[, rsamsel[1, ]] * Fprop + M[, rsam[1, 
                                                          ]] * Mprop
    Zpos <- Fbar * (1 - Fprop) * sel[, rsamsel[1, ]] + M[, 
                                                         rsam[1, ]] * (1 - Mprop)
    Ztot <- Fbar * sel[c(1:ages, rep(ages, 49 - ages)), rsamsel[1, 
                                                                ]] + M[c(1:ages, rep(ages, 49 - ages)), rsam[1, ]]
    Zcum <- apply(Ztot, 2, function(x) c(0, cumsum(x)))
    N1 <- R * exp(-unname(Zcum))
    Ny[, 1, ] <- rbind(N1[1:(ages - 1), ], colSums(N1[ages:50, 
                                                      ]))
    ssby[1, ] <- colSums(Mat[, rsam[1, ]] * Ny[, 1, ] * west[, 
                                                             rsam[1, ]]/exp(Zpre))
    for (j in 2:pmax(2, ssb_lag)) {
      Ny[, j, ] <- rbind(N1[1:(ages - 1), ], colSums(N1[ages:50, 
                                                        ]))
      ssby[j, ] <- colSums(Mat[, rsam[j - 1, ]] * Ny[, 
                                                     1, ] * west[, rsam[j - 1, ]]/exp(Zpre))
    }
    for (j in (2 + ssb_lag):Nrun) {
      Fnext <- Fbar * pmin(1, ssby[j - 1, ] * exp(SSBerr[j - 
                                                           1, ])/Btrigger)
      Fnext <- exp(Ferr[j, ]) * Fnext
      Zpre <- rep(Fnext, each = length(Fprop)) * Fprop * 
        sel[, rsamsel[j - 1, ]] + M[, rsam[j - 1, ]] * 
        Mprop
      Fy[, j - 1, ] <- rep(Fnext, each = ages) * sel[, 
                                                     rsamsel[j - 1, ]]
      Ny[-1, j, ] <- Ny[1:(ages - 1), j - 1, ] * exp(-Fy[1:(ages - 
                                                              1), j - 1, ] - M[1:(ages - 1), rsam[j - 1, ]])
      Ny[ages, j, ] <- Ny[ages, j, ] + Ny[ages, j - 1, 
                                          ] * exp(-Fy[ages, j - 1, ] - M[ages, rsam[j - 
                                                                                      1, ]])
      if (ssb_lag == 0) {
        ssby[j, ] <- apply(array(Mat[, rsam[j, ]] * Ny[, 
                                                       j, ] * west[, rsam[j, ]]/exp(Zpre), c(ages, 
                                                                                             Nmod)), 2, sum)
      }
      SSBforRec <- ssby[j - ssb_lag, ]
      if (process.error) {
        allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR, 
                                                                           SSBforRec) + resids[, j]))
      }
      else {
        allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR, 
                                                                           SSBforRec)))
      }
      select <- cbind(seq(Nmod), as.numeric(factor(SR$mod, 
                                                   levels = unique(SR$mod))))
      Ny[1, j, ] <- allrecs[select]
      ssby[j, ] <- apply(array(Mat[, rsam[j, ]] * Ny[, 
                                                     j, ] * west[, rsam[j, ]]/exp(Zpre), c(ages, Nmod)), 
                         2, sum)
      Cy[, j, ] <- Ny[, j - 1, ] * Fy[, j - 1, ]/(Fy[, 
                                                     j - 1, ] + M[, rsam[j - 1, ]]) * (1 - exp(-Fy[, 
                                                                                                   j - 1, ] - M[, rsam[j - 1, ]]))
    }
    Cw <- Cy * Wy
    land <- Cy * Ry * Wl
    Lan <- apply(land, 2:3, sum)
    Cat <- apply(Cw, 2:3, sum)
    quants <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
    ssbs[, i] <- stats::quantile(ssby[begin:Nrun, ], quants)
    cats[, i] <- stats::quantile(Cat[begin:Nrun, ], quants)
    lans[, i] <- stats::quantile(Lan[begin:Nrun, ], quants)
    recs[, i] <- stats::quantile(Ny[1, begin:Nrun, ], quants)
    ferr[i, , ] <- Ferr[begin:Nrun, ]
    ssbsa[i, , ] <- ssby[begin:Nrun, ]
    catsa[i, , ] <- Cat[begin:Nrun, ]
    lansa[i, , ] <- Lan[begin:Nrun, ]
    recsa[i, , ] <- Ny[1, begin:Nrun, ]
    if (verbose) 
      loader(i/NF)
  }
  if (verbose) 
    icesTAF::msg("Summarising simulations")
  dimnames(ssbs) <- dimnames(cats) <- dimnames(lans) <- dimnames(recs) <- list(quants = c("p025", 
                                                                                          "p05", "p25", "p50", "p75", "p95", 
                                                                                          "p975"), fmort = Fscan)
  rbp2dataframe <- function(x, variable) {
    x <- data.frame(t(x))
    x$variable <- variable
    x$Ftarget <- as.numeric(row.names(x))
    rownames(x) <- NULL
    return(x)
  }
  rbp <- rbind(rbp2dataframe(recs, "Recruitment"), rbp2dataframe(ssbs, 
                                                                 "Spawning stock biomass"), rbp2dataframe(cats, 
                                                                                                          "Catch"), rbp2dataframe(lans, "Landings"))
  rbp <- rbp[, c(9, 8, 1:7)]
  FCrash05 <- Fscan[which.max(cats[2, ]):NF][which(cats[2, 
                                                        which.max(cats[2, ]):NF] < 0.05 * max(cats[2, ]))[1]]
  FCrash50 <- Fscan[which.max(cats[4, ]):NF][which(cats[4, 
                                                        which.max(cats[4, ]):NF] < 0.05 * max(cats[4, ]))[1]]
  if (missing(extreme.trim)) {
    catm <- apply(catsa, 1, mean)
    lanm <- apply(lansa, 1, mean)
  }
  else {
    catm <- apply(catsa, 1, function(x) {
      mean(x[x <= stats::quantile(x, extreme.trim[2]) & 
               x >= stats::quantile(x, extreme.trim[1])])
    })
    lanm <- apply(lansa, 1, function(x) {
      mean(x[x <= stats::quantile(x, extreme.trim[2]) & 
               x >= stats::quantile(x, extreme.trim[1])])
    })
  }
  maxcatm <- which.max(catm)
  maxlanm <- which.max(lanm)
  rbp$Mean <- NA
  rbp$Mean[rbp$variable == "Catch"] <- catm
  rbp$Mean[rbp$variable == "Landings"] <- lanm
  catsam <- apply(catsa, c(1, 3), mean)
  lansam <- apply(lansa, c(1, 3), mean)
  maxpf <- apply(catsam, 2, which.max)
  maxpfl <- apply(lansam, 2, which.max)
  FmsyLan <- Fscan[maxpfl]
  msymLan <- mean(FmsyLan)
  vcumLan <- stats::median(FmsyLan)
  fmsy.densLan <- stats::density(FmsyLan)
  vmodeLan <- fmsy.densLan$x[which.max(fmsy.densLan$y)]
  FmsyCat <- Fscan[maxpf]
  msymCat <- mean(FmsyCat)
  vcumCat <- stats::median(FmsyCat)
  fmsy.densCat <- stats::density(FmsyCat)
  vmodeCat <- fmsy.densCat$x[which.max(fmsy.densCat$y)]
  pFmsyCat <- data.frame(Ftarget = fmsy.densCat$x, value = cumsum(fmsy.densCat$y * 
                                                                    diff(fmsy.densCat$x)[1]), variable = "pFmsyCatch")
  pFmsyLan <- data.frame(Ftarget = fmsy.densLan$x, value = cumsum(fmsy.densLan$y * 
                                                                    diff(fmsy.densLan$x)[1]), variable = "pFmsyLandings")
  pProfile <- rbind(pFmsyCat, pFmsyLan)
  if (!missing(Blim)) {
    pBlim <- apply(ssbsa > Blim, 1, mean)
    i <- max(which(pBlim > 0.95))
    grad <- diff(Fscan[i + 0:1])/diff(pBlim[i + 0:1])
    flim <- Fscan[i] + grad * (0.95 - pBlim[i])
    i <- max(which(pBlim > 0.9))
    grad <- diff(Fscan[i + 0:1])/diff(pBlim[i + 0:1])
    flim10 <- Fscan[i] + grad * (0.9 - pBlim[i])
    i <- max(which(pBlim > 0.5))
    grad <- diff(Fscan[i + 0:1])/diff(pBlim[i + 0:1])
    flim50 <- Fscan[i] + grad * (0.5 - pBlim[i])
    pBlim <- data.frame(Ftarget = Fscan, value = 1 - pBlim, 
                        variable = "Blim")
    pProfile <- rbind(pProfile, pBlim)
  }
  else {
    flim <- flim10 <- flim50 <- Blim <- NA
  }
  if (!missing(Bpa)) {
    pBpa <- apply(ssbsa > Bpa, 1, mean)
    pBpa <- data.frame(Ftarget = Fscan, value = 1 - pBpa, 
                       variable = "Bpa")
    pProfile <- rbind(pProfile, pBpa)
  }
  else {
    Bpa <- NA
  }
  catF <- c(flim, flim10, flim50, vcumCat, Fscan[maxcatm], 
            FCrash05, FCrash50)
  lanF <- c(NA, NA, NA, vcumLan, Fscan[maxlanm], NA, NA)
  catC <- stats::approx(Fscan, cats[4, ], xout = catF)$y
  lanC <- stats::approx(Fscan, lans[4, ], xout = lanF)$y
  catB <- stats::approx(Fscan, ssbs[4, ], xout = catF)$y
  lanB <- stats::approx(Fscan, ssbs[4, ], xout = lanF)$y
  Refs <- rbind(catF, lanF, catC, lanC, catB, lanB)
  rownames(Refs) <- c("catF", "lanF", "catch", 
                      "landings", "catB", "lanB")
  colnames(Refs) <- c("F05", "F10", "F50", 
                      "medianMSY", "meanMSY", "FCrash05", 
                      "FCrash50")
  auxi <- stats::approx(Fscan, cats[4, ], xout = seq(min(Fscan), 
                                                     max(Fscan), length = 200))
  FmsyMedianC <- auxi$x[which.max(auxi$y)]
  MSYMedianC <- max(auxi$y)
  FmsylowerMedianC <- auxi$x[min((1:length(auxi$y))[auxi$y/MSYMedianC >= 
                                                      0.95])]
  FmsyupperMedianC <- auxi$x[max((1:length(auxi$y))[auxi$y/MSYMedianC >= 
                                                      0.95])]
  auxi <- stats::approx(Fscan, lans[4, ], xout = seq(min(Fscan), 
                                                     max(Fscan), length = 200))
  FmsyMedianL <- auxi$x[which.max(auxi$y)]
  MSYMedianL <- max(auxi$y)
  FmsylowerMedianL <- auxi$x[min((1:length(auxi$y))[auxi$y/MSYMedianL >= 
                                                      0.95])]
  FmsyupperMedianL <- auxi$x[max((1:length(auxi$y))[auxi$y/MSYMedianL >= 
                                                      0.95])]
  F5percRiskBlim <- flim
  refs_interval <- data.frame(FmsyMedianC = FmsyMedianC, FmsylowerMedianC = FmsylowerMedianC, 
                              FmsyupperMedianC = FmsyupperMedianC, FmsyMedianL = FmsyMedianL, 
                              FmsylowerMedianL = FmsylowerMedianL, FmsyupperMedianL = FmsyupperMedianL, 
                              F5percRiskBlim = F5percRiskBlim, Btrigger = Btrigger)
  sim <- list(ibya = list(Mat = Mat, M = M, Fprop = Fprop, 
                          Mprop = Mprop, west = west, weca = weca, sel = sel), 
              rbya = list(ferr = ferr), rby = fit$rby, rbp = rbp, Blim = Blim, 
              Bpa = Bpa, Refs = Refs, pProfile = pProfile, id.sim = fit$id.sr, 
              refs_interval = refs_interval, rhologRec = rhologRec)
  if (verbose) 
    icesTAF::msg("Calculating MSY range values")
  sim <- eqsim_range(sim)
  return(sim)
}