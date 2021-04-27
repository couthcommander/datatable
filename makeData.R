library(data.table)

makeData <- function(n = 10, seed = 30) {
  set.seed(seed)
  n <- 10
  n_visits <- rpois(n, 4)
  n_labs <- rpois(n, 5)
  n_surgeries <- rpois(n, 1)
  n_rxfills <- rpois(n, 8)
  n_days <- 365.25 * 4

  incident <- data.table(
    uid = rep(seq(n), each = 2),
    incident_date = as.Date(sample(365.25*4, n * 2, replace = TRUE), origin = '2015-12-31')
  )

  demo <- data.table(
    uid = sample(n),
    age = round(runif(n, 35, 65)),
    race = sample(c('white', 'non-white'), n, replace = TRUE),
    sex = sample(c('male', 'female', NA), n, replace = TRUE)
  )

  visit_dat <- data.table(
    uid = rep(seq(n), times = n_visits),
    event_date = as.Date(sample(n_days, sum(n_visits), replace = TRUE), origin = '2015-12-31'),
    key = c('uid', 'event_date')
  )

  lab_dat <- data.table(
    uid = rep(seq(n), times = n_labs),
    event_date = as.Date(sample(n_days, sum(n_labs), replace = TRUE), origin = '2015-12-31'),
    key = c('uid', 'event_date')
  )

  surgery_dat <- data.table(
    uid = rep(seq(n), times = n_surgeries),
    event_date = as.Date(sample(n_days, sum(n_surgeries), replace = TRUE), origin = '2015-12-31'),
    key = c('uid', 'event_date')
  )

  rxfill_dat <- data.table(
    uid = rep(seq(n), times = n_rxfills),
    event_date = as.Date(sample(n_days, sum(n_rxfills), replace = TRUE), origin = '2015-12-31'),
    key = c('uid', 'event_date')
  )

  visit_dat[, obs := seq_len(.N), by = uid]
  lab_dat[, obs := seq_len(.N), by = uid]
  surgery_dat[, obs := seq_len(.N), by = uid]
  rxfill_dat[, obs := seq_len(.N), by = uid]

  visit_dat <- dcast(visit_dat, uid ~ obs, value.var = 'event_date')
  lab_dat <- dcast(lab_dat, uid ~ obs, value.var = 'event_date')
  surgery_dat <- dcast(surgery_dat, uid ~ obs, value.var = 'event_date')
  rxfill_dat <- dcast(rxfill_dat, uid ~ obs, value.var = 'event_date')

  setnames(visit_dat, -1, paste0('visit_', names(visit_dat)[-1]))
  setnames(lab_dat, -1, paste0('lab_', names(lab_dat)[-1]))
  setnames(surgery_dat, -1, paste0('surgery_', names(surgery_dat)[-1]))
  setnames(rxfill_dat, -1, paste0('rxfill_', names(rxfill_dat)[-1]))

  events_wide <- Reduce(function(x, y) merge(x, y, all = TRUE), list(visit_dat, lab_dat, surgery_dat, rxfill_dat))

  # clean up
  rm(visit_dat, lab_dat, surgery_dat, rxfill_dat)

  list(incident, demo, events_wide)
}
