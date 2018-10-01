#' Beregn nye stemmer på baggrund af meningsmåling
#'
#' Funktionen tager stemmerne fra FV15 og omfordeler dem efter meningsmålinger.
#'
#'
#' @param storkreds_res stemmerne fra FV15
#' @param pp ny meningsmåling
#'
#' @return en tibble
#' @export
beregn_nye_stemmer <- function(storkreds_res, pp){

  # Do input check
  name_check <- all(names(pp) == c("parti", "pp_pct"))
  if(!name_check) stop("pp must be a data frame with two columns: 'parti' and 'pp_pct'")

  # take out D - we need them later
  # we take them out since they did not exist in 2015.
  pp_n_d <- pp %>%
    dplyr::filter(parti != "D") %>%
    dplyr::mutate(pp_pct = pp_pct / sum(pp_pct) * 100)

  ## CALCULATE NEW VOTES ----

  # Make a data frame of the votes from FV15
  df1 <- storkreds_res %>%
    dplyr::group_by(storkreds, parti) %>%
    dplyr::summarise(stemmer = sum(stemmer)) %>%
    dplyr::ungroup()


  # Calculate each storkreds' share of total party votes
  df2 <- df1 %>%
    dplyr::group_by(parti) %>%
    dplyr::mutate(ratio = stemmer / sum(stemmer)) %>%
    dplyr::ungroup() %>%
    dplyr::select(- stemmer)

  # Calculate each partys new total votes based on the poll
  new_total_votes <- pp_n_d %>%
    dplyr::mutate(new_total_votes = sum(df1$stemmer) * (pp_pct / 100) ) %>%
    dplyr::select(- pp_pct)

  # Calculate new votes per storkreds by taking each storkreds' share of the
  # new votes
  df3 <- df2 %>%
    dplyr::left_join(new_total_votes, by = "parti") %>%
    dplyr::mutate(stemmer = new_total_votes * ratio) %>%
    dplyr::select(storkreds, parti, stemmer)

  # Calculate sums used in calculating
  new_sum <- df3 %>%
    dplyr::group_by(storkreds) %>%
    dplyr::summarise(new_sum = sum(stemmer))

  old_sum <- df1 %>%
    dplyr::group_by(storkreds) %>%
    dplyr::summarise(old_sum = sum(stemmer))

  # Even out the votes so no storkreds gets more or less votes than there are voters
  df4 <- df3 %>%
    dplyr::left_join(new_sum, by = "storkreds") %>%
    dplyr::left_join(old_sum, by = "storkreds") %>%
    dplyr::mutate(stemmer = stemmer / new_sum * old_sum) %>%
    dplyr::select(storkreds, parti, stemmer)

  ## SANITY CHECK without D ----

  # Here we perform a sanity check. Do we think everything is ok. This is more
  # or less

  message(crayon::blue("SANITY CHECK BEFORE ADDING D"))

  # Same total votes?
  tots_votes <- (df4 %>% dplyr::pull(stemmer) %>% sum()) == (df1 %>% dplyr::pull(stemmer) %>% sum())

  if(tots_votes){
    message(crayon::green("OK - Total votes match"))
  } else {
    warning("Total votes do not match")
  }

  # Storkreds votes the same?
  x <- df4 %>% dplyr::group_by(storkreds) %>% dplyr::summarise(new_stemmer = sum(stemmer))
  y <- df1 %>% dplyr::group_by(storkreds) %>% dplyr::summarise(old_stemmer = sum(stemmer))

  sk_match <- x %>%
    dplyr::left_join(y, by = "storkreds") %>%
    dplyr::mutate(same = round(new_stemmer) == round(old_stemmer))

  tots_votes_sk <- all(sk_match$same)

  if(tots_votes_sk){
    message(crayon::green("OK - Total votes match for each Storkreds"))
  } else {
    warning("Total votes do not match for each Storkreds")
  }

  # Percentages?
  pct_match <- df4 %>%
    dplyr::group_by(parti) %>%
    dplyr::summarise(stemmer = sum(stemmer)) %>%
    dplyr::mutate(pct = stemmer / sum(stemmer) * 100) %>%
    dplyr::left_join(pp_n_d,by = "parti") %>%
    dplyr::select(-stemmer) %>%
    dplyr::mutate(pct = round(pct, 1),
                  pp_pct = round(pp_pct, 1)) %>%
    purrr::set_names(c("parti", "input", "output")) %>%
    tidyr::gather(type, pct, - parti) %>%
    tidyr::spread(parti, pct)

  names(pct_match)[1] <- ""
  message("Check the percentages - are you ok with them?")
  message(paste0(knitr::kable(pct_match), collapse = "\n"))

  ## ADD D BACK IN ----

  # I will add in D evenly in every Storkreds. The share they have in the polls
  # I will give them in each Storkreds.

  d <- pp %>%
    dplyr::filter(parti == "D") %>%
    dplyr::pull(pp_pct)

  d_inv <- (100 - d) / 100

  df5 <- df4 %>%
    dplyr::mutate(d_stemmer = stemmer * d_inv,
                  d = stemmer - d_stemmer) %>%
    dplyr::group_by(storkreds) %>%
    dplyr::summarise(parti = "D", stemmer = sum(d))

  df6 <- df4 %>%
    dplyr::mutate(stemmer = stemmer * d_inv)

  df7 <- df5 %>% dplyr::bind_rows(df6) %>%
    dplyr::arrange(storkreds, parti)


  ## SANITY CHECK with D ----

  # Here we perform a sanity check. Do we think everything is ok. This is more
  # or less

  message(crayon::blue("SANITY CHECK AFTER ADDING D"))

  # Same total votes?
  tots_votes <- (df7 %>% dplyr::pull(stemmer) %>% sum()) == (df1 %>% dplyr::pull(stemmer) %>% sum())

  if(tots_votes){
    message(crayon::green("OK - Total votes match"))
  } else {
    warning("Total votes do not match")
  }

  # Storkreds votes the same?
  x <- df7 %>% dplyr::group_by(storkreds) %>% dplyr::summarise(new_stemmer = sum(stemmer))
  y <- df1 %>% dplyr::group_by(storkreds) %>% dplyr::summarise(old_stemmer = sum(stemmer))

  sk_match <- x %>%
    dplyr::left_join(y, by = "storkreds") %>%
    dplyr::mutate(same = round(new_stemmer) == round(old_stemmer))

  tots_votes_sk <- all(sk_match$same)

  if(tots_votes_sk){
    message(crayon::green("OK - Total votes match for each Storkreds"))
  } else {
    warning("Total votes do not match for each Storkreds")
  }

  # Percentages?
  pct_match <- df7 %>%
    dplyr::group_by(parti) %>%
    dplyr::summarise(stemmer = sum(stemmer)) %>%
    dplyr::mutate(pct = stemmer / sum(stemmer) * 100) %>%
    dplyr::left_join(pp, by = "parti") %>%
    dplyr::select(-stemmer) %>%
    dplyr::mutate(pct = round(pct, 1),
                  pp_pct = round(pp_pct, 1)) %>%
    purrr::set_names(c("parti", "input", "output")) %>%
    tidyr::gather(type, pct, - parti) %>%
    tidyr::spread(parti, pct)

  names(pct_match)[1] <- ""
  message("Check the percentages - are you ok with them?")
  message(paste0(knitr::kable(pct_match), collapse = "\n"))

  # Return data
  return(df7)

}
