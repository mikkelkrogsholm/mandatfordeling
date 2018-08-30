# dHondt_df ----
#' Beregner dHondt
#'
#' @param party en vektor med partier i valget
#' @param votes en vektor med partiernes stemmer
#' @param seats integer med antal mandater i alt
#'
#' @export
dHondt_df <- function(party, votes, seats){
  tmp <- tibble::tibble(
    candidates = rep(party, each = seats),
    scores = as.vector(sapply(votes, function(x) x / 1:seats))
  )

  tmp <- tmp$candidates[order( - tmp$scores )] [1:seats]
  tmp <- tibble::as_tibble(table(tmp))

  names(tmp) <- c("party", "seats")

  tmp
}

# added_seats ----
#' Beregner Sainte-Laguës med modificering
#'
#' @param candidates vektor med kandidater / partier
#' @param votes vektor med deres respektive stemmetal
#' @param seats antal mandater i alt
#' @param attained hvor mange de i forvejen har fået
#' @param by hvad devisoren skal hoppe med
#'
#' @export
#' @importFrom magrittr %>%
added_seats <- function(candidates, votes, seats, attained, by){

  reps <- seq(1, by = by, length.out = 175)
  cand_len <- length(candidates)

  tmp <- tibble::tibble(
    party = rep(candidates, each = length(reps)),
    scores = as.vector(sapply(votes, function(x) x / reps))
  )

  tmp <- purrr::map_dfr(1:length(candidates), function(i){

    slice_start <- attained[i] + 1

    if(slice_start > 0) {
      tmp %>%
        dplyr::filter(party == candidates[i]) %>%
        dplyr::slice(slice_start:n())
    } else {
      tmp %>%
        dplyr::filter(party == candidates[i])
    }

  })

  tmp <- tmp$party[order( - tmp$scores )] [1:seats]
  tmp <- tibble::as_tibble(table(tmp))

  names(tmp) <- c("party", "seats")

  tmp

}

# round_preserve_sum ----
#' Runder men beholder summen
#'
#' @param x tal der skal rundes
#' @param digits antal decimaler
#'
#' @export
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# fjern_under_graensen ----
#' Fjerner partier under spærregrænsen
#'
#' @param storkreds_votes stemmer fordelt på storkredse
#'
#' @export
#' @importFrom magrittr %>%
fjern_under_graensen <- function(storkreds_votes){

  parties_OK <- storkreds_votes %>%
    dplyr::group_by(parti) %>%
    dplyr::summarise(stemmer = sum(stemmer)) %>%
    dplyr::mutate(pct = stemmer / sum(stemmer),
                  above = pct > 0.02) %>%
    dplyr::filter(above) %>%
    dplyr::pull(parti)

  storkreds_votes_OK <- storkreds_votes %>%
    dplyr::filter(parti %in% parties_OK)

  return(storkreds_votes_OK)
}

# beregn_kredsmandater ----
#' Beregner kredsmandater
#'
#' @param storkreds_votes stemmer fordelt på storkredse
#' @param kredsmandater oversigt over antallet af kredsmandater
#'
#' @export
#' @importFrom magrittr %>%
beregn_kredsmandater <- function(storkreds_votes, kredsmandater){

  # Beregn fordelingen af kredsmandater
  storkreds_beregnet <- purrr::map_dfr(1:nrow(kredsmandater), function(i){

    # Filtrer på storkreds
    x <- kredsmandater$storkreds[i]
    y <- kredsmandater$mandater[i]
    res <- storkreds_votes %>% dplyr::filter(storkreds == x)

    # Beregn mandatfordelingen med dHondt
    out <- dHondt_df(party = res$parti, votes = res$stemmer, seats = y)

    # Tilføj storkreds til resultatet
    out$storkreds <- x

    # Tilføj de partier, der ikke kom med under dHondt og giv
    # dem 0 i mandater
    out <- out %>%
      dplyr::select(-storkreds) %>%
      dplyr::full_join(res, by = c("party" = "parti")) %>%
      dplyr::select(storkreds, parti = party, votes = stemmer, seats)

    out[is.na(out)] <- 0

    # Returner resultetet
    return(out)
  }) %>%
    # Og tilføj landsdele
    dplyr::left_join(landsdel_storkreds, ., by = "storkreds")

  # Lav et sanity check
  sanity_check <- storkreds_beregnet %>%
    dplyr::group_by(storkreds) %>%
    dplyr::summarise(seats = sum(seats))

  if(!(sum(sanity_check$seats) == 135)){
    warning("Total seats do not match")
  }

  m_v <- kredsmandater %>%
    dplyr::arrange(storkreds) %>%
    dplyr::pull(mandater)


  if(!(all(sanity_check$seats == m_v))){
    warning("Individual seats do not match")
  }

  # Returner resultatet
  return(storkreds_beregnet)
}

# beregn_manglende ----
#' Beregner manglende tillægsmandater
#'
#' @param kredsmandater_beregnet output fra beregn_kredsmandater()
#'
#' @export
#' @importFrom magrittr %>%
beregn_manglende <- function(kredsmandater_beregnet){

  # Beregn opnåede mandater ----
  seats_gotten <- kredsmandater_beregnet %>%
    dplyr::group_by(landsdel, parti) %>%
    dplyr::summarise(votes = sum(votes),
                     seats_gotten = sum(seats)) %>%
    dplyr::ungroup()

  # Beregn hvad der mangler at blive fordelt i tillægsmandater ----
  votes_per_seat <- sum(seats_gotten$votes) / 175

  seats_gotten_DK <- seats_gotten %>%
    dplyr::group_by(parti) %>%
    dplyr::summarise(votes = sum(votes),
                     seats_gotten = sum(seats_gotten)) %>%
    dplyr::mutate(seats_owed = votes / votes_per_seat,
                  seats_owed = round_preserve_sum(seats_owed),
                  seats_missing = seats_owed - seats_gotten)

  seats_gotten_DK[, 3:5] <- seats_gotten_DK[, 3:5] %>% purrr::map(as.integer)

  # Sanity check ----
  sanity_df <- seats_gotten_DK %>%
    tidyr::gather(seats_type, seats_count, -c(parti, votes)) %>%
    dplyr::group_by(seats_type) %>%
    dplyr::summarise(seats_count = sum(seats_count))

  if(!all(sanity_df$seats_count == c(135L, 40L, 175L))){
    warning("Seat count does not match")
  }

  return(seats_gotten_DK)

}

# beregn_tillaeg_landsdel ----
#' Beregner fordelingen af tillægsmandater på partier og landsdele
#'
#' @param kredsmandater_beregnet output fra beregn_kredsmandater()
#' @param manglende_beregnet output fra beregn_manglende()
#'
#' @export
#' @importFrom magrittr %>%
beregn_tillaeg_landsdel <- function(kredsmandater_beregnet, manglende_beregnet){

  seats_gotten <- kredsmandater_beregnet %>%
    dplyr::group_by(landsdel, parti) %>%
    dplyr::summarise(votes = sum(votes),
                     seats_gotten = sum(seats)) %>%
    dplyr::ungroup()

  party_missing <- manglende_beregnet %>%
    dplyr::select(parti, seats_missing) %>%
    dplyr::mutate(extra = 0)

  reps <- seq(1, by = 2, length.out = 175)

  landsdele <- seats_gotten %>% split(.$landsdel)

  devisor_tabel <- purrr::map_dfr(landsdele, function(landsdel_df){

    devisors <- purrr::map(1:nrow(landsdel_df), function(i){
      devs <- landsdel_df$votes[i] / reps
      j <- landsdel_df$seats_gotten[i]
      if(j > 0){
        devs <- devs[-c(1:j)]
      }
      devs
    })

    names(devisors) <- landsdel_df$parti

    devisors <- devisors %>% t() %>% tibble::as_tibble() %>% tidyr::gather(parti, devisorer)

    out <- landsdel_df %>% dplyr::left_join(devisors, by = "parti")

    return(out)

  }) %>%
    tidyr::unnest(devisorer) %>% dplyr::arrange(-devisorer)

  landsdel_missing <- tillægsmandater %>% dplyr::mutate(extra = 0)

  landsdel_out <- purrr::map_dfr(1:nrow(devisor_tabel), function(i){

    my_row <- devisor_tabel[i, ]
    my_party <- my_row$parti
    my_landsdel <- my_row$landsdel

    j <- which(manglende_beregnet$parti == my_party)
    k <- which(landsdel_missing$landsdel == my_landsdel)

    # If party still needs votes
    if(party_missing$seats_missing[j] > party_missing$extra[j] &&
       landsdel_missing$tillægsmandater[k] > landsdel_missing$extra[k]){

      party_missing$extra[j] <<- party_missing$extra[j] + 1
      landsdel_missing$extra[k] <<- landsdel_missing$extra[k] + 1

      return(my_row)
    } else {
      return(NULL)
    }

  }) %>%
    dplyr::count(landsdel, parti) %>%
    dplyr::rename(tillaeg = n)

  if(sum(landsdel_out$tillaeg) != 40L){
    warning("Sum of tillaeg does not equal 40L")
  }

  return(landsdel_out)

}

# beregn_endeligt ----
#' Beregner det endelige resultat og fordeler tillægsmandater på storkredse
#'
#' @param kredsmandater_beregnet output fra beregn_kredsmandater()
#' @param tillaeg_landsdel_beregnet output fra beregn_tillaeg_landsdel()
#'
#' @export
#' @importFrom magrittr %>%
beregn_endeligt <- function(kredsmandater_beregnet, tillaeg_landsdel_beregnet){

  partier <- tillaeg_landsdel_beregnet$parti %>% unique()
  landsdele <- tillaeg_landsdel_beregnet$landsdel %>% unique()

  storkreds_out <- purrr::map_dfr(landsdele, function(x){

    purrr::map_dfr(partier, function(y){

      input <- kredsmandater_beregnet %>%
        dplyr::filter(landsdel == x,
                      parti == y) %>%
        dplyr::rename(attained = seats)

      seats <- tillaeg_landsdel_beregnet %>%
        dplyr::filter(landsdel == x,
                      parti == y) %>%
        dplyr::pull(tillaeg)

      if(length(seats) == 0) seats <- 0

      if(seats != 0) {

        storkreds_out <- added_seats(candidates = input$storkreds, votes = input$votes,
                                     seats = seats, attained = input$attained, by = 3) %>%
          dplyr::rename(storkreds = party, extra = seats)

        out <- dplyr::left_join(input, storkreds_out, by = "storkreds")
        out[is.na(out)] <- 0

        out <- out %>% dplyr::mutate(total = attained + extra)

      } else {

        out <- input %>%
          dplyr::mutate(extra = 0,
                        total = attained + extra)

      }

      return(out)

    })

  })

  return(storkreds_out)
}

# beregn_mandater ----
#' Beregner fordelingen af mandater
#'
#' @param storkreds_votes stemmer fordelt på storkredse
#'
#' @export
beregn_mandater <- function(storkreds_votes){

  # Fjern partier under spærregrænsen
  storkreds_votes_OK <- fjern_under_graensen(storkreds_votes)

  # Beregn kredsmandater
  kredsmandater_beregnet <- beregn_kredsmandater(storkreds_votes_OK, kredsmandater)

  # Beregn hvor mange madater, der mangler i tillæg
  manglende_beregnet <- beregn_manglende(kredsmandater_beregnet)

  # Beregn fordelingen af tillæg på landsdele
  tillaeg_landsdel_beregnet <- beregn_tillaeg_landsdel(kredsmandater_beregnet, manglende_beregnet)

  # Beregn fordelingen af tillæg på storkredse og giv endeligt resultat
  endeligt_resultat <- beregn_endeligt(kredsmandater_beregnet, tillaeg_landsdel_beregnet)

  ## sanity

  if(!sum(endeligt_resultat$total) == 175){
    warning("Total seats are not matching")
  }

  if(!sum(endeligt_resultat$extra) == 40){
    warning("Extra seats are not matching")
  }

  ## return
  return(endeligt_resultat)
}

