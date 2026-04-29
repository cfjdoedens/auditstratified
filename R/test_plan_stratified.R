#' @title
#'   test plan_stratified()
#'
#' @description
#'   Test uitgaande van de invoervelden van plan_stratified plus
#'   de uitvoervelden of plan_stratified() juist heeft gewerkt.
#'
#' @details
#'   De functie test of:
#'   1.  De max_fout inderdaad onder de materialiteit ligt
#'   2.  Of voor elk van de strata waarvoor n_laag_extra > 0 geldt,
#'       dat als je deze waarde met 1 verlaagt,
#'       en je dan eval_stratified() toepast, je dan boven de materialiteit uitkomt.
#'
#' @param steekproeven Een tibble met de planningsgegevens.
#' @param totale_materialiteit De maximaal toegestane foutfractie voor de gehele populatie.
#' @param totale_zekerheid Het algehele zekerheidsniveau (bijv. 0.95).
#' @param model Het statistische model dat gebruikt wordt voor de extrapolatie.
#'   Keuze uit \code{"binomiaal"} (standaard) of \code{"poisson"}.
#' @param methode Rekenmethode voor evaluatie.
#'   Keuze uit \code{"FFT"} (standaard) of \code{"MonteCarlo"}.
#'   \code{"FFT"} wordt aanbevolen. Deels omdat dat ietsje sneller is, deels
#'   omdat dat dezelfde resultaten geeft ongeacht de startwaarde van de
#'   toevalsgenerator.
#' @param granulariteit Bepaalt de nauwkeurigheid van de berekening.
#'   Bij \code{"FFT"} is dit het aantal stappen op de kanskromme-as.
#'   Bij \code{"MonteCarlo"} is dit het aantal toevalsiteraties.
#'
#' @returns Lijst met antwoorden op vraag 1 en 2.
#' @importFrom dplyr left_join select
test_plan_stratified <- function(steekproeven,
                                 totale_materialiteit,
                                 totale_zekerheid = 0.95,
                                 model = c("binomiaal", "poisson"),
                                 methode = c("FFT", "MonteCarlo"),
                                 granulariteit = 10000) {
  model <- match.arg(model)
  methode <- match.arg(methode)

  plan_result <- plan_stratified(
    steekproeven,
    totale_materialiteit,
    totale_zekerheid,
    model,
    methode,
    granulariteit
  )

  # Gebruik direct het verrijkte resultaat van de planning.
  # We kopiëren n_definitief naar n_laag zodat eval_stratified ermee kan rekenen,
  # en we behouden bewust de k_laag (verwachte fouten) zoals berekend in het plan.
  eval_data <- plan_result |>
    mutate(n_laag = .data$n_definitief)

  # Test 1
  #   Beoordeel of convolutie met de definitieve n_laag de
  #   materialiteit respecteert.
  eval_res_1 <- eval_stratified(
    steekproeven = eval_data,
    model = model,
    zekerheid = totale_zekerheid,
    methode = methode,
    granulariteit = granulariteit,
    vergelijk = FALSE
  )
  max_fout_1 <- eval_res_1$max_fout_convolutie
  test_1_passed <- max_fout_1 < totale_materialiteit ||
    isTRUE(all.equal(max_fout_1, totale_materialiteit))

  # Test 2
  #   Beoordeel voor elk stratum of een vermindering van 1 post leidt tot
  #   het falen van de materialiteitseis.
  {
    strata_met_extra <- eval_data |> filter((.data$n_definitief - .data$n_basis) > 0)

    test_2_results <- list()
    test_2_passed <- TRUE

    if (nrow(strata_met_extra) > 0) {
      for (i in 1:nrow(strata_met_extra)) {
        stratum_naam <- strata_met_extra$naam[i]

        # Haal er precies 1 post af voor dit specifieke stratum.
        test_data <- eval_data |>
          mutate(n_laag = ifelse(
            .data$naam == stratum_naam,
            .data$n_laag - 1,
            .data$n_laag
          ))

        eval_res_2 <- eval_stratified(
          steekproeven = test_data,
          model = model,
          zekerheid = totale_zekerheid,
          methode = methode,
          granulariteit = granulariteit,
          vergelijk = FALSE
        )

        max_fout_2 <- eval_res_2$max_fout_convolutie
        passed <- max_fout_2 > totale_materialiteit

        test_2_results[[stratum_naam]] <- list(
          geteste_n_laag = test_data$n_laag[test_data$naam == stratum_naam],
          max_fout = max_fout_2,
          passed = passed
        )

        if (!passed) {
          test_2_passed <- FALSE
        }
      }
    }
    }

  # Retourneer de samenvatting van de testen.
  list(
    test_1 = list(
      beschrijving = "Ligt de maximale fout met de geplande steekproef onder de materialiteit?",
      max_fout = max_fout_1,
      materialiteit = totale_materialiteit,
      passed = test_1_passed
    ),
    test_2 = list(
      beschrijving = "Leidt 1 post minder bij de strata met n_laag_extra > 0 tot een overschrijding van de materialiteit?",
      aantal_strata_getest = nrow(strata_met_extra),
      passed = test_2_passed,
      details = test_2_results
    ),
    overall_passed = test_1_passed && test_2_passed
  )
}
