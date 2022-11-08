#' scrapeHemnet
#'
#' @param area numeric value from hemnet
#'
#' @return totRes
#' @export 
#'
#' @import rvest xml2 purrr dplyr tidyr stringr lubridate
#'    
#' @importFrom magrittr %>%
#'    

scrapeHemnet <- function(area) {
  
  for (page in 1:49) {
    p <-
      paste(
        "https://www.hemnet.se/salda/bostader?location_ids%5B%5D=", area, "&page=",
        page,
        # "&rooms_max=2.5&rooms_min=2&selling_price_max=3500000&selling_price_min=2500000",
        sep = ""
      )
    
    htmlfile <- read_html(p)
    res <-
      list(
        # Address
        rvest::html_nodes(htmlfile, ".qa-selling-price-title") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = ""),
        
        # Kvadratmeter o rum
        rvest::html_nodes(htmlfile, ".sold-property-listing__area") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = ""),
        
        # hyra
        rvest::html_nodes(htmlfile, ".sold-property-listing__fee") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "") %>%
          gsub("[^[:digit:].]", "", .) %>%
          as.numeric(),
        
        # Slutpris
        rvest::html_nodes(htmlfile, ".sold-property-listing__price") %>%
          rvest::html_nodes(".sold-property-listing__subheading") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "") %>%
          gsub("[^[:digit:].]", "", .) %>%
          as.numeric(),
        
        # Sald Datum
        rvest::html_nodes(htmlfile, ".sold-property-listing__sold-date") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "") %>%
          gsub(pattern = "Såld", replacement = "") %>%
          gsub(pattern = " ", replacement = ""),
        
        #  Over eller under utropspris
        rvest::html_nodes(htmlfile, ".sold-property-listing__price-change") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = " ", replacement = "") %>%
          gsub(pattern = "%", replacement = ""),
        
        # Pris per kvm
        rvest::html_nodes(htmlfile, ".sold-property-listing__price-per-m2") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "") %>%
          gsub("[^[:digit:].]", "", .) %>%
          as.numeric(),
        
        # Forsaljnings objekt ( Lgh, Radhus, Tomt... )
        rvest::html_nodes(htmlfile, ".svg-icon__fallback-text") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "")
      ) %>%
      purrr::transpose() %>%
      unlist(recursive = T) %>%
      matrix(ncol = 8, byrow = T) %>%
      tidyr::as_tibble() %>%
      dplyr::mutate (across (c (V3, V4, V7), as.numeric)) %>%
      dplyr::mutate(V5 = lubridate::dmy(V5)) %>%
      dplyr::rename(
        address = V1,
        kvm = V2,
        hyra = V3,
        pris = V4,
        soldDate = V5,
        budgivning = V6,
        kvmPris = V7,
        objektTyp = V8
      ) %>%
      
      dplyr::mutate(ar = year(soldDate), manad = month(soldDate)) %>%
      tidyr::separate(kvm, c("kvm", "rum"), "m²") %>%
      dplyr::mutate( kvm = stringr::str_trim(kvm)) %>%
      dplyr::mutate( kvm = stringr::str_replace(kvm, ",", ".")) %>%
      dplyr::mutate( rum = stringr::str_replace(rum, ",", ".")) %>%
      dplyr::mutate( rum = stringr::str_replace(rum, "rum", "")) %>%
      dplyr::mutate( rum = stringr::str_trim(rum)) %>%
      dplyr::mutate(rum = as.numeric(rum))
    
    if (page == 1) {
      totRes <- res
    }
    else {
      totRes <- bind_rows(totRes, res)
    }
    Sys.sleep(1)
    
    
  }
  
  totRes
  
}