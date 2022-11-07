#' scrapeHemnet
#'
#' @param area 
#'
#' @return totRes
#' @export 
#'
#' @import rvest, xml12, data.table, purrr, 
#'  lubridate,
#'   ggplot2,
#'    dplyr
#'    
#' @importFrom magrittr %>%
#'    
#' @examples scrapeHemnet("898748")
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
        
        # Såld Datum
        rvest::html_nodes(htmlfile, ".sold-property-listing__sold-date") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "") %>%
          gsub(pattern = "Såld", replacement = "") %>%
          gsub(pattern = " ", replacement = ""),
        
        #  Över eller under utropspris
        rvest::html_nodes(htmlfile, ".sold-property-listing__price-change") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = " ", replacement = "") %>%
          mutate( rum = str_replace(rum, "%", "")),
        
        # Pris per kvm
        rvest::html_nodes(htmlfile, ".sold-property-listing__price-per-m2") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "") %>%
          gsub("[^[:digit:].]", "", .) %>%
          as.numeric(),
        
        # Försäljnings objekt ( Lgh, Radhus, Tomt... )
        rvest::html_nodes(htmlfile, ".svg-icon__fallback-text") %>%
          rvest::html_text() %>%
          gsub(pattern = "\n ", replacement = "") %>%
          gsub(pattern = "  ", replacement = "")
      ) %>%
      purrr::transpose() %>%
      unlist(recursive = T) %>%
      matrix(ncol = 8, byrow = T) %>%
      tibble::as_tibble() %>%
      dplyr::mutate (across (c (V3, V4, V7), as.numeric)) %>%
      dplyr::mutate(V5 = dmy(V5)) %>%
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
      
      dplyr::mutate(år = year(soldDate), månad = month(soldDate)) %>%
      separate(kvm, c("kvm", "rum"), "m²") %>%
      mutate( kvm = str_trim(kvm)) %>%
      mutate( kvm = str_replace(kvm, ",", ".")) %>%
      mutate( rum = str_replace(rum, ",", ".")) %>%
      mutate( rum = str_replace(rum, "rum", "")) %>%
      mutate( rum = str_trim(rum)) %>%
      mutate(rum = as.numeric(rum))
    
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