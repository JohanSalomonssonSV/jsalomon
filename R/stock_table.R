#' stock_table
#' @param key API key 
#' @importFrom dplyr  mutate left_join rename
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @export
#' @return all_ins data frame
stock_table<-function(key){
  all_ins<-fetch_instruments(key)%>% tibble()
  countries<-fetch_countries(key=key) %>% rename(countryId=id, country=name) %>% tibble()
  markets<-fetch_markets(key)%>% rename(marketId=id, market=name)%>% tibble()
  sectors<-fetch_sectors(key)%>% rename(sectorId=id, sector=name)%>% tibble()
  branch<-fetch_branches(key) %>%  rename(branchId=id, branch=name)%>% tibble()
  all_ins<-all_ins %>% left_join(countries) %>% left_join(markets) %>% left_join(branch %>% left_join(sectors))
  all_ins}

