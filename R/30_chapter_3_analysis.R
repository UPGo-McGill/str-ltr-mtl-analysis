#### 30 CHAPTER 3 ANALYSIS #####################################################

#' This script produces the tables and facts for chapter 3. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `05_cmhc_data_import.R`
#' - `09_str_processing.R`
#' - `12_rent_increases.R`
#' - `13_condo_analysis.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")

load("output/str_processed.Rdata")
load("output/geometry.Rdata")
load("output/cmhc.Rdata")
load("output/condo_analysis.Rdata")


# Prepare new objects -----------------------------------------------------

FREH <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", day(date) == 1) %>% 
  group_by(date) %>% 
  summarize(across(c(FREH, FREH_3), sum))

FREH_borough <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", month(date) == 12, day(date) == 1) %>% 
  group_by(borough, date) %>% 
  summarize(FREH = sum(FREH_3), .groups = "drop") %>% 
  mutate(date = year(date))

GH_borough <- 
  GH %>% 
  filter(date >= "2016-01-01", month(date) == 12, day(date) == 31) %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(boroughs) %>% 
  st_drop_geometry() %>% 
  group_by(borough, date) %>% 
  summarize(GH = sum(housing_units, na.rm = TRUE), .groups = "drop") %>% 
  mutate(date = year(date))


# STR-induced housing loss ------------------------------------------------

#' At the end of 2020, there were 1,810 [1] FREH listings in the City of 
#' Montreal, and 230 [2] more housing units which were operating as ghost 
#' hostels. In total, therefore, short-term rentals removed 2,040 [3] housing 
#' units from Montreal’s long-term market last year (Figure 3.1). This 
#' represents a dramatic decline of 63.1% [4] since the end of 2019, when the
#' figure was 5,530 [5].

#' [1] FREH in 2020-12
FREH %>% 
  filter(date == "2020-12-01") %>% 
  pull(FREH_3) %>%
  scales::comma(10)

#' [2] GH in 2020-12
GH %>% 
  filter(date == LTM_end_date, status != "B") %>% 
  pull(housing_units) %>% sum() %>% 
  scales::comma(10)

#' [3] Total housing loss for 2019
{{FREH %>% filter(date == "2020-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date, status != "B") %>% 
        pull(housing_units) %>% sum()}} %>% 
  scales::comma(10)

#' [4] YOY increase in housing loss
{{{FREH %>% filter(date == "2020-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date, status != "B") %>% 
        pull(housing_units) %>% sum()}} /
  {{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
      {GH %>% filter(date == LTM_end_date - years(1), status != "B") %>% 
          pull(housing_units) %>% sum()}}} %>% 
  {. - 1} %>% 
  scales::percent(0.1)

#' [5] Total housing loss for 2019
{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
  {GH %>% filter(date == LTM_end_date - years(1), status != "B") %>% 
      pull(housing_units) %>% 
      sum()}} %>% 
  scales::comma(10)

#'  While the number of active daily listings declined by 8.8% [1] from 2018 to 
#'  2019, the number of housing units which STRs removed from Montreal’s housing 
#'  market increased by 14.2% [2] in that same time period, from 4,850 [3] to 
#'  5,530 [4].

#' [1] Active daily listings in 2018 and 2019
daily %>% 
  filter(housing, year(date) %in% 2018:2019, status != "B") %>% 
  group_by(year = year(date)) %>% 
  summarize(listings = sum(n()) / 365, .groups = "drop") %>% 
  summarize(change = (listings[1] - listings[2]) / listings[2]) %>% 
  pull(change) %>% 
  scales::percent(0.1)

#' [2] YOY increase in housing loss, 2018-2019
{{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date - years(1), status != "B") %>% 
        pull(housing_units) %>% sum()}} /
    {{FREH %>% filter(date == "2018-12-01") %>% pull(FREH_3)} +
        {GH %>% filter(date == LTM_end_date - years(2), status != "B") %>% 
            pull(housing_units) %>% sum()}}} %>% 
  {. - 1} %>% 
  scales::percent(0.1)

#' [3] STR housing loss, 2018
{{FREH %>% filter(date == "2018-12-01") %>% pull(FREH_3)} +
  {GH %>% filter(date == LTM_end_date - years(2), status != "B") %>% 
      pull(housing_units) %>% sum()}} %>% 
  scales::comma(10)

#' [4] STR housing loss, 2018
{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date - years(1), status != "B") %>% 
        pull(housing_units) %>% sum()}} %>% 
  scales::comma(10)

#' In 2019, six in ten (60.9% [1]) entire-home listings and one in four 
#' (26.7% [1]) private-room listings contributed to housing loss in the city. 
#' In 2016, the proportions were only 33.5% [1] and 12.6% [1] respectively. Even 
#' in 2020, amidst the pandemic, one in seven (14.9% [1]) entire-home listings 
#' and three in ten (31.6% [1]) private-room listings were taking housing off 
#' the market in Montreal (Figure 3.2). 

#' [1] Housing loss shares
daily %>% 
  filter(housing, status != "B", 
         listing_type %in% c("Entire home/apt", "Private room")) %>% 
  group_by(date) %>% 
  summarize(eh = mean(FREH_3[listing_type == "Entire home/apt"] > 0.5),
            pr = mean(GH[listing_type == "Private room"])) %>% 
  mutate(across(c(eh, pr), slide_dbl, mean, .before = 30)) %>% 
  mutate(across(c(eh, pr), scales::percent, 0.1)) %>% 
  filter(date %in% as.Date(c("2016-12-31", "2019-12-31", "2020-12-31")))

#' The 2,120 housing units taken off of Montreal’s housing market in 2020 is 
#' only 0.3% [1] of the total amount of housing in the city, but this housing 
#' loss has been concentrated in a small part of the city..... In the borough 
#' of Ville-Marie, 1.1% [2] of all housing units have been converted to 
#' dedicated STRs, while the figure is 0.9% [2] for Le Plateau-Mont-Royal.

#' [1] Total housing loss as % of dwellings, 2020
{{{FREH %>% filter(date == "2020-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date) %>% pull(housing_units) %>% sum()}} /
  sum(boroughs$dwellings)} %>% 
  scales::percent(0.1)

#' [2] Housing loss in VM and LPMR
boroughs %>% 
  left_join(FREH_borough) %>% 
  left_join(GH_borough) %>% 
  filter(date == "2020") %>% 
  mutate(GH = replace_na(GH, 0L),
         housing_loss_per_dwelling = (FREH + GH) / dwellings,
         housing_loss_per_dwelling = 
           scales::percent(housing_loss_per_dwelling, 0.1)) %>% 
  st_drop_geometry() %>% 
  filter(borough %in% c("Ville-Marie", "Le Plateau-Mont-Royal"))

#' Table 3.1
borough_housing_table <- 
  boroughs %>% 
  st_drop_geometry() %>% 
  left_join(FREH_borough, by = "borough") %>% 
  left_join(GH_borough, by = c("borough", "date")) %>%
  mutate(GH = replace_na(GH, 0L)) %>% 
  group_by(borough) %>% 
  summarize(
    dwellings = mean(dwellings),
    loss_2018 = sum(FREH[date == 2018]) + sum(GH[date == 2018]),
    loss_2019 = sum(FREH[date == 2019]) + sum(GH[date == 2019]),
    loss_2020 = sum(FREH[date == 2020]) + sum(GH[date == 2020]),
    change_2019 = (loss_2019 - loss_2018) / loss_2018,
    change_2020 = (loss_2020 - loss_2019) / loss_2019,
    loss_pct_2019 = loss_2019 / dwellings,
    loss_pct_2020 = loss_2020 / dwellings)
  
borough_housing_table %>% 
  summarize(
    dwellings = sum(dwellings),
    loss_2018 = sum(loss_2018),
    loss_2019 = sum(loss_2019),
    loss_2020 = sum(loss_2020),
    change_2019 = (loss_2019 - loss_2018) / loss_2018,
    change_2020 = (loss_2020 - loss_2019) / loss_2019,
    loss_pct_2019 = loss_2019 / dwellings,
    loss_pct_2020 = loss_2020 / dwellings) %>% 
  mutate(borough = "City of Montreal", .before = dwellings) %>% 
  bind_rows(borough_housing_table) %>% 
  select(-dwellings, -loss_2018) %>% 
  filter(loss_2020 > 40) %>% 
  arrange(desc(loss_2020)) %>%
  mutate(across(c(loss_2019, loss_2020), scales::comma, 10)) %>% 
  mutate(across(change_2019:loss_pct_2020, scales::percent, 0.1)) %>% 
  rename(Borough = borough,
         `Housing loss (2019)` = loss_2019,
         `Housing loss (2020)` = loss_2020,
         `Year-over-year growth (2018-2019)` = change_2019,
         `Year-over-year growth (2019-2020)` = change_2020,
         `% of housing lost (2019)` = loss_pct_2019,
         `% of housing lost (2020)` = loss_pct_2020) %>% 
  gt() %>% 
  tab_header(title = "STR-induced housing loss by borough") %>%
  opt_row_striping()



# The impact of STRs on rental housing supply and vacancy rates -----------

#' In 2019, the City of Montreal’s rental vacancy rate declined for the third 
#' year in a row, reaching a 15-year low of 1.6% [1]. According to CMHC, a 
#' healthy rental market should have a vacancy rate of at least 3%, but most 
#' neighbourhoods in the central city are significantly below that. Le Sud-Ouest 
#' had the city’s lowest vacancy rate, at 0.3% [2], which means that, of the 
#' zone’s approximately 31,000 [3] rental apartments, fewer than 100 [4] were 
#' available to be rented by prospective tenants in October 2019, when CMHC’s 
#' survey was conducted. In general, vacancy rates are even lower for 
#' family-sized housing units (defined by CMHC as units with two or more 
#' bedrooms). For example, the vacancy rate for units with three or more 
#' bedrooms was 0.5% [5] for Ville-Marie (the Downtown Montreal/Îles-des-Soeurs 
#' zone), and 0.3% [6] in Le Plateau-Mont-Royal. 

#' #' [1] 2019 city vacancy
#' city_vacancy %>% 
#'   filter(date == 2019, bedroom == "Total")
#' 
#' #' [2] Le Sud-Ouest 2019 vacancy
#' annual_vacancy %>% 
#'   filter(zone == 2, date == 2019, dwelling_type == "Total", bedroom == "Total")
#' 
#' #' [3] Total rental units in Le Sud-Ouest
#' annual_units %>% 
#'   filter(zone == 2, date == 2019, dwelling_type == "Total", 
#'          bedroom == "Total") %>% 
#'   pull(units) %>% 
#'   round(-2)

#' #' [4] Total vacant units in Le Sud-Ouest
#' annual_units %>% 
#'   left_join(annual_vacancy) %>% 
#'   filter(zone == 2, date == 2019, dwelling_type == "Total", 
#'          bedroom == "Total") %>% 
#'   transmute(vacant_units = vacancy * units)
#' 
#' #' [5] 3+ bedroom vacancy rate in VM
#' annual_vacancy %>% 
#'   filter(zone == 1, date == 2018, dwelling_type == "Total",
#'          bedroom == "3 Bedroom +") %>% 
#'   pull(vacancy)
#' 
#' #' [6] 3+ bedroom vacancy rate in LPM
#' annual_vacancy %>% 
#'   filter(zone == 6, date == 2019, dwelling_type == "Total",
#'          bedroom == "3 Bedroom +") %>% 
#'   pull(vacancy)

#' For example, in the Notre-Dame-de-Grâce/Côte-St-Luc zone, 66.9% [1] of 
#' households are renters, so we assume that 66.9% of housing units converted to 
#' dedicated STRs would have been rental housing, and the remaining 33.1% would 
#' have been ownership housing.

# DA_probabilities_2019 %>% 
#   mutate(across(c(p_condo, p_renter), ~{.x * dwellings})) %>% 
#   mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
#   select(dwellings, p_condo, p_renter, geometry) %>% 
#   st_interpolate_aw(cmhc, extensive = TRUE) %>% 
#   st_drop_geometry() %>% 
#   select(-Group.1) %>% 
#   rename(n_condo = p_condo, n_renter = p_renter) %>% 
#   cbind(cmhc, .) %>% 
#   as_tibble() %>% 
#   select(-geometry) %>% 
#   mutate(p_renter = n_renter / dwellings) %>% 
#   select(zone, p_renter) %>% 
#   filter(zone == 4)


# The impact of STRs on residential rents ---------------------------------

#' Between 2015 and 2019, we estimate that STRs have been responsible for a 
#' 2.4% [1] increase in the average monthly rent. As average rents have risen 
#' 16.9% [2] in this time period, this implies that one seventh of all rent 
#' increases over the last five years have been caused by the growth of STRs. 
#' Put differently, from 2015 to 2019, the average renter household in Montreal 
#' has paid an additional $870 [3] in rent because of the impact of STRs on the 
#' housing market.

#' #' [1] Total rent increase
#' rent_increase %>% 
#'   slice(-1) %>% 
#'   mutate(rent_increase = rent_increase + 1) %>% 
#'   pull(rent_increase) %>% 
#'   prod() %>% 
#'   {. - 1} %>% 
#'   round(3)
#' 
#' #' [2] Average rent increase
#' city_avg_rent %>% 
#'   filter(bedroom == "Total") %>% 
#'   filter(date >= 2014) %>% 
#'   mutate(increase = slide_dbl(avg_rent, ~{.x[2] / .x[1]}, .before = 1)) %>% 
#'   slice(-1) %>% 
#'   pull(increase) %>% 
#'   prod() %>% 
#'   {. - 1} %>% 
#'   round(3)
#' 
#' #' [3] Extra rent payment
#' city_avg_rent %>% 
#'   filter(bedroom == "Total") %>% 
#'   mutate(date = as.character(date)) %>% 
#'   left_join(rent_increase, by = c("date" = "year_created")) %>% 
#'   filter(date >= "2015") %>% 
#'   select(date, avg_rent, rent_increase) %>% 
#'   mutate(rent_plus_1 = 1 + rent_increase,
#'          cumulative_increase = slide_dbl(rent_plus_1, prod, .before = 4),
#'          extra_rent = (cumulative_increase - 1) * avg_rent * 12) %>% 
#'   pull(extra_rent) %>% 
#'   sum() %>% 
#'   round(-1)
#' 
#' #' Table 3.2
#' strs_by_zone <- 
#'   property %>% 
#'   st_intersection(cmhc) %>% 
#'   st_drop_geometry() %>% 
#'   select(property_ID, zone) %>% 
#'   left_join(daily, .) %>% 
#'   filter(housing, date >= LTM_start_date, date <= LTM_end_date, 
#'          status != "B") %>% 
#'   count(zone) %>% 
#'   mutate(active_strs = n / 365)
#' 
#' annual_avg_rent %>% 
#'   filter(bedroom == "Total", occupied_status == "Occupied Units") %>% 
#'   select(-bedroom, -occupied_status, -quality, -occ_rent_higher) %>% 
#'   mutate(date = as.character(date)) %>% 
#'   left_join(rent_increase_zone, by = c("zone", "date" = "year_created")) %>% 
#'   group_by(zone, zone_name) %>%
#'   mutate(rent_plus_1 = 1 + rent_increase,
#'          cumulative_increase = slide_dbl(rent_plus_1, prod, .before = 4),
#'          extra_rent = (cumulative_increase - 1) * avg_rent * 12) %>% 
#'   summarize(avg_rent_2015 = avg_rent[date == 2015],
#'             avg_rent_2019 = avg_rent[date == 2019],
#'             rent_change = (avg_rent_2019 - avg_rent_2015) / avg_rent_2015,
#'             str_rent_increase = prod(rent_plus_1) - 1,
#'             str_share = str_rent_increase / rent_change,
#'             extra_rent = sum(extra_rent)) %>% 
#'   ungroup() %>% 
#'   left_join(strs_by_zone) %>% 
#'   relocate(active_strs, .after = zone_name) %>% 
#'   select(-zone, -n) %>% 
#'   add_row(
#'     tibble_row(
#'       zone_name = "City of Montreal",
#'       active_strs = nrow(
#'         filter(daily, housing, status != "B", date >= LTM_start_date, 
#'                date <= LTM_end_date)) / 365,
#'       avg_rent_2015 = (filter(city_avg_rent, date == 2015, 
#'                               bedroom == "Total"))$avg_rent,
#'       avg_rent_2019 = (filter(city_avg_rent, date == 2019, 
#'                               bedroom == "Total"))$avg_rent,
#'       rent_change = (avg_rent_2019 - avg_rent_2015) / avg_rent_2015,
#'       str_rent_increase = (rent_increase %>% 
#'                              slice(-1) %>% 
#'                              mutate(rent_increase = rent_increase + 1) %>% 
#'                              pull(rent_increase) %>% 
#'                              prod()) - 1,
#'       str_share = str_rent_increase / rent_change,
#'       extra_rent = city_avg_rent %>% 
#'         filter(bedroom == "Total") %>% 
#'         mutate(date = as.character(date)) %>% 
#'         left_join(rent_increase, by = c("date" = "year_created")) %>% 
#'         filter(date >= "2015") %>% 
#'         select(date, avg_rent, rent_increase) %>% 
#'         mutate(rent_plus_1 = 1 + rent_increase,
#'                cumulative_increase = slide_dbl(rent_plus_1, prod, .before = 4),
#'                extra_rent = (cumulative_increase - 1) * avg_rent * 12) %>% 
#'         pull(extra_rent) %>% 
#'         sum())) %>% 
#'   arrange(-active_strs) %>% 
#'   slice(1:9) %>% 
#'   mutate(across(c(active_strs, avg_rent_2015, avg_rent_2019, extra_rent), round, 
#'                 -1)) %>% 
#'   set_names(c("CMHC Zone", "Active daily STR listings (2019)", 
#'               "Average rent (2015)", "Average rent (2019)",
#'               "Total rent increase (2015-2019)", 
#'               "STR-induced rent increase (2015-2019)",
#'               "STR share of total rent increase",
#'               "Average extra rent paid due to STRs (2015-2019")) %>% 
#'   gt() %>%
#'   tab_header(title = "STR impacts on rents") %>%
#'   opt_row_striping() %>% 
#'   fmt_percent(columns = c(5:7), decimals = 1) %>% 
#'   fmt_number(columns = 2:4, decimals = 0) %>% 
#'   fmt_currency(8, decimals = 0)


# Clean up ----------------------------------------------------------------

rm(annual_avg_rent, annual_units, annual_vacancy, borough_housing_table,
   boroughs, boroughs_raw, city, city_avg_rent, city_units, city_vacancy,
   cmhc, DA, DA_probabilities_2017, DA_probabilities_2019, FREH, FREH_borough,
   GH_borough, listing_probabilities_2017, listing_probabilities_2019, province,
   rent_increase, rent_increase_zone, streets, streets_downtown, strs_by_zone)
