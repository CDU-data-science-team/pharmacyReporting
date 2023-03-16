# This code was run during shadow testing

# DB code

source("shadow_secret.R")

three_years_ago <- Sys.Date() - 366 * 3

two_years_ago <- Sys.Date() - 366 * 2

one_week_ago <- Sys.Date() - 7

# Add in AAH divided order_cycle for shadow testing purposes
product_ord_cycle <- readr::read_csv(
  "secret/AAH product profle with order cycle 1.1.22.csv")

sup_order_sched <- tibble::tibble(Supplier = c("AAH", "AAH"),
                                  Order_cycle = c("AAHa", "AAHb"),
                                  Order_day = c("Monday", "Monday"),
                                  Order_cycle_start_date = as.Date(
                                    c("2022-01-03", "2022-01-10")))

Outstanding_orders <- structure(
  list(Ord_index = 30, Ord_daycnt = 407, Ord_quant = 0,
       Ord_del_daycnt = -1), class = "data.frame", row.names = c(NA, -1L)
)

# Create table to convert modular order schedule into Delta_p
ord_sch <- tibble::tibble(x = rep(c(0:13), times = 5),
                          Day = c(rep(c("Monday"), times = 14),
                                  rep(c("Tuesday"), times = 14),
                                  rep(c("Wednesday"), times = 14),
                                  rep(c("Thursday"), times = 14),
                                  rep(c("Friday"), times = 14)),
                          Delta_p = rep(c(14:1), times = 5))

# Make table for different site numbers - this is static data and never changes
# so can be set up once within the system

Site_table <- dplyr::tibble(
  Site = as.numeric(c("100", "240", "241", "242", "878", "958")),
  SiteID = as.numeric(c("1476", "15", "260", "19", "1145", "259")),
  LocationID_Site = as.numeric(c("1476", "15", "260", "19", "1145", "259")))

# Import orderlogs

board <- pins::board_rsconnect()

w_order_log_df2 <- board %>%
  pins::pin_read("Hazel.Kirkland/w_order_log_df2")

w_order_log_df1 <- w_order_log_df2 %>%
  dplyr::filter(DateReceived > two_years_ago | DateOrdered > two_years_ago)

# Change relevant columns to dates and numbers
w_order_log_df1$DateReceived <-  as.Date(as.POSIXct(w_order_log_df1$DateReceived, 'GMT'))
w_order_log_df1$DateOrdered <-  as.Date(as.POSIXct(w_order_log_df1$DateOrdered, 'GMT'))
w_order_log_df1$QtyRec <- as.integer(w_order_log_df1$QtyRec)
w_order_log_df1$QtyOrd <- as.integer(w_order_log_df1$QtyOrd)
w_order_log_df1$QtyRec[is.na(w_order_log_df1$QtyRec)] <- 0

# import this as a one off for shadow testing

initial_product_list <- readr::read_csv("secret/Initial_product_list.csv")

initial_product_list  <- initial_product_list %>%
  dplyr::arrange(Drug_name)

# Change relevant columns to dates and numbers
initial_product_list$Packsize <-  as.integer(initial_product_list$Packsize)
initial_product_list$stocklvl <- as.integer(initial_product_list$stocklvl)
initial_product_list$lastordered <- as.Date(initial_product_list$lastordered,
                                            "%d/%m/%Y")

w_trans_log_df <- w_trans_log_db %>%
  dplyr::select(WTranslogID, SisCode, LogDateTime, Qty, Ward, Site) %>%
  dplyr::filter(as.Date(LogDateTime) > three_years_ago) %>%
  dplyr::collect()

w_trans_log_df1 <- w_trans_log_df %>%
  dplyr::mutate_if(is.character, stringr::str_trim) %>%
  dplyr::rename(Drug_code = SisCode, Date = LogDateTime)

# Change relevant columns to dates and numbers
w_trans_log_df1$Date <-  as.Date(as.POSIXct(w_trans_log_df1$Date, 'GMT'))
w_trans_log_df1$Qty <- as.integer(w_trans_log_df1$Qty)


w_requis_df <- readr::read_csv("secret/Outstanding_requisitions.csv")

w_requis_df$DateOrdered <- as.Date(w_requis_df$DateOrdered, "%d/%m/%Y")

w_requis_df1 <-  w_requis_df %>%
  dplyr::mutate(Outstanding = as.integer(Outstanding))
