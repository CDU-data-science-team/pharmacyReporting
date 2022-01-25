
# run at the top of the Shiny server

three_years_ago <- Sys.Date() - 366 * 3

two_years_ago <- Sys.Date() - 366 * 2

one_week_ago <- Sys.Date() - 7

pharmacy_conn <- odbc::dbConnect(odbc::odbc(),
                                 Driver = "SQL Server",
                                 Server = "DBPharmacyEMIS",
                                 Database = "EMIS_HAP_LIVE",
                                 Trusted_Connection = "True")

pharmacy_report_conn <- odbc::dbConnect(odbc::odbc(),
                                        Driver = "SQL Server",
                                        Server = "DBPharmacyEMIS",
                                        Database = "EMIS_HAP_REPORT_LIVE",
                                        Trusted_Connection = TRUE)

Outstanding_orders <- structure(
  list(Ord_index = 30, Ord_daycnt = 407, Ord_quant = 0, 
       Ord_del_daycnt = -1), class = "data.frame", row.names = c(NA, -1L)
)

# Make table for different site numbers - this is static data and never changes 
# so can be set up once within the system

Site_table <- dplyr::tibble(
  Site = as.numeric(c("100", "240", "241", "242", "878", "958")),
  SiteID = as.numeric(c("1476", "15", "260", "19", "1145", "259")),
  LocationID_Site = as.numeric(c("1476", "15", "260", "19", "1145", "259")))

product_nsvCode_db <- dplyr::tbl(pharmacy_report_conn, 
                                 dbplyr::in_schema("dbo", "rProductStock"))

product_nsvCode_df <- product_nsvCode_db |>
  dplyr::select(ProductStockID, DrugID, LocationID_Site, InUse, NSVCode, Stocked) |>
  dplyr::filter(InUse == "Y" & Stocked == "Y" & LocationID_Site != 260) |> 
  dplyr::collect()

product_nsvCode_df <-  product_nsvCode_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim)

product_db <- dplyr::tbl(pharmacy_report_conn, dbplyr::in_schema("dbo", "rProduct"))

product_df <- product_db %>% 
  dplyr::select(NSVCode, StoresDescription, ReOrderPacksize, PrintForm) %>% 
  dplyr::collect()

product_df <- product_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim)

product_stock_db <- dplyr::tbl(pharmacy_conn, 
                               dbplyr::in_schema("icwsys", "ProductStock"))

product_stock_df <- product_stock_db %>% 
  dplyr::select(DrugID, ProductID, LocationID_Site, inuse, sisstock, ordercycle, 
                supcode, altsupcode, 
                lastordered, message, stocklvl) %>% 
  dplyr::filter(inuse == "Y" & sisstock == "Y" & LocationID_Site != 260) %>% 
  dplyr::collect()

product_stock_df<-  product_stock_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim)

###

site_product_db <- dplyr::tbl(pharmacy_conn, dbplyr::in_schema("icwsys", "SiteProductData"))

site_product_df <- site_product_db %>% 
  dplyr::select(DrugID, ProductID, siscode, storesdescription, tradename, convfact, printformv) %>% 
  dplyr::collect()

site_product_df <- site_product_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim)

w_supplier_profile_db <- dplyr::tbl(pharmacy_conn, dbplyr::in_schema("icwsys", "WSupplierProfile"))

w_supplier_profile_df <- w_supplier_profile_db %>% 
  dplyr::select(NSVCode, SupCode, LocationID_Site, SupplierTradeName) %>% 
  dplyr::collect()

w_supplier_profile_df <- w_supplier_profile_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim)

###

w_supplier_2_db <- dplyr::tbl(pharmacy_conn, dbplyr::in_schema("icwsys", "WSupplier2")) 

w_supplier_2_df <- w_supplier_2_db %>%
  dplyr::select(SiteID, Code, Description, InUse) %>% 
  dplyr::filter(InUse == "1") %>% 
  dplyr::collect()

w_supplier_2_df <-  w_supplier_2_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim)

# Join product and supplier tablets together

product_nsv <- dplyr::left_join(product_stock_df, product_nsvCode_df, 
                                by = c("DrugID" = "DrugID", 
                                       "LocationID_Site" = "LocationID_Site"))

product_supplier <- dplyr::left_join(product_nsv, w_supplier_profile_df, 
                                     by = c("NSVCode" = "NSVCode",
                                            "LocationID_Site" = "LocationID_Site",
                                            "supcode" = "SupCode"))

product_profile1 <- dplyr::left_join(product_supplier, product_df,  
                                     by = c("NSVCode" = "NSVCode"))

product_profile <- dplyr::left_join(product_profile1, Site_table, 
                                    by = c("LocationID_Site" = "LocationID_Site"))

product_profile <- product_profile %>% 
  dplyr::select(Site, NSVCode, StoresDescription, SupplierTradeName, ReOrderPacksize, PrintForm, 
         stocklvl, lastordered, supcode, ordercycle, altsupcode)

product_sup_profile <- product_profile %>% 
  dplyr::rename(Drug_code = NSVCode, Supplier_name = supcode, Drug_name = StoresDescription, Packsize = ReOrderPacksize) 

product_sup_profile <- product_sup_profile %>% 
  dplyr::arrange(Drug_name)


# Change relevant columns to dates and numbers
product_sup_profile$Packsize <-  as.integer(product_sup_profile$Packsize)
product_sup_profile$stocklvl <- as.integer(product_sup_profile$stocklvl)
product_sup_profile$lastordered <- as.Date(product_sup_profile$lastordered, 
                                           "%d%m%Y")

w_order_log_db <- dplyr::tbl(pharmacy_conn, dbplyr::in_schema("icwsys", "WOrderLog"))

w_order_log_df <- w_order_log_db %>% 
  dplyr::select(WOrderLogID, OrderNum, SisCode, DateOrdered, DateReceived, QtyOrd, QtyRec,
                SupCode, Site, Kind) %>% 
  dplyr::filter(Kind %in% c("O", "I", "R"),
                DateReceived > two_years_ago | DateOrdered > two_years_ago) %>% 
  dplyr::arrange(dplyr::desc(WOrderLogID)) %>% 
  dplyr::collect()

w_order_log_df1 <- w_order_log_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim) %>% 
  dplyr::rename(Drug_code = SisCode, Supplier_name = SupCode)

# Change relevant columns to dates and numbers
w_order_log_df1$DateReceived <-  as.Date(as.POSIXct(w_order_log_df1$DateReceived, 'GMT'))
w_order_log_df1$DateOrdered <-  as.Date(as.POSIXct(w_order_log_df1$DateOrdered, 'GMT'))
w_order_log_df1$QtyRec <- as.integer(w_order_log_df1$QtyRec)
w_order_log_df1$QtyOrd <- as.integer(w_order_log_df1$QtyOrd)
w_order_log_df1$QtyRec[is.na(w_order_log_df1$QtyRec)] <- 0

w_trans_log_db <- dplyr::tbl(pharmacy_conn, dbplyr::in_schema("icwsys", "WTransLog"))

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


w_requis_db <- dplyr::tbl(pharmacy_conn, dbplyr::in_schema("icwsys", "WRequis")) 

w_requis_df <- w_requis_db %>% 
  dplyr::select(WRequisID, Code, Outstanding, SiteID, OrdDate, SupCode, ToFollow) %>% 
  dplyr::filter(ToFollow == "1") %>% 
  dplyr::collect()

w_requis_df$OrdDate <- as.Date(w_requis_df$OrdDate, "%d%m%Y")

w_requis_df1 <-  w_requis_df %>% 
  dplyr::mutate_if(is.character, stringr::str_trim) %>% 
  dplyr::rename(Drug_code = Code, Supplier_name = SupCode, DateOrdered =  OrdDate) %>% 
  dplyr::mutate(Outstanding = as.numeric(Outstanding)) |> 
  dplyr::filter(DateOrdered > one_week_ago)
