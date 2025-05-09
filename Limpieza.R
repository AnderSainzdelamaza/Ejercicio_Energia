# 3. Transformación de variables
df_pred_10 <- df %>%
  dplyr::select(TotalC2010, TotalP2010, TotalE2010, GDP2010)   
colnames(df_pred_10) <- c("TotalC", "TotalP", "TotalE", "GDP")  

df_pred_11 <- df %>%
  dplyr::select(TotalC2011, TotalP2011, TotalE2011, GDP2011)
colnames(df_pred_11) <- c("TotalC", "TotalP", "TotalE", "GDP")

df_pred_12 <- df %>%
  dplyr::select(TotalC2012, TotalP2012, TotalE2012, GDP2012)
colnames(df_pred_12) <- c("TotalC", "TotalP", "TotalE", "GDP")

df_pred_13 <- df %>%
  dplyr::select(TotalC2013, TotalP2013, TotalE2013, GDP2013)
colnames(df_pred_13) <- c("TotalC", "TotalP", "TotalE", "GDP")

df_pred_14 <- df %>%
  dplyr::select(TotalC2014, TotalP2014, TotalE2014, GDP2014)
colnames(df_pred_14) <- c("TotalC", "TotalP", "TotalE", "GDP")

# creamos un dataframe con la información de todos los dataframes por años
# para prepararlo una modelización
df_pred <- rbind(df_pred_10, df_pred_11, df_pred_12, df_pred_13, df_pred_14)
