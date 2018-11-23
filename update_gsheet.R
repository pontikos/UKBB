

library(dplyr)
library(googlesheets)

ukbb_vars <- c(
  'strabismus_count',
  'amblyopia_count',
  'count_ukbb_keratometry',
  'count_ukbb_keratoconus',
  'ukbb_total_number_of_participants',
  'total_number_of_participants',
  'laser_refractive_count',
  'eye_surgery_count',
  'asymmetry_index_unreliable_count',
  'keratometry_unreliable_count',
  'zero_astigmatism_count',
  'ukbb_median_corneal_astigmatism',
  'right_3mm_strong_meridian_48D_count',
  'right_3mm_strong_meridian_48D_prop',
  'right_3mm_strong_meridian_49D_count',
  'right_3mm_strong_meridian_49D_prop',
  'right_3mm_strong_meridian_50D_count',
  'right_3mm_strong_meridian_50D_prop',
  'pct_greater_than_05D_right_3mm_corneal_astigmatism',
  'pct_greater_than_1D_right_3mm_corneal_astigmatism',
  'pct_greater_than_15D_right_3mm_corneal_astigmatism',
  'pct_greater_than_2D_right_3mm_corneal_astigmatism',
  'pct_greater_than_05D_left_3mm_corneal_astigmatism',
  'pct_greater_than_1D_left_3mm_corneal_astigmatism',
  'pct_greater_than_15D_left_3mm_corneal_astigmatism',
  'pct_greater_than_2D_left_3mm_corneal_astigmatism',
  'pct_white',
  'pct_black',
  'pct_asian',
  'pct_chinese',
  'pct_mixed',
  'count_males',
  'pct_males',
  'both_KC_proxy1_count',
  'both_KC_proxy2_count',
  'both_KC_proxy3_count',
  'multivariable_rsquared',
  'multivariable_townsend_pvalue'
)

commify <- function(x) formatC(x, format="d", big.mark=",")
manuscript_vars <- data.frame()
for (v in ukbb_vars) {
  x <- get(v)
  if (is.numeric(x) && x>1000) {x <- commify(x)} else {x <- as.character(x)}
   manuscript_vars <- rbind(manuscript_vars, data.frame(varname=sprintf('#%s#',v),value=x))
}

gk <- '1XGtLNJa2SCLixfiPwczZQGLjCWIlE1AWu3UpJyEe5X8"'
gsheet <- gs_key(k)
#ukbb_var_gsheet <- gs_new(gsheet,ws_title="ukbb_variables", input = manuscript_vars,trim = TRUE, verbose = TRUE)
gs_ws_new(gsheet, input=manuscript_vars, ws_title = "ukbb_variables", trim=TRUE, verbose = TRUE)
gsheet %>% gs_read()
gs_edit_cells(gsheet, input=manuscript_vars, trim=TRUE, verbose=TRUE)
gsheet %>% gs_read()

print(gsheet$sheet_key)

# table 1
gsheet <- gs_key(k)
gsheet %>% gs_read()
gs_ws_new(gsheet, input=table.1, ws_title = "table1", trim=TRUE, verbose = TRUE)

# table 2
gsheet <- gs_key(k)
gsheet %>% gs_read()
gs_ws_new(gsheet, input=table.2, ws_title = "table2", trim=TRUE, verbose = TRUE)

# table 3
gsheet <- gs_key(k)
gs_ws_new(gsheet, input=all.res, ws_title = paste("table_assoc",y.trans,sep='_'), trim=TRUE, verbose = TRUE)

gs_ws_new(gsheet, input=right_corneal_astigmatism, ws_title='right_corneal_astigmatism', trim=TRUE, verbose=TRUE)
gs_ws_new(gsheet, input=right_log_corneal_astigmatism, ws_title='right_log_corneal_astigmatism',trim=TRUE,verbose=TRUE)
gs_ws_new(gsheet, input=left_corneal_astigmatism, ws_title='left_corneal_astigmatism', trim=TRUE, verbose=TRUE)
gs_ws_new(gsheet, input=left_log_corneal_astigmatism, ws_title='left_log_corneal_astigmatism', trim=TRUE, verbose=TRUE)

# 

RL.uni.corneal_astigmatism <- cbind(right_corneal_astigmatism[,1:3],left_corneal_astigmatism[,2:3])
colnames(RL.uni.corneal_astigmatism) <- c('Description','right eye\nunivariate beta (95% CI)','pvalue','left eye\nunivariate beta (95% CI)','pvalue')
gs_ws_delete(gsheet, ws='RL_uni_corneal_astigmatism')
gs_ws_new(gsheet, input=RL.uni.corneal_astigmatism, ws_title='RL_uni_corneal_astigmatism', trim=TRUE, verbose=TRUE)
#gs_edit_cells(gsheet, ws="RL_uni_corneal_astigmatism", input=RL.uni.corneal_astigmatism, anchor = "A1", trim=TRUE, col_names = TRUE)

RL.uni.log_corneal_astigmatism <- cbind(right_log_corneal_astigmatism[,1:3],left_log_corneal_astigmatism[,2:3])
colnames(RL.uni.log_corneal_astigmatism) <- c('Description','right eye\nunivariate beta (95% CI)','pvalue','left eye\nunivariate beta (95% CI)','pvalue')
gs_ws_new(gsheet, input=RL.uni.log_corneal_astigmatism, ws_title='RL_uni_log_corneal_astigmatism', trim=TRUE, verbose=TRUE)

RL.multi.corneal_astigmatism <- cbind(right_corneal_astigmatism[,c(1,4,5)],left_corneal_astigmatism[,4:5])
colnames(RL.multi.corneal_astigmatism) <- c('Description','right eye\nmultivariate beta (95% CI)','pvalue','left eye\nmultivariate beta (95% CI)','pvalue')
gs_ws_delete(gsheet, ws='RL_multi_corneal_astigmatism')
gsheet <- gs_key(k)
gs_ws_new(gsheet, input=RL.multi.corneal_astigmatism, ws_title='RL_multi_corneal_astigmatism', trim=TRUE, verbose=TRUE)

RL.multi.log_corneal_astigmatism <- cbind(right_log_corneal_astigmatism[,c(1,4,5)],left_log_corneal_astigmatism[,4:5])
colnames(RL.multi.log_corneal_astigmatism) <- c('Description','right eye\nmultivariate beta (95% CI)','pvalue','left eye\nmultivariate beta (95% CI)','pvalue')
gs_ws_delete(gsheet, ws='RL_multi_log_corneal_astigmatism')
gsheet <- gs_key(k)
gs_ws_new(gsheet, input=RL.multi.log_corneal_astigmatism, ws_title='RL_multi_log_corneal_astigmatism', trim=TRUE, verbose=TRUE)
