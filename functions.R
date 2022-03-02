add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
add_paid_shares<-function(imonthly_model,
                          i_cpm,
                          i_usd,
                          i_side,
                          i_source,
                          i_startMonth,
                          i_endMonth
                          ){
  processing_month<-i_startMonth

  while (processing_month<i_endMonth){
          imonthly_model<- imonthly_model %>% add_row(
            "month"=processing_month,
            "retention_month"=processing_month,
            "starting"=0,
            "churned"=0,
            "returning"=0,
            "new"=0,
            "resurrected"=0,
            "ending"=0,
            "shares_out"=i_usd/i_cpm,
            "shares_in"=0,
            "impressions_out"=(i_usd/i_cpm) * 1000,
            "impressions_in"=0,
            "engagements"= 0,
            "transactions"=1,
            "revenue"=-1*i_usd,
            "traffic_source"=i_source,
            "network_side"="Company"
          )
          processing_month<-add.months(processing_month,1)
  }
  return(imonthly_model)
}
add_subscription_revenue<- function( 
  imonthly_model,
  i_subs_per_suber,
  i_rev_per_sub,
  i_sub_rate,
  i_take_rate = .1,
  i_startMonth,
  i_endMonth,
  i_side
)
{
  processing_month<-i_startMonth
  # Get all the starting members each month and use that to calculate subscription Revenue
  
  monthly_starters<- imonthly_model %>%
    group_by (month) %>%
    summarize(
      starting=sum(starting_big),
    ) 
  
  
  while (processing_month<i_endMonth){
    # Get starting User Count to add subscription revenue data
    
    starting_this_month <- monthly_starters %>%
      filter (month==processing_month) %>%
      select (starting)
    
    imonthly_model<- imonthly_model %>% add_row(
      "month"=processing_month,
      "retention_month"=processing_month,
      "starting"=0,
      "churned"=0,
      "returning"=0,
      "new"=0,
      "resurrected"=0,
      "ending"=0,
      "shares_out"=0,
      "shares_in"=0,
      "impressions_out"=0,
      "impressions_in"=0,
      "engagements"= 0,
      "transactions"=starting_this_month$starting[1]*i_sub_rate,
      "revenue"=i_rev_per_sub*starting,
      "traffic_source"="",
      "network_side"=i_side
    )
    processing_month<-add.months(processing_month,1)
  }
  return(imonthly_model)
}

add_members<-function(imonthly_model,
                      i_side,
                      i_source,
                      i_start,
                      i_new,
                      i_churn1_sm,
                      i_churnX_sm,
                      i_churn1_bg,
                      i_churnX_bg,
                      i_shareout_sm,
                      i_sharein_sm,
                      i_shareout_bg,
                      i_sharein_bg,
                      i_audout_sm,
                      i_audin_sm,
                      i_audout_bg,
                      i_audin_bg,
                      i_conv_small_sm,
                      i_conv_big_sm,
                      i_conv_transaction_sm,
                      i_conv_engagement_sm,
                      i_conv_small_bg,
                      i_conv_big_bg,
                      i_conv_transaction_bg,
                      i_conv_engagement_bg,
                      i_rev_per_sub,
                      i_rev_sub_rate,
                      i_startMonth,
                      i_endMonth){
  
  # initialize loop values
  processing_month<-i_startMonth
  retention_month<-processing_month
  
  # sharing variables 
  shared_small_via_small_lm <- 0
  shared_new_small_via_small_lm <- 0
  
  shared_small_via_big_lm <- 0
  shared_new_small_via_big_lm <- 0
  
  shared_big_via_big_lm <- 0
  shared_new_big_via_big_lm <- 0
  
  shared_big_via_small_lm <- 0
  shared_new_big_via_small_lm <- 0
  
  # to count users and compute churn from the prior month
  t_lastMonth = i_start
  t_new_lastMonth=0
  
  # Nested loop to insert new users on month 1 and then follow them over time as they churn. Do that for each month 1. 
  
  while (processing_month<i_endMonth){
    while (retention_month<i_endMonth){
      if(retention_month==processing_month) {
        # Insert New Users on the first month
        new=i_new
      }
      else {
        new=0
      }
      
      if (i_side==small_side) {
        churned<--1*((t_lastMonth-t_new_lastMonth)*i_churnX_sm+(t_new_lastMonth*i_churn1_sm))
        starting=t_lastMonth
        returning<-t_lastMonth+churned
        resurrected<-0
        ending=returning+new+resurrected
        shares_out=t_lastMonth * i_shareout_sm
        shares_in=t_lastMonth * i_sharein_sm
        imps_out=t_lastMonth * i_shareout_sm* i_audout_sm
        imps_in=t_lastMonth * i_sharein_sm * i_audin_sm
        engage=(t_lastMonth * i_sharein_sm * i_audin_sm) * i_conv_engagement_sm
        transact=(t_lastMonth * i_sharein_sm * i_audin_sm) * i_conv_transaction_sm
        revenue=(t_lastMonth * i_sharein_sm * i_audin_sm) * i_conv_transaction_sm*Rev_rev_per_transaction
      }
      else {
        churned<--1*((t_lastMonth-t_new_lastMonth)*i_churnX_bg+(t_new_lastMonth*i_churn1_bg))
        starting=t_lastMonth
        returning<-t_lastMonth+churned
        resurrected<-0
        ending=returning+new+resurrected
        shares_out=t_lastMonth * i_shareout_bg
        shares_in=t_lastMonth * i_sharein_bg
        imps_out=t_lastMonth * i_shareout_bg* i_audout_bg
        imps_in=t_lastMonth * i_sharein_bg * i_audin_bg
        engage=(t_lastMonth * i_sharein_bg * i_audin_bg) * i_conv_engagement_bg
        transact=t_lastMonth*i_rev_sub_rate
        revenue=t_lastMonth*i_rev_sub_rate*i_rev_per_sub
      }
      
      # insert users into monthly model 
      # transact=(t_lastMonth * i_sharein_bg * i_audin_bg) * i_conv_transaction_bg
      # revenue=(t_lastMonth * i_sharein_bg * i_audin_bg) * i_conv_transaction_bg * Rev_rev_per_transaction 
      
      imonthly_model<- imonthly_model %>% add_row(
        "month"=processing_month,
        "retention_month"=retention_month,
        "starting"=starting,
        "churned"=churned,
        "returning"=returning,
        "new"=new,
        "resurrected"=resurrected,
        "ending"=ending,
        "shares_out"=shares_out,
        "shares_in"=shares_in,
        "impressions_out"=imps_out,
        "impressions_in"=imps_in,
        "engagements"= engage,
        "transactions"=transact,
        "revenue"=revenue,
        "traffic_source"=i_source,
        "network_side"=i_side
      )
      
      ## Sharing
      
      # We apply sharing assumptions to "starting" users. 
      # Many different impacts from sharing: 
      # User Growth - 4 separate tracks - shared_small_via_small, shared_small_via_big, shared_big_via_small, shared_small_via_big
      # Network engagement - Engagement happens the month of the share
      # Network Monetization - Happens on the month of the share 
      
      # Share impact on small via small
      if (i_side==small_side){
        shared_small_via_small_new=((t_lastMonth + shared_small_via_small_lm + shared_small_via_big_lm) * i_shareout_sm * i_audout_sm * i_conv_small_sm) 
      }
      else {
        shared_small_via_small_new= ((shared_small_via_small_lm +shared_small_via_big_lm) * i_shareout_sm * i_audout_sm * i_conv_small_sm )
      }
      small_churn =
        -1 * (
          (shared_small_via_small_lm - shared_new_small_via_small_lm) * i_churnX_sm +
            (shared_new_small_via_small_lm * i_churn1_sm)
        )
      small_returning <- shared_small_via_small_lm + small_churn
      small_resurrected <- 0
      small_total<- small_returning+small_resurrected+shared_small_via_small_new
      imonthly_model <- imonthly_model %>% add_row(
        "month" = add.months(processing_month, 0),
        "retention_month"=retention_month,
        "starting" = shared_small_via_small_lm,
        "churned" = small_churn,
        "returning" = small_returning,
        "new" = shared_small_via_small_new,
        "resurrected" = small_resurrected,
        "ending" = small_total,
        "shares_out" = shared_small_via_small_lm * i_shareout_sm,
        "shares_in" = shared_small_via_small_lm * i_sharein_sm,
        "impressions_out" = shared_small_via_small_lm * i_shareout_sm * i_audout_sm,
        "impressions_in" = shared_small_via_small_lm * i_sharein_sm * i_audin_sm,
        "engagements"=shared_small_via_small_lm * i_sharein_sm * i_audin_sm * i_conv_engagement_sm,
         "traffic_source" = paste(small_side, "shares"),
        "network_side" = small_side
      )
      # "transactions"=shared_small_via_small_lm * i_sharein_sm * i_audin_sm * i_conv_transaction_sm,
      # "revenue"=shared_small_via_small_lm * i_sharein_sm * i_audin_sm * i_conv_transaction_sm*Rev_rev_per_transaction,
      
      # Impact on big_via_small
      if (i_side==small_side){
        shared_big_via_small_new=(t_lastMonth + shared_small_via_small_lm + shared_small_via_big_lm ) * i_shareout_sm * i_audout_sm * i_conv_big_sm 
      }
      else {
        shared_big_via_small_new= (shared_small_via_small_lm + shared_small_via_big_lm ) * i_shareout_sm * i_audout_sm * i_conv_big_sm
      }
      big_churn =
        -1 * (
          (shared_big_via_small_lm - shared_new_big_via_small_lm) * i_churnX_bg +
            (shared_new_big_via_small_lm * i_churn1_bg)
        )
      big_returning <- shared_big_via_small_lm + big_churn
      big_resurrected <- 0
      big_total<- big_returning+big_resurrected+shared_big_via_small_new
      
      imonthly_model <- imonthly_model %>% add_row(
        "month" = add.months(processing_month, 0),
        "retention_month"=retention_month,
        "starting" = shared_big_via_small_lm,
        "churned" = big_churn,
        "returning" = big_returning,
        "new" = shared_big_via_small_new,
        "resurrected" = big_resurrected,
        "ending" = big_total,
        "shares_out" = shared_big_via_small_lm * i_shareout_bg,
        "shares_in" = shared_big_via_small_lm * i_sharein_bg,
        "impressions_out" = shared_big_via_small_lm * i_shareout_bg * i_audout_bg,
        "impressions_in" = shared_big_via_small_lm * i_sharein_bg * i_audin_bg,
        "engagements"=shared_big_via_small_lm * i_sharein_bg * i_audin_bg * i_conv_engagement_bg,
        "transactions"=shared_big_via_small_lm * i_sharein_bg * i_audin_bg * i_conv_transaction_bg,
        "revenue"=shared_big_via_small_lm * i_sharein_bg * i_audin_bg * i_conv_transaction_bg*Rev_rev_per_transaction,
        "transactions"=shared_big_via_small_lm * i_rev_sub_rate,
        "revenue"=shared_big_via_small_lm * i_rev_per_sub* i_rev_sub_rate,
        "traffic_source" = paste(small_side, "shares"),
        "network_side" = big_side
      )
      # "transactions"=shared_big_via_small_lm * i_sharein_bg * i_audin_bg * i_conv_transaction_bg,
      # "revenue"=shared_big_via_small_lm * i_sharein_bg * i_audin_bg * i_conv_transaction_bg*Rev_rev_per_transaction,
      
      # impact on small side via big side
      if (i_side==small_side){
        shared_small_via_big_new=((shared_big_via_big_lm + shared_big_via_small_lm) * i_shareout_bg * i_audout_bg * i_conv_small_bg)  
      }
      else {
        shared_small_via_big_new= ((t_lastMonth+shared_big_via_big_lm + shared_big_via_small_lm) * i_shareout_bg * i_audout_bg * i_conv_small_bg)  
      }
      
      small_vb_churn =
        -1 * (
          (shared_small_via_big_lm - shared_new_small_via_big_lm) * i_churnX_sm +
            (shared_new_small_via_big_lm * i_churn1_sm)
        )
      small_vb_returning <- shared_small_via_big_lm + small_vb_churn
      small_vb_resurrected <- 0
      small_vb_total<- small_vb_returning+small_vb_resurrected+shared_small_via_big_new
      
      imonthly_model <- imonthly_model %>% add_row(
        "month" = add.months(processing_month, 0),
        "retention_month"=retention_month,
        "starting" = shared_small_via_big_lm,
        "churned" = small_vb_churn,
        "returning" = small_vb_returning,
        "new" = shared_small_via_big_new,
        "resurrected" = small_vb_resurrected,
        "ending" = small_vb_total,
        "shares_out" = shared_small_via_big_lm * i_shareout_sm,
        "shares_in" = shared_small_via_big_lm * i_sharein_sm,
        "impressions_out" = shared_small_via_big_lm * i_shareout_sm * i_audout_sm,
        "impressions_in" = shared_small_via_big_lm * i_sharein_sm * i_audin_sm,
        "engagements"=shared_small_via_big_lm * i_sharein_sm * i_audin_sm * i_conv_engagement_sm,
        "transactions"=shared_small_via_big_lm * i_sharein_sm * i_audin_sm * i_conv_transaction_sm,
        "revenue"=shared_small_via_big_lm * i_sharein_sm * i_audin_sm * i_conv_transaction_sm * Rev_rev_per_transaction,
        "traffic_source" = paste(big_side, "shares"),
        "network_side" = small_side
      )
      
      # Impact on big side via big side
      if (i_side==small_side){
        shared_big_via_big_new=((shared_big_via_big_lm + shared_big_via_small_lm)* i_shareout_bg * i_audout_bg * i_conv_big_bg)  
      }
      else {
        shared_big_via_big_new= ((t_lastMonth+shared_big_via_big_lm + shared_big_via_small_lm) * i_shareout_bg * i_audout_bg * i_conv_big_bg)  
      }
      big_vb_churn =
        -1 * (
          (shared_big_via_big_lm - shared_new_big_via_big_lm ) * i_churnX_bg +
            (shared_new_big_via_big_lm * i_churn1_bg)
        )
      big_vb_returning <- shared_big_via_big_lm + big_vb_churn
      big_vb_resurrected <- 0
      big_vb_total<- big_vb_returning+big_vb_resurrected+shared_big_via_big_new
      
      imonthly_model <- imonthly_model %>% add_row(
        "month" = add.months(processing_month, 0),
        "retention_month"=retention_month,
        "starting" = shared_big_via_big_lm,
        "churned" = big_vb_churn,
        "returning" = big_vb_returning,
        "new" = shared_big_via_big_new,
        "resurrected" = big_vb_resurrected,
        "ending" = big_vb_total,
        "shares_out" = shared_big_via_big_lm * i_shareout_bg,
        "shares_in" = shared_big_via_big_lm * i_sharein_bg,
        "impressions_out" = shared_big_via_big_lm * i_shareout_bg * i_audout_bg,
        "impressions_in" = shared_big_via_big_lm * i_sharein_bg * i_audin_bg,
        "engagements"=shared_big_via_big_lm * i_sharein_bg * i_audin_bg * i_conv_engagement_bg,
        "transactions"=shared_big_via_big_lm * i_rev_sub_rate,
        "revenue"=shared_big_via_big_lm * i_rev_per_sub * i_rev_sub_rate,
        "traffic_source" = paste(big_side, "shares"),
        "network_side" = big_side
      )
      # "transactions"=shared_big_via_big_lm * i_sharein_bg * i_audin_bg * i_conv_transaction_bg,
      # "revenue"=shared_big_via_big_lm * i_sharein_bg * i_audin_bg * i_conv_transaction_bg*Rev_rev_per_transaction,
      
      
      # Update variables for the next loop 
      t_lastMonth = ending
      t_new_lastMonth= new
      
      shared_big_via_small_lm = big_total
      shared_new_big_via_small_lm=shared_big_via_small_new
      
      shared_small_via_small_lm = small_total
      shared_new_small_via_small_lm =shared_small_via_small_new
      
      shared_small_via_big_lm = small_vb_total
      shared_new_small_via_big_lm =shared_small_via_big_new
      
      shared_big_via_big_lm = big_vb_total
      shared_new_big_via_big_lm =shared_big_via_big_new
      
      retention_month <- add.months(retention_month,1)
    }
    
    # reset for new acquisition month 
    t_new_lastMonth=0
    t_lastMonth=0
    
    shared_small_via_big_lm = 0
    shared_new_small_via_big_lm =0
    
    shared_big_via_small_lm = 0
    shared_new_big_via_small_lm =0
    
    shared_big_via_big_lm =0
    shared_new_big_via_big_lm=0
    
    shared_small_via_small_lm = 0
    shared_new_small_via_small_lm =0
    
    processing_month <- add.months(processing_month,1)
    retention_month<-processing_month
  }
  return(imonthly_model)
}


