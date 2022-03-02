
# Basic Network Growth Model:  Converting OutPool to network members, small or big side.
# Rob Goldman
# Honlulu Hawaii, December 1
# TERMINOLOGY: 
# SmallSide (small_side), bigside (big_sides).
#0.Outpool size (Global Internet: 4.7B, China :0.86B, US: 269.5MM)
#Pool growth rate (monthly, key missing assumption, set to zero)

#1.traffic onramps (denoted in OR,measured in $ and users per month per network side).
#2.network engagement (denoted NE), measured in cow mmunications per month per network side and direction
#4 funnel conversion (denoted FC), measured in members per communication by network side and direction 

start_month <- as.Date("2022-01-01")
ending_month <- as.Date("2027-01-01")

outpool_size <- 4700000000  # 4.7B baaybee  Where should we factor this in? 
outPool_growth <- 0 # Conservative assumption

company_name="MyPlace"

big_side="Guests"
starting_big=0

OR_paid_big= 600 # Monthly users acquired through paid marketing big members
OR_paid_big_usd = 60000 # Monthly paid marketing to acquire big side members
OR_paid_big_CPM =10

LTV_month1_churn_big = .65
LTV_ongoing_churn_big = .15

small_side= "Hosts"
starting_small=1

OR_paid_small_usd = 0  # Monthly paid marketing to aquire small side members
OR_paid_small = 1 # Monthly users acquired through paid marketing small members

LTV_month1_churn_small= .35
LTV_ongoing_churn_small = .10

OR_organic_small = 1 # Monthly users acquired organically small members
OR_organic_big = 1 # Monthly users acquired organically big members

OR_organic_small_usd = 0
OR_organic_big_usd = 0

NE_big_shares_out = .15 # Monthly number of shares out of the network
NE_big_shares_in = .1 # Monthly number of shares in the network
NE_big_shares_out_audience = 1 # Average number of people seeing an out of network share
NE_big_shares_in_audience =10 #Average number of people seeing an in-network share

NE_small_shares_out = 1 # Monthly number of shares out of the network %sharing*shares/sharer/month
NE_small_shares_in = 1 # Monthly number of shares in the network
NE_small_shares_out_audience = 100 # Average number of people seeing an out of network share
NE_small_shares_in_audience = 10 #Average number of people seeing an in-network share

FC_shared_big_vbig = .05 * .15            #Shared big users via big shares
FC_shared_small_vbig = 1          #Shared small users v big shares 
FC_shared_transactions_vbig=.0001
FC_shared_engagements_vbig=.1

FC_shared_big_vsmall= .01 * .07     #Shared big users v small shares:
FC_shared_small_vsmall =.001 * .05   #Small users via small shares CTR - 1%, lp conversoin 5%
FC_shared_transactions_vsmall = .01
FC_shared_engagements_vsmall=.1


# Kfactor Small = small shares -> small users
#                 big shares -> small users
#                 small shares -> big users -> big shares -> small users
#                 big shares -> big users -> big_shares -> small users

k_small= (NE_small_shares_out * NE_small_shares_out_audience * FC_shared_small_vsmall) + (NE_big_shares_out * NE_big_shares_out_audience * FC_shared_small_vbig) 

