##
##   Blackjack basic strategy calculator
##

require( "tidyverse")


##
##  Calculate the transition matrix for the dealer strategy
##  Here the dealer hits on soft 17
##

cards <- tibble( 
  value = c( 1,2,3,4,5,6,7,8,9,10,10,10,10),
  name  = c("A","2","3","4","5","6","7","8","9","10","J","Q","K"),
  csoft  = c( T, rep(F,12))
)

cards$name <- factor( cards$name, cards$name)

dealer <- expand_grid( cur_tot = 1:21, final_tot = 8:26, soft = c(F,T), final_soft = c(F,T), p= NA)  |> 
  mutate( final_p = ifelse( final_tot == cur_tot & final_soft == soft, 1, 0 ) ) |> 
  mutate( p = ifelse( final_tot < cur_tot, 0, p )) |> # impossible or hit
  mutate( p = ifelse( cur_tot >= 17, final_p, p )) |> #stand
  mutate( p = ifelse( soft & cur_tot >= 8 & cur_tot <= 11, final_p, p )) |> 
  select( -final_p )
  

# totals here are the "hard" totals, treating aces as 1

for( cur_tot_update in 16:1)
{
  one_card <- expand_grid( filter( dealer, cur_tot == !!cur_tot_update, is.na(p) ), cards) |> 
    select( -p ) |> 
    mutate( next_tot = cur_tot+value, next_soft = soft | csoft ) |> 
    mutate( final_p = ifelse( final_tot == next_tot & final_soft == next_soft, 1, 0 ) ) |> 
    left_join( dealer, by=join_by( next_tot == cur_tot, next_soft == soft, final_tot, final_soft )) |> 
    mutate( p = ifelse( next_tot > final_tot, 0, p)) |> 
    mutate( p = ifelse( next_tot > 21, final_p, p)) 
  
  dealer_update <- one_card |> group_by( cur_tot, final_tot, soft, final_soft ) |> 
    summarize( newp = mean( p), .groups =  "drop") 
  
  #  dealer_update |> print( n=100)
  
  dealer <- left_join( dealer, dealer_update, by=join_by(cur_tot, final_tot, soft, final_soft)) |> 
    mutate( p =coalesce(p,newp)) |> 
    select(-newp) 
  
}

# sanity check total probability

dealer |> group_by( soft, cur_tot ) |> 
  summarize( ss = sum(p)) |>  
  mutate( is1 = abs( ss-1 )<1e-6 ) |> 
  pull( is1 ) |> 
  all()


dealer <- dealer |> 
  mutate( 
      cur_soft_tot   = ifelse( soft & cur_tot <= 11, cur_tot+10, cur_tot),
      final_soft_tot = ifelse( final_soft & final_tot <= 11, final_tot+10, final_tot ) 
  )


  
##
## Plot the transition matrix
##

ggplot( dealer, aes(cur_tot, final_tot,fill=p)) +
  geom_tile()+
  scale_fill_gradient(low="white", high="blue") +
  facet_wrap( soft ~final_soft)


##
##  Dealer Outcome-by-upcard table
##


# Figure out dealer outcomes indexed by dealer upcard
# This version includes the probability that dealer gets initial blackjack

upcard <- cards |> rename( soft=csoft, cur_tot= value, upcard=name) |> 
  left_join( dealer, relationship = "many-to-many" ) 

upcard |> filter( final_soft_tot ==21)

upcard |> filter( cur_tot == 1 | cur_tot == 10, final_tot==11, final_soft) |> 
  pull( p ) |> 
  sum() /13


#  Calculate the bust probability given the card that's showing

bust_p <- upcard |> 
  filter( final_tot > 21 ) |> 
  group_by( upcard ) |> 
  summarise( bust_p = sum(p), cur_tot=last( cur_tot))


ggplot(bust_p, aes(upcard, bust_p)) + geom_point()

# overall bust probability

bust_p$bust_p |> mean()


# Dealer outcomes indexed by dealer upcard
# This version is only playable upcards
# playable dealer hands by upcard (remove initial blackjacks)

dealer_hands <- expand_grid( 
  rename( cards, soft_up=csoft, value_up = value, upcard=name), 
  rename( cards, soft_ho=csoft, value_ho = value, hole=name) 
) |> 
  mutate( soft = soft_up | soft_ho, cur_tot = value_up + value_ho ) |> 
  filter( value_up != 10 | value_ho != 1, value_up != 1 | value_ho != 10 ) |> 
  select( upcard, hole, soft, cur_tot ) |> 
  left_join( dealer, relationship = "many-to-many")

upcard_playable <- dealer_hands |> 
  group_by( upcard, final_soft, final_tot ) |> 
  summarize( p = mean( p )) |> 
  mutate( final_soft_tot = ifelse( final_soft & final_tot <= 11, final_tot+10, final_tot )) 


# There is a choice here: upcard or upcard playable?  

dealer_outcomes <- upcard_playable |> 
  filter( p > 0 ) |> 
  mutate( outcome_val = ifelse( final_soft_tot > 21, 0, final_soft_tot )) |> 
  group_by( upcard, outcome_val ) |> 
  summarize( p = sum(p), .groups="drop") |> 
  mutate( outcome = ifelse( outcome_val > 0, as.character(outcome_val), "bust" ))

outcome_fac <- c( "17","18","19","20","21","bust")

dealer_outcomes$outcome <- factor( dealer_outcomes$outcome, outcome_fac)

ggplot( dealer_outcomes, aes( upcard, p, fill=outcome) ) +
  geom_col( position="stack") + 
  scale_fill_brewer( palette ="Oranges") +
  labs(title = "Dealer Outcome Probabilities",
       x = "Dealer Upcard",
       y = "Probability",
       fill = "Outcome") +
  theme_minimal()



#
# Simplified BJ:
#   No initial BJ win
#   No double, no split
#


dealer_outcomes_wide <- dealer_outcomes |> 
  select( -outcome_val ) |>
  pivot_wider( names_from = outcome, values_from = p )

maxt = 31

# basic setup and bust edge

hit_stand <- expand_grid( upcard = cards$name, cur_tot = 1:maxt, soft = c(F,T),
                     stand_w = NA, stand_t =NA, stand_l = NA, stand_ex = NA, 
                     hit_w = NA, hit_t= NA, hit_l = NA, hit_ex = NA,
                     w=NA, t = NA, l=NA, ex = NA,
                     decision=NA ) |> 
  left_join( dealer_outcomes_wide, by=join_by( upcard)) |> 
  mutate( 
    cur_soft_tot = ifelse( soft & cur_tot <= 11, cur_tot+10, cur_tot),
    decision = ifelse( cur_tot > 21, "bust", decision ),
    w = ifelse( cur_tot > 21, 0, w ),
    t = ifelse( cur_tot > 21, 0, t ),
    l = ifelse( cur_tot > 21, 1, l ),
    ex = ifelse( cur_tot > 21, w-l, ex )
  ) 

# stand edge
hit_stand <-hit_stand |> 
  rowwise() |> 
  mutate( 
    vec     = list( c( `bust`, rep(0,16), `17`,`18`,`19`,`20`,`21`, rep(0,maxt-22+2))),
    stand_w = ifelse( cur_soft_tot > 21, 0, sum( vec[1:cur_soft_tot])), 
    stand_t = ifelse( cur_soft_tot > 21, 0, vec[cur_soft_tot+1]),
    stand_l = ifelse( cur_soft_tot > 21, 1, sum( vec[(cur_soft_tot+2):(maxt+2)])),
    stand_ex = stand_w-stand_l
  ) |> 
  select( -vec )

hit_stand |> mutate( check = stand_w + stand_t + stand_l ) |> pull( check)

for( tot_update in 21:1 )
{
 
  hit_stand_slim <- hit_stand |> select( upcard, cur_tot, soft, w,t,l,ex )
  
  one_card <- expand_grid( filter( hit_stand, cur_tot == !!tot_update), cards) |> 
    select( -w,-l,-t,-ex) |> 
    mutate( next_tot = cur_tot+value, next_soft = soft | csoft ) |> 
    left_join( hit_stand_slim, by=join_by( next_tot == cur_tot, next_soft == soft, upcard )) 

  hit_stand_update <- one_card |> group_by( upcard, cur_tot, soft  ) |> 
    summarize(
      hit_w_up = mean(w),
      hit_t_up = mean(t),
      hit_l_up = mean(l),
      .groups = "drop"
    ) 

  
  #  dealer_update |> print( n=100)
  
  hit_stand <- left_join( hit_stand, hit_stand_update, by=join_by(cur_tot, soft, upcard)) |> 
    mutate( 
      hit_w =coalesce(hit_w,hit_w_up),
      hit_t =coalesce(hit_t,hit_t_up),
      hit_l =coalesce(hit_l,hit_l_up)
    ) |> 
    select(-hit_w_up,-hit_t_up,-hit_l_up) |>  
    mutate(
      hit_ex = hit_w - hit_l,
      decision = ifelse( is.na( stand_ex) | is.na(hit_ex), decision, ifelse( stand_ex > hit_ex, "stand", "hit")),
      w = ifelse( is.na( decision ), w, ifelse( decision=="hit", hit_w, stand_w)),
      t = ifelse( is.na( decision ), t, ifelse( decision=="hit", hit_t, stand_t)),
      l = ifelse( is.na( decision ), l, ifelse( decision=="hit", hit_l, stand_l)),
      ex = w-l
    )
  
}

# sanity checks
hit_stand |> mutate( check = hit_w + hit_t + hit_l ) |> pull( check)
hit_stand |> mutate( check = w + t + l ) |> pull( check)

# visualize

hit_stand0 <- hit_stand |> filter( cur_tot <=21 )
ggplot( hit_stand0, aes( upcard, cur_tot, fill=decision)) + geom_tile() + facet_wrap( ~soft)
ggplot( hit_stand0, aes( upcard, cur_tot, fill=ex)) + geom_tile() + facet_wrap( ~soft) + scale_fill_distiller(palette = "PRGn")


##
## Calculate double edge
##

double <- expand_grid( upcard = cards$name, cur_tot = 2:21, soft = c(F,T), cards ) |> 
  mutate( double_tot = cur_tot + value, double_soft = soft | csoft ) |> 
  left_join( hit_stand, by=join_by( double_tot == cur_tot, double_soft == soft, upcard )) |> 
  group_by( upcard, cur_tot, soft )|> 
  summarize( 
    double_w = mean( stand_w ),
    double_t = mean( stand_t ),
    double_l = mean( stand_l ),
    double_ex = mean( stand_ex) * 2,
    .groups = "drop"
  ) 


ggplot( double, aes( upcard, cur_tot, fill=double_ex)) + geom_tile() + facet_wrap( ~soft) + scale_fill_distiller(palette = "PRGn")


hit_stand_double <- hit_stand |> 
  left_join( double, by = join_by( upcard, cur_tot, soft )) |> 
  mutate( 
    decision = ifelse( !is.na( double_ex ) & decision== "hit" & double_ex > hit_ex, "double", decision ),
    w = ifelse( decision == "double", double_w, w ),
    t = ifelse( decision == "double", double_t, t ), 
    l = ifelse( decision == "double", double_l, l ),
    ex = ifelse( decision == "double", double_ex, ex )  
  )
  

hit_stand_double0 <- hit_stand_double |> filter( cur_tot <=21 )
ggplot( hit_stand_double0, aes( upcard, cur_tot, fill=decision)) + geom_tile() + facet_wrap( ~soft) + scale_fill_brewer(palette = "Set2") 
ggplot( hit_stand_double0, aes( upcard, cur_tot, fill=ex)) + geom_tile() + facet_wrap( ~soft) + scale_fill_distiller(palette = "PRGn")


##
##  Calculate split edge
##

hit_stand_double_slim <- hit_stand_double |> select( upcard, cur_tot, soft, ex, decision)

split <- expand_grid( upcard = cards$name, cards ) |> 
  rename( pair = name, split_val = value, soft = csoft ) |> 
  filter( pair != "J", pair != "Q", pair != "K") |> 
  expand_grid( cards ) |> 
  filter( split_val != value ) |> 
  mutate( post_split_tot = split_val + value, post_split_soft = soft | csoft ) |> 
  left_join( hit_stand_double_slim, by = join_by( upcard, post_split_soft==soft, post_split_tot==cur_tot)) |> 
  group_by( upcard, soft, pair, split_val ) |> 
  summarize( split_ex = mean( ex )*2, .groups="drop" ) |> 
  mutate( split_ex = ifelse( split_val == 10, 13/(13-4) * split_ex, 13/(13-1) * split_ex )) # handle resplits

split <- split |> 
  mutate( pair_tot = split_val * 2 ) |> 
  left_join( hit_stand_double_slim, by = join_by( upcard, soft, pair_tot==cur_tot)) |> 
  mutate( 
    play_ex = ex,
    decision = ifelse( split_ex > play_ex, "split", decision ),
    ex = ifelse( decision == "split", split_ex, play_ex )
  )

ggplot( split, aes( upcard, pair, fill=decision)) + geom_tile() + scale_fill_brewer(palette = "Set2") 
ggplot( split, aes( upcard, pair, fill=ex)) + geom_tile()  + scale_fill_distiller(palette = "PRGn")



##
##  Overall game expectations
##


hand_freq <-cards |> rename( upcard = name, dealer1=value ) |> select( -csoft ) |> 
  expand_grid( cards, cnt=1 ) |> rename( hole=name, dealer2=value) |> select( -csoft ) |> 
  expand_grid( cards ) |> rename( name1=name, val1=value, soft1 = csoft ) |>
  expand_grid( cards ) |> rename( name2=name, val2=value, soft2 = csoft ) |> 
  filter( dealer1 != 1 | dealer2 != 10, dealer1 != 10 | dealer2 != 1) |> 
  filter( val1 != 1 | val2 != 10,       val1 != 10 | val2 != 1) |> 
  mutate( cur_tot = val1 + val2, soft = soft1 | soft2, pair = val1 == val2 ) |> 
  group_by( upcard, cur_tot, soft, pair ) |> 
  summarize( cnt = sum( cnt ), .groups="drop") |> 
  mutate( 
    p = cnt / sum(cnt), 
    cur_soft_tot = ifelse( soft & cur_tot <=11, cur_tot +10, cur_tot)
  )

 
 ggplot( hand_freq, aes( upcard, cur_soft_tot, fill=p)) + 
   geom_tile() + facet_wrap( pair~soft) + scale_fill_distiller(palette = "Blues", direction=1) + theme_minimal()
 
#
# initial blackjack probabilities 
#
 
p_bj = (1*4*2)/(13*13)
p_bj1 = p_bj * (1- p_bj) # solo BJ, only dealer or only player
p_no_bj = (1-p_bj)^2

bj_factor = 3/2
bj_ex = (bj_factor-1) * p_bj1 

split_slim <- split |> select( upcard, soft, cur_tot = pair_tot, ex) |> mutate( pair = T)

expectation_table <- hand_freq |>
  left_join( mutate( hit_stand_double_slim, pair = F), by = join_by( upcard, pair, soft, cur_tot)) |> 
  left_join( rename( split_slim, ex2  = ex), by =  join_by( upcard, pair, soft, cur_tot)) |> 
  mutate( ex = coalesce(ex, ex2)) |> 
  select( -ex2) 

ex_play <- expectation_table |> summarize( ex = sum( ex*p)) |> pull( ex)

ex <- p_no_bj * ex_play + bj_ex
ex
