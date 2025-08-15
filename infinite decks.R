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

upcard <- cards |> rename( soft=csoft, cur_tot= value, upcard=name) |> 
  left_join( dealer, relationship = "many-to-many" ) 

  
##
## Plot the transition matrix
##
ggplot( dealer, aes(cur_tot, final_tot,fill=p)) +
  geom_tile()+
  scale_fill_gradient(low="white", high="blue") +
  facet_wrap( soft ~final_soft)

## 
##  Calculate the bust probability given the card that's showing
##


bust_p <- upcard |> 
  filter( final_tot > 21 ) |> 
  group_by( upcard ) |> 
  summarise( bust_p = sum(p), cur_tot=last( cur_tot))


ggplot(bust_p, aes(upcard, bust_p)) + geom_point()

# overall bust probability

bust_p$bust_p |> mean()


# overall blackjack probability

upcard |> filter( final_soft_tot ==21)


upcard |> filter( cur_tot == 1 | cur_tot == 10, final_tot==11, final_soft) |> 
  pull( p ) |> 
  sum() /13


outcome_fac <- c( "17","18","19","20","21","bust")

dealer_outcomes <- upcard |> 
  filter( p > 0 ) |> 
  mutate( outcome_val = ifelse( final_soft_tot > 21, 0, final_soft_tot )) |> 
  group_by( upcard, outcome_val ) |> 
  summarize( p = sum(p), .groups="drop") |> 
  mutate( outcome = ifelse( outcome_val > 0, as.character(outcome_val), "bust" ))

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
#

dealer_outcomes_wide <- dealer_outcomes |> 
  select( -outcome ) |>
  pivot_wider( names_from = outcome_val, values_from = p )

maxt = 31


13^5

# basic setup and bust edge
hit_stand <- expand_grid( upcard = cards$name, cur_tot = 1:maxt, soft = c(F,T),
                     stand_w = NA, stand_t =NA, stand_l = NA, stand_ex = NA, 
                     hit_w = NA, hit_t= NA, hit_l = NA, hit_ex = NA,
                     w=NA, t = NA, l=NA, ex = NA,
                     decision=NA ) |> 
  left_join( dealer_outcomes_wide, by=join_by( upcard)) |> 
  mutate( 
    cur_soft_tot = ifelse( soft & cur_tot <= 11, cur_tot+10, cur_tot),
    decision = ifelse( total > 21, "bust", decision ),
    w = ifelse( cur_tot > 21, 0, w ),
    t = ifelse( cur_tot > 21, 0, t ),
    l = ifelse( cur_tot > 21, 1, l ),
    ex = ifelse( cur_tot > 21, w-l, ex )
  ) 

# stand edge
hit_stand <-hit_stand |> 
  rowwise() |> 
  mutate( 
    vec     = list( c( `0`, rep(0,16), `17`,`18`,`19`,`20`,`21`, rep(0,maxt-22+2))),
    stand_w = sum( vec[1:cur_soft_tot]), 
    stand_t = vec[cur_soft_tot+1],
    stand_l = sum( vec[(cur_soft_tot+2):(maxt+2)]),
    stand_ex = stand_w-stand_l
  ) |> 
  select( -vec )

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

hit_stand0 <- hit_stand |> filter( cur_tot <=21 )

ggplot( hit_stand0, aes( upcard, cur_tot, fill=decision)) + geom_tile() + facet_wrap( ~soft)

ggplot( hit_stand0, aes( upcard, cur_tot, fill=ex)) + geom_tile() + facet_wrap( ~soft) + scale_fill_distiller(palette = "PRGn")
