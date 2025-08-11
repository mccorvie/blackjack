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

upcard <- cards |> rename( soft=csoft, cur_tot= value) |> 
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
  group_by( name ) |> 
  summarise( bust_p = sum(p), cur_tot=last( cur_tot))


ggplot(bust_p, aes(name, bust_p)) + geom_point()

# overall bust probability

bust_p$bust_p |> mean()


# overall blackjack probability

upcard |> filter( final_soft_tot ==21)


upcard |> filter( cur_tot == 1 | cur_tot == 10, final_tot==11, final_soft) |> 
  pull( p ) |> 
  sum() /13
