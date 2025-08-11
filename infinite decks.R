

require( "tidyverse")

#aa <- expand_grid( total = 1:26, soft = c(F,T), bustp = NA ) |> 

cards <- tibble( 
  value = c( 1,2,3,4,5,6,7,8,9,10,10,10,10),
  name  = c("A","2","3","4","5","6","7","8","9","10","J","Q","K"),
  soft  = c( T, rep(F,12))
) 
#soft = c(T,F),

dealer <- expand_grid( cur_tot = 1:21, final_tot = 17:26, p= NA)  |> 
  mutate( p = ifelse( final_tot <= cur_tot, 0, p )) |> 
  mutate( p = ifelse( cur_tot >= 17  & final_tot == cur_tot, 1, p )) |> #stand
  mutate( p = ifelse( cur_tot >= 17  & final_tot > cur_tot,  0, p )) 

#> mutate( p = ifelse( cur_tot == 17 & final_tot == 17 & !soft, 1,p))

for( cur_tot_update in 16:1)
{

  
  # create the transition step
  one_card <- expand_grid( filter( dealer, cur_tot == !!cur_tot_update, is.na(p) ), cards) |> 
    select( -p ) |> 
    mutate( next_tot = cur_tot+value) |> 
    left_join( dealer, by=join_by( next_tot == cur_tot, final_tot )) |> 
    mutate( p = ifelse( next_tot > final_tot,0,p)) |> 
    mutate( p = ifelse( next_tot > 21, ifelse( final_tot==next_tot,1,0),p)) 
  
  dealer_update <- one_card |> group_by( cur_tot, final_tot) |> 
    summarize( newp = mean( p), .groups =  "drop") 
  
  dealer <- left_join( dealer, dealer_update, by=join_by(cur_tot, final_tot)) |> 
    mutate( p =coalesce(p,newp)) |> 
    select(-newp) 
  
  
  
}

ggplot( dealer, aes(cur_tot, final_tot,fill=log(p+1e-6))) +
  geom_tile()+
  scale_fill_gradient(low="white", high="blue")

bust_p <-dealer |> filter( final_tot > 21, cur_tot<=10) |>
  group_by( cur_tot) |> 
  summarize( bust_p = sum(p)) |> 
  print( n=30)

ggplot(bust_p, aes(cur_tot, bust_p)) + geom_point()



### adding soft hands


cards <- tibble( 
  value = c( 1,2,3,4,5,6,7,8,9,10,10,10,10),
  name  = c("A","2","3","4","5","6","7","8","9","10","J","Q","K"),
  csoft  = c( T, rep(F,12))
)
#soft = c(T,F),

dealer <- expand_grid( hard_tot = 1:21, final_tot = 8:26, soft = c(F,T), final_soft = c(F,T), p= NA)  |> 
  mutate( final_p = ifelse( final_tot == hard_tot & final_soft == soft, 1, 0 ) ) |> 
  mutate( p = ifelse( final_tot < hard_tot, 0, p )) |> # impossible or hit
  mutate( p = ifelse( hard_tot >= 17, final_p, p )) |> #stand
  mutate( p = ifelse( final_soft & hard_tot >= 8 & hard_tot <= 11, final_p, p )) |> 
  select( -final_p )
  


dealer0 <- dealer
#> mutate( p = ifelse( hard_tot == 17 & final_tot == 17 & !soft, 1,p))

for( hard_tot_update in 16:1)
{
  

## one card should be starting state x cards 

  # create the transition step
one_card <- expand_grid( filter( dealer, hard_tot == !!hard_tot_update, is.na(p) ), cards) |> 
  select( -p ) |> 
  mutate( next_tot = hard_tot+value, next_soft = soft | csoft ) |> 
  mutate( final_p = ifelse( final_tot == next_tot & final_soft == next_soft, 1, 0 ) ) |> 
  left_join( dealer, by=join_by( next_tot == hard_tot, next_soft == soft, final_tot, final_soft )) |> 
  mutate( p = ifelse( next_tot > final_tot, 0, p)) |> 
  mutate( p = ifelse( next_tot > 21, final_p, p)) 



  
  
  #filter( one_card, final_tot==11 ) |> print( n=100)
  #filter( dealer0, final_tot==16) |> print( n=100)
  # filter( dealer, final_tot==11) |> print( n=100)
  
  dealer_update <- one_card |> group_by( hard_tot, final_tot, soft, final_soft ) |> 
    summarize( newp = mean( p), .groups =  "drop") 
  
  dealer_update |> print( n=100)
  
  dealer_new <- left_join( dealer, dealer_update, by=join_by(hard_tot, final_tot, soft, final_soft)) |> 
      mutate( p =coalesce(p,newp)) |> 
      select(-newp) 

  dealer_new |> filter( hard_tot ==12) |> print(n=100)
  dealer <- dealer_new    
    
    

#  dealer <- dealer0
  
}



ggplot( dealer, aes(hard_tot, final_tot,fill=log(p+1e-6))) +
  geom_tile()+
  scale_fill_gradient(low="white", high="blue") +
  facet_wrap( soft ~final_soft)


filter( dealer, hard_tot == !!hard_tot_update) |> print( n=100)
filter( dealer, hard_tot == 16) |> print( n=100)
print( filter( dealer, !is.na(p), hard_tot==17)  , n=100)
filter( dealer, hard_tot == 16, is.na(p) )
