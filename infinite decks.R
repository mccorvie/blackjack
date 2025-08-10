

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

ggplot( dealer, aes(cur_tot, final_tot,fill=p)) +
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
  soft  = c( T, rep(F,12))
)
#soft = c(T,F),

dealer <- expand_grid( cur_tot = 1:21, final_tot = 17:26, soft = c(F,T), p= NA)  |> 
  mutate( p = ifelse( final_tot <= cur_tot, 0, p )) |> # impossible or hit
  mutate( p = ifelse( cur_tot >= 17  & final_tot == cur_tot, 1, p )) |> #stand
  mutate( p = ifelse( cur_tot >= 17  & final_tot > cur_tot,  0, p ))    

#> mutate( p = ifelse( cur_tot == 17 & final_tot == 17 & !soft, 1,p))

#for( cur_tot_update in 16:1)
#{

cur_tot_update <- 16

  # create the transition step
  one_card <- expand_grid( filter( dealer, cur_tot == !!cur_tot_update, is.na(p) ), cards) |> 
    select( -p ) |> 
    mutate( next_tot = cur_tot+value) |> 
    left_join( dealer, by=join_by( next_tot == cur_tot, final_tot, soft )) |> 
    mutate( p = ifelse( next_tot > final_tot, 0, p)) 
  
  
  |> 
    mutate( p = ifelse( next_tot > 21, ifelse( final_tot==next_tot,1,0),p)) 
  
  dealer_update <- one_card |> group_by( cur_tot, final_tot) |> 
    summarize( newp = mean( p), .groups =  "drop") 
  
  dealer <- left_join( dealer, dealer_update, by=join_by(cur_tot, final_tot)) |> 
    mutate( p =coalesce(p,newp)) |> 
    select(-newp) 
}


filter( dealer, cur_tot == !!cur_tot_update) |> print( n=100)
filter( dealer, cur_tot == 16) |> print( n=100)
print( filter( dealer, !is.na(p), cur_tot==17)  , n=100)
filter( dealer, cur_tot == 16, is.na(p) )
