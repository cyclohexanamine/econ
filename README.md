A simplistic grid-based economy system. 

The algorithm is explained in `algorithm.pdf`; the Haskell implementation is in `src`.

In brief, each grid square is modelled as a unit that moves its production towards that which produces the most wealth.
Prices are allocated to goods based on their local marginal utility (based on their supply), and the trade prices of their neighbours.
Production of goods is determined as a function of the population allocated to a resource in a particular square.
The whole system is updated iteratively, with the intention that complex behaviours arise from these simpler interactions.
