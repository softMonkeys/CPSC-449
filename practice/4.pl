%
%  eats(Predator, Prey).
%
eats(bird,prawn).
eats(bird,mussels).
eats(bird,crab).
eats(bird,limpets).
eats(bird,whelk).
eats(crab,mussels).
eats(crab,limpets).
eats(fish,prawn).
eats(limpets,seaweed).
eats(lobster,crab).
eats(lobster,mussels).
eats(lobster,limpets).
eats(lobster,whelk).
eats(mussels,phytoplankton).
eats(mussels,zooplankton).
eats(prawn,zooplankton).
eats(whelk,limpets).
eats(whelk,mussels).
eats(zooplankton,phytoplankton).

isProducer(X) :-
  \+member(X, eats(X, _)).
