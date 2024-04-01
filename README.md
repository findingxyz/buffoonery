# buffoonery
something to "help" you play Balatro

In Balatro sometimes you want to know the likelihood of drawing a flush, or a two pair, or drawing the cards to play them.
This program gives the answer. It simulates draws, otherwise it will take too long to calculate exact probabilities.

### right now output
Create a standard 52 card deck and find the expected number of draws required to play a high card.
```
> create
> drawInto HighCard
0.99999875
> hand
0 card(s) in hand: 
[]
```

### possible output
```
> create deck
standard 52 card deck shown here
> draw 5
5 random cards here
> size deck
47
> draw Ace
a random ace is drawn and shown
> draw Ace Spades
a random ace of spades is drawn and shown
> draw 5?
show all draws (doesn't draw)
> draw Ace?
show all ace draws (doesn't draw)
> draw Ace from 5
draws 5 cards, at least one of which is an Ace
> draw Ace from 5?
shows all possible draws of 5 that contains at least one Ace
```
