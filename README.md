# molis-hai

Password Generation using Markov models, Huffman trees, and Charles Dickens

This repo contains the Racket code that I used to generate markov models and
huffman trees for [Molis Hai](http://www.brinckerhoff.org/molis-hai/pwgen.html),
a random password generator that generates relatively memorable strings.


It's a Racket Package, which means you can install it using

```
raco pkg install molis-hai
```

You can run it using

```
raco molis-hai
```

This will give you a single password using an order-2 model and 56 bits of entropy.

You could instead run

```
raco molis-hai -o 3 -b 80 -n 4
```

to generate 4 passwords using a third-order model with 80 bits of entropy each.

When I'm generating secure root passwords, I generate 8 passwords using 59 bits
of entropy, and then pick the one I like the best. This guarantees at least 56
bits of entropy in the resulting password.

Give it a try!
