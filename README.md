# Neptune's Pride Galaxy Generator

Based on and faithful to the generation behavior of the Python script by "The Graken", this Purescript app will spit out JSONs that generate valid maps in Neptune's Pride.

It currently has no UI to mess with the orbits or preview the maps.

[Demo](https://darthnerda.github.io/np-star-gen/)


### FAQ
Q: Why Purescript?
A: For fun and bragging rights.

Q: Yeah, but like was there any reason it's better in Purescript?
A: Not really. It takes my full brain power to deal with strict purity and monads. I burned two free days doing this -> There was very little bang for this buck.

Q: No reason at all?
A: Perhaps terseness and a delicious code smell, plus a few guarantees like exhaustion of edge cases and making it very difficult to produce garbage data or runtime crashes. But since this project is only doing a tiny amount of actual work, most of those guarantees are of negligible value. I could have done it in 25% of the time with 75% of the same guarantees by just doing it in Typescript.