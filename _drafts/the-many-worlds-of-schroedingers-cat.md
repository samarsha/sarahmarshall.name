---
layout: post
title: The many worlds of Schrödinger's cat
---

It started as a thought experiment to bring to light an apparent paradox of quantum mechanics, but the [many-worlds interpretation][mwi] sheds a different light on [Schrödinger's cat][cat].
In a real sense---if you're willing to accept the premise of many worlds---Schrödinger's cat *can* be both alive and dead at the same time.
The wave function never collapses: what we see as collapse is only an illusion, brought on by entanglement between the observer, the observee, and the rest of the environment, as the universe diverges to realize all possible measurement outcomes simultaneously.

It's a framework that has profound implications for the true nature of reality.
Although it may be difficult or impossible to find direct evidence for many worlds, the interpretation is worth considering because it does away with wave function collapse and the arbitrary separation between the "microscopic" and the "macroscopic" that exists in the [Copenhagen interpretation][copenhagen], while still remaining consistent with observations.

As cosmology flirts with the possibility of a spatially infinite universe, many worlds lets us consider an infinite *number* of universes.
In the multiverse, what does it mean that every quantum measurement that can happen, does happen?
What does it mean to be "you" when "you" are constantly diverging into separate entities who can never interact with each other, and each of whom is convinced that they are the only version of themselves that really exists?

But let's consider a more modest question.

## Enter *Superposition*

[*Superposition*][superposition] is a video game that I'm making with my friend [Rory Soiffer][rsoiffer].
The premise is: You are Schrödinger's cat, and you really can be in a superposition of alive and dead.

It's a puzzle game, but we wanted to make it approachable and more like a 2D action-adventure game than a pure abstract puzzle game would be.
The player directly controls Schrödinger's cat, and in real time instead of with discrete turns.
But under the hood, a quantum system is being simulated.
Like the thought experiment, Schrödinger's cat gives us a bridge between the microscopic and the macroscopic---between the weird quantum world and the familiar classical world.

An important point of clarification: I'm using the many-worlds interpretation as a metaphor for the game's mechanics, since Rory and I decided to call our representation of superposition states in the game "universes" that are part of a "multiverse."
But they are not the same thing as MWI.
Universes in the game can be "recombined" even if different events happened in their history, unlike MWI.
Also, *Superposition* doesn't allow measurement, and the rest of the game doesn't assume any particular interpretation of quantum mechanics, either.

The good news is that *Superposition* does actually model a mathematically accurate quantum system.
Actually, the "universes" are just a fancy name for basis states and their corresponding probability amplitudes, and the "multiverse" is just the complete state of the system.
When universes recombine, it is due to the interference of basis states, just like real quantum mechanics.

So with that said, the question is: How do we design a video game that's fun to play while being mathematically rigorous and consistent with quantum mechanics?

Let's define our [Hilbert space][hilbert-space].

## State space

The Hilbert space, which is really just the space of possible states that our game can be in, is the product of the states that each entity in the game can be in.
As of the time of writing, we just have two entities that have quantum state: Schrödinger's cat, controlled by the player and who we will affectionately call *Erwin*; and *quballs*, which are ball-shaped qubits that can be picked up and moved around the level.

By "quantum state," I mean that the state is a linear combination of basis states in **C**^*n*, where the coefficient on each basis state is the probability amplitude, and the squared magnitudes of the probability amplitudes must sum to one.
Each level contains a discrete grid, and each entity can be in one or more of these cells.
That is, each cell is a basis state, and an entity is in a linear combination of them.
This is in contrast with other state in the game that is not quantum, which we call *metadata*.
For example, the precise pixel position of each entity is part of the *game* state, but not the *quantum* state.
We use the precise position to smoothly animate objects across the grid, but it does not affect the behavior of the quantum system.
A quantum gate is applied equally to every object in a cell, regardless of if it is in the center or one of the corners.

So here is a complete list of the quantum state for our two types of entities:

| Erwin            | Quball                 |
|------------------|------------------------|
| Current position | Current position       |
| Alive or dead    | On or off              |
| -                | Carried or not carried |

And that's it, at least in terms of basic entity types.
Naturally, if there is more than one quball in the level, the state space expands.

There are other kinds of objects like doors that can be either open or closed depending on the quball that is placed next to them, but the doors do not actually hold quantum state.
They simply reflect the state of the quball that is affecting them.
Quantum gates cannot be applied to the doors themselves, so they are not listed here.

You may notice that alive-dead, on-off, and carried-not-carried sound like qubits, but position is more complicated.
Position is a [qudit] that depends on the size of the level; it increases the size of the state space dramatically compared to just having a few qubits lying around.
And position is treated equally to the binary state of qubits.
Just as a quball can be in a superposition of on and off, it can be in a superposition of here and there.
There are more basis states for position, so there are more possibilites for superposition states as well.

## Gates

We need quantum gates to evolve the state of the system over time.
We define a *gate* as two functions:

{% highlight scala %}
trait Gate[A] {
  def apply(value: A)(universe: Universe): NonEmptyList[Universe]
  def adjoint: Gate[A]
}
{% endhighlight %}

`adjoint` is familiar if you know some quantum computing.
It is the gate that does the reverse of the original gate when given the same argument.

`apply` takes an arbitrary argument to give to the gate, and a universe in which to apply the gate.
In return, the gate gives you a one or more universes back.

Remember, when you see "universe," just think of a term in an equation representing a quantum state: a probability amplitude multipled by a basis state.
In traditional quantum mechanics, we can describe the state of a two-qubit system as *|psi> = a|00> + b|01> + c|10> + d|11>*.
Here, we say that we have four universes:

1. *(d, {q1 -> 0, q0 -> 0})*
2. *(a, {q1 -> 0, q0 -> 1})*
3. *(b, {q1 -> 1, q0 -> 0})*
4. *(c, {q1 -> 1, q0 -> 1})*

Then `apply` just maps a term to one or more terms.
For example:

* *X* maps a\|0> to a\|1> and a\|1> to a\|0>.
* *H* maps a\|0> (a/sqrt(2) \|0>, a/sqrt(2) \|1>), and a\|1> to (a/sqrt(2) \|0>, -a/sqrt(2) \|1>).

We go to the trouble of calling terms *universes* because we want to provide the illusion that each term represents an entire world, with its own processes and animations in the game, where all of the qudits have a particular classical state.
To do this we need to associate more information with each term than just its probability amplitude and basis state.

The meaning of the first parameter `value` depends on the specific gate used.
Usually, it contains the ID of the qudit whose state should be changed and any additional parameters that the gate needs, such as the degree of a rotation.

Gates are the only way to change the state of the quantum system in *Superposition*.
How does the player pick up a quball?
By applying the *X* gate to qubit representing the quball's carried state.
How does the player move?
By applying the *Translate* gate to the qudit representing their position.
All gates must be unitary, which ensures that any changes to the game state are sound.

### Transforming gates

Gates are functions, so they can be transformed like functions.
An example is `contramap` which transforms the type of the input value to a gate.
Its signature is:

{% highlight scala %}
def contramap[A, B](f: B => A)(gate: Gate[A]): Gate[B]
{% endhighlight %}

This is called `contramap` instead of `map` because the order of the types is reversed.
The new `Gate[B]` applies the mapping function on its input of type `B` to transform it into type `A`, and only then can it call `Gate[A]`.
If you're into category theory, gates are actually [contravariant functors][contravariant].

Another example is `multi`, which takes a gate that operates on a single value of type `A` and creates a gate that operates a sequence of those values and accumulates the universes:

{% highlight scala %}
def multi(gate: Gate[A]): Gate[Seq[A]] = new Gate[Seq[A]] {
  override def apply(values: Seq[A])(universe: Universe) = values match {
    case Seq() => NonEmptyList(universe)
    case x :: xs => gate(x)(universe) flatMap gate.multi(xs)
  }

  override def adjoint = gate.adjoint.multi contramap (_.reverse)
}
{% endhighlight %}

Basically: if the sequence is empty, return the original universe unchanged.
Otherwise, apply the first value to the gate in the initial universe, and then repeat for the remaining values using the universes produced by the first application.
This is analogous to the [`ApplyToEach`][applytoeach] operation in Q#.

**TODO**

## Puzzles

**TODO**


[applytoeach]: https://docs.microsoft.com/en-us/qsharp/api/qsharp/microsoft.quantum.canon.applytoeach
[cat]: https://en.wikipedia.org/wiki/Schr%C3%B6dinger%27s_cat
[contravariant]: https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant.html
[copenhagen]: https://en.wikipedia.org/wiki/Copenhagen_interpretation
[hilbert-space]: https://en.wikipedia.org/wiki/Hilbert_space
[mwi]: https://en.wikipedia.org/wiki/Many-worlds_interpretation
[qudit]: https://en.wikipedia.org/wiki/Qubit#Variations_of_the_qubit
[rsoiffer]: https://github.com/rsoiffer
[superposition]: https://github.com/samarsha/Superposition
