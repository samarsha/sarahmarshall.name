---
layout: post
title: The many worlds of Schrödinger's cat
---

It started as a thought experiment to bring to light an apparent paradox of quantum mechanics, but the [many-worlds interpretation][mwi] sheds a different light on [Schrödinger's cat][cat].
In a real sense---if you're willing to accept the premise of many worlds---Schrödinger's cat *can* be both alive and dead at the same time.
The wave function never collapses: what we see as collapse is only an illusion, brought on by entanglement between the observer, the observee, and the rest of the environment, as the universe diverges to realize all possible measurement outcomes simultaneously.

It's a framework that has profound implications on the true nature of reality.
Although it may be difficult or impossible to find direct evidence for many worlds, the interpretation is worth considering because it does away with wave function collapse and the arbitrary separation between the "microscopic" and the "macroscopic" that exists in the [Copenhagen interpretation][copenhagen], while still remaining consistent with observations.

As cosmology flirts with the possibility of a spatially infinite universe, many worlds lets us consider an infinite *number* of universes.
In the multiverse, what does it mean that every quantum measurement that can happen, does happen?
What does it mean to be "you" when "you" are constantly diverging into separate entities who can never interact with each other, and each of whom is convinced that they are the only version of themselves that really exists?

But let's consider a more modest question.

## Enter *Superposition*

[*Superposition*][superposition] is a video game that I'm making with my friend [Rory Soiffer][rsoiffer].
The premise is: You are Schrödinger's cat, and many worlds are real.

It's a puzzle game, but we wanted to make something less abstract than a pure quantum puzzle game like [Hello Quantum][hello-quantum].
So we added an avatar for the player, used a top-down view, and made time continuous rather than discrete.
*Superposition* is superficially similar to traditional 2D action-adventure games.
But under the hood, a quantum system is being simulated.
Like the thought experiment, Schrödinger's cat gives us a bridge between the microscopic and the macroscopic, between the weird quantum world and the familiar classical world.

The question is: How do we design a video game that's fun to play while being mathematically rigorous and consistent with quantum mechanics?

Let's define our [Hilbert space][hilbert-space].

## State space

The Hilbert space, which is really just the space of possible states that our game can be in, is the product of the states that each entity in the game can be in.
As of the time of writing, we just have two entities that have quantum state: Schrödinger's cat, controlled by the player and who we will affectionately call *Erwin*; and *quballs*, which are ball-shaped qubits that can be picked up and moved around the level.

By "quantum state," I mean that the state is a linear combination of basis states in **C**^*n*, where the coefficient on each basis state is the *probability amplitude*, and the squared magnitudes of the probability amplitudes must sum to one.
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
If there is more than one quball in the level, the state space increases.

There are other kinds of objects like doors that can be either open or closed depending on the quball that is placed next to them, but the doors do not actually hold quantum state.
They simply reflect the state of the quball that is affecting them.
Quantum gates cannot be applied to the doors themselves, so they are not listed here.

You may notice that alive-dead, on-off, and carried-not-carried sound like qubits, but position is more complicated.
Position is a [qudit] that depends on the size of the level; it increases the size of the state space dramatically compared to just having a few qubits lying around.
And position is treated equally to the binary state of qubits.
Just as a quball can be in a superposition of on and off, it can be in a superposition of here and there.
There are more basis states for position, so there are more possibilites for superposition states as well.

## Gates

TODO

## Puzzles

TODO


[cat]: https://en.wikipedia.org/wiki/Schr%C3%B6dinger%27s_cat
[copenhagen]: https://en.wikipedia.org/wiki/Copenhagen_interpretation
[hello-quantum]: https://helloquantum.mybluemix.net/
[hilbert-space]: https://en.wikipedia.org/wiki/Hilbert_space
[mwi]: https://en.wikipedia.org/wiki/Many-worlds_interpretation
[qudit]: https://en.wikipedia.org/wiki/Qubit#Variations_of_the_qubit
[rsoiffer]: https://github.com/rsoiffer
[superposition]: https://github.com/samarsha/Superposition
