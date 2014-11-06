# wes
[![Build Status](https://travis-ci.org/wooga/wes.png?branch=master)](https://travis-ci.org/wooga/wes)

## Introduction
Wes is a library that helps you to build actor based services in erlang.

### Actors, Channels, Commands and Reads
An _Actor_ is a isolated state and logic that acts upon this state given input.
An Actor subscribes to one channel at a time. The user interacts with it's actors via Commands and Reads.

A _Channel_ is implemented as a erlang process that among other 
things holdes the states of all actors that subscribes to it.

A _Command_ is sent to a channel and is then broadcasted to all actors subscribing to that channel. 
Each actor can react to the command by changing it's state.
A Command call returns <code>ok</code> iff all actors handles the message.

A _Read_ is sent directly to one actor. The actor returns a view of its state.

In other words, a channel is a set of actors that acts atomically to commands.
The restrictions of commands and reads makes is to easier to keep your code decouple.
All actors for a channel is persisted periodical and when a channel stops (without signaling an error).

## Getting started

### Example project
[wes bank](https://github.com/anha0825/wes_bank) -
Implements a simple bank account model on top of wes.

## Building blocks
For each building block (except channels) wes exposes a callback interface
to implement.

### Name registry
A name registry implementation keep trac of in which process each channel is
running and to which channel an actor is listening.

### Channels
A channel receives commands and reads.
Commands are broadcasted to all actors that are listening to the channel.
Reads are sent to the actor that it is aimed for.
A channel manages the timeouts for itself and for the actors connected to it.

#### Error Handling

Any exception that occurs inside a channel will shut the channel down without
saving the actors. When resuming, the channel will have the latest persistent
state of the actors, allowing you to resume from a known state.

If the exception occurred in a command to the channel, the same exception will
be raised from the call to `wes:command/2` or `wes:command/3`.

The stats module will also get a `stop` event with an
`{exception, Class, Reason, Stacktrace}` argument so that individual error
statistics can be tracked.

### Actors
This is where the game logic lives.
An actors is offline (persisted) or listens to one channel.
An actor gets all commands sent to the channel where it lives and
the read events that is sent directly to it.
An actor can tell it's channel to stop and can register periodic
timeout calls from the channel.

### Actor persistance
A persistance module implements read and write opertaions for serialized actors.

### Statistics
A stats implementation gets statistics about what happens in each channel.
