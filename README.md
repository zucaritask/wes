# wes
[![Build Status](https://travis-ci.org/wooga/wes.png?branch=master)](https://travis-ci.org/wooga/wes)

## Introduction
Wes is a library that helps you to build actor based services in erlang.
The focus of Wes is to keep your code decouple and reuseable.

## Getting started

### Example project
[wes bank](https://github.com/anha0825/wes_bank) -
Implements a simple bank account model on top of wes.

## API
A caller interacts with with system through commands and reads.

### Command
Changes the state of all actors connected to a channel.

### Read
Retrives a view of the state of an actor.

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

### Actors
This is where the game logic lives.
An actors is offline (persisted) or listens to one channel.
An actor gets all commands sent to the channel where it lives and
the read events that is sent directly to it.
An actor can tell it's channel to stop and can register periodic
timeout calls from the channel.

### Storage
A storage implementation reads and writes serialized actors.

### Statistics
A stats implementation gets statistics about what happens in each channel.
