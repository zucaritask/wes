# wes

## Introduction

wes aims to the be the hub for the building blocks need to build an
actor based services in erlang.
wes helps you to decouple the building blocks of the system.
The caller is decoupled from the application logic.
The application logic is decoupled from the process registry,

## API
A caller interacts with with system through commands and reads.

### Command
Changes the state of all actors connected to a channel.

### Read
Retrives a view of the state of an actor.

This is a work in progress. See todo.org for more details.

## Building blocks

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
