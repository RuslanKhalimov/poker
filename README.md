# Texas Hold’em

Client and server for Texas Hold’em

### Requirements

[stack](https://docs.haskellstack.org/en/stable/README/) installed

### Installation

```sh
git clone https://github.com/RuslanKhalimov/poker.git
cd poker
stack build
```

### Server usage

```sh
stack exec server <port>
```

### Client usage

```sh
stack exec client <name> <ip> <port> <players count>
```
* `<name>`            - nickname you want to play with
* `<ip> <port>`       - server info 
* `<players> <count>` - the number of players you want to play with

##### Control

* Use the keyboard to enter a bet (`delete` key for removing)
* Use buttons in the lower right corner of the screen or keys `b`, `c` or `f` to make bet, check or fold cards respectively
