# FileSwarm - A BitTorrent client written in Haskell.

I was inspired by [Jakub Okoński](https://github.com/farnoy)'s [post on Reddit](https://www.reddit.com/r/haskell/comments/3nr24c/writing_a_bittorrent_client_in_haskell_1/) that I thought I'd try the same exercise myself in order to get some experience working on a non-trivial Haskell development project.

I developed the code myself but have borrowed from Jakub's choice of third party libraries.

This code is in a very early developmental state and is quite unusable

**The current architecture has started becoming a bit cumbersome. I need to lear more about monad transformers and come back to this. For now, I've stopped working on this and have moved development [here](https://github.com/lisphacker/fileswarm2)**


## Log

### 2018-02-27
- Working handshake

### 2017-12-27
- FileIO messaging and thread beginning to work
- Moved FileIO specific types from Types to FileIO

### 2017-12-26
- Refactoring code to make dedicated FileIO thread controlled over messages (Not working yet)

### 2017-12-21
- Added dummy piece client.
- Need to fix locks. STM is not useful for locks connected to IO
- Refactored types for more fine-grained STM access over pieces

### 2017-12-17
- Added piece writes
- Added locking around file reads/writes
- File checksum verification at startup to identify incomplete pieces

### 2017-12-13
- Moved types to a single file for ease of use
- Refactored file initializing code as per type changes
- Added piece reads from files

### 2017-12-11
- Output file(s) initialization
- Beginnings of a piece to file translation layer

### 2017-12-04
- Started log
- Discovered lens! Why did I not learnn this before?!
