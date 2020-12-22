## Description

Current solution implements poker hands strength evaluation. Three poker game types are supported: 

* Texas Hold'Em [see rules](https://en.wikipedia.org/wiki/Texas_hold_%27em)
* Omaha Hold'Em [see rules](https://en.wikipedia.org/wiki/Omaha_hold_%27em)
* Five Card Draw [see rules](https://en.wikipedia.org/wiki/Five-card_draw)

Input is expected in the form of 

    <game-type> [<5 board cards>] <hand 1> <hand 2> <...> <hand N>
    texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d

Where each card is represented as two chars (rank and suit respectively).

## Environment

Solution is implemented in the FSharp language, hence dotnet SDK is required. 
You could either build your image based on `ubuntu:latest` image, by running `prepare.sh` script to setup the dependencies then 
or base your image on `mcr.microsoft.com/dotnet/sdk:latest`, where required SDK is installed already.

## Execution

Run `run.sh` to run the application. Data is read from the `stdin` line by line with output sent to `stdout`.
Passing piped files as input and output is the expected way to use this application: 

```
./run.sh < ./data/input.txt > ./out/output.txt