## Description

Current solution implements poker hands strength evaluation. Three poker game types are supported: 

* Texas Hold'Em [see rules](https://en.wikipedia.org/wiki/Texas_hold_%27em)
* Omaha Hold'Em [see rules](https://en.wikipedia.org/wiki/Omaha_hold_%27em)
* Five Card Draw [see rules](https://en.wikipedia.org/wiki/Five-card_draw)

Input is expected in the form of 

    <game-type> [<5 board cards>] <hand 1> <hand 2> <...> <hand N>
    texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
    omaha-holdem 6hQsQd9hAh 5h7cAs9s 2h9c6s2d QcTs8c2s
    five-card-draw Ah9h5d9d6c Qs3d5s8c7c Th5cQdAsTs JcQc7sAdJh 6s9s8hKc8s

Where each card is represented as two chars (rank and suit respectively).

Simple brute force algorithm is used, application hasn't been optimized in terms of time or memory consumption and isn't covered by tests. Processing of 300k games takes of about 6 minutes. 
Type system currently used might be simplified and/or refactored, but is left as is to simplify possible algorithm issues investigation: a bunch of unnecessary for the end result data is passed from place to place.

## Environment

Solution is implemented in the FSharp language, hence dotnet SDK is required. 
You could either build your image based on `ubuntu:latest` image, by running `prepare.sh` script to setup the dependencies then 
or base your image on `mcr.microsoft.com/dotnet/sdk:latest`, where required SDK is installed already.

Alternatively you could use provided `.Dockerfile` to build an image and execute application inside it.
For your convenience bind `data` and `out` directories as input is read from `/data/input.txt` and written to `/out/result.txt`.
Make sure that you're running command from the repository root as paths are relative.

    docker build -t bootcamp . 
    docker run --name bootcamp \ 
        --mount type=bind,source="$(pwd)/out",target=/out \ 
        --mount type=bind,source="$(pwd)/data",target=/data \ 
        bootcamp

## Execution

Run `run.sh` to run the application. Data is read from the `stdin` line by line with output sent to `stdout`.
Passing piped files as input and output is the expected way to use this application: 

    ./run.sh < ./data/input.txt > ./out/result.txt

Alternatively you could use interactive mode of execution. The same file from `/data/input.txt` will be read with result going to `/out/result.txt`

    dotnet fsi ./scripts/interactive.fsx 

You might use `/scripts/generate.fsx` to generate test input file. Update parameters specified in the top of the script and run it.

    dotnet fsi /scripts/generate.fsx
