FROM ubuntu:latest
COPY prepare.sh /prepare.sh 
RUN ./prepare.sh
ENV DOTNET_NOLOGO=true

COPY /src src
COPY /run.sh run.sh
COPY /data/input.txt input.txt
RUN mkdir /out
CMD ./run.sh < input.txt > /out/result.txt