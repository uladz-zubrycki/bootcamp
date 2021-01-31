FROM ubuntu:latest
ENV DOTNET_NOLOGO=true

COPY /prepare.sh prepare.sh 
COPY /src src
COPY /run.sh run.sh
RUN mkdir /out
RUN ./prepare.sh
CMD ./run.sh < /data/input.txt > /out/result.txt