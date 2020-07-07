FROM fpco/stack-build:lts-16.2

RUN mkdir /piClalc
WORKDIR /piCalc

COPY . .
RUN stack build
