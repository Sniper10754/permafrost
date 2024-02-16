FROM ghcr.io/rust-lang/rust:nightly-bullseye-slim

RUN apt update
RUN apt upgrade
RUN apt install pax-utils -y

RUN mkdir /sdk
RUN mkdir /sdk/build
RUN mkdir /sdk/bin

WORKDIR /sdk/build

ADD . /sdk/build/

RUN --mount=type=cache,target=target \
    --mount=type=cache,target=~/.cargo \
    cargo build --release

RUN for file in $(scanelf -B -F %s ./target/release); do cp $file /sdk/bin; done

WORKDIR /sdk

RUN rm -rf /sdk/build
