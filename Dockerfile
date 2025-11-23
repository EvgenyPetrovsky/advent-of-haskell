FROM mcr.microsoft.com/devcontainers/base:ubuntu

RUN sudo apt update
RUN sudo apt install -y libgmp-dev
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
