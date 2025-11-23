# Base image: Ubuntu 22.04
FROM ubuntu:22.04

# Avoid interactive prompts during install
ENV DEBIAN_FRONTEND=noninteractive

# Install prerequisites
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install ghcup (Haskell toolchain manager)
RUN curl -sSL https://get-ghcup.haskell.org | bash -s -- --no-upgrade

# Add ghcup binaries to PATH
ENV PATH="/root/.ghcup/bin:${PATH}"

# Install latest GHC, Cabal, and Stack
RUN ghcup install ghc recommended && \
    ghcup set ghc recommended && \
    ghcup install cabal recommended && \
    ghcup set cabal recommended && \
    ghcup install stack recommended && \
    ghcup set stack recommended && \
    ghcup install hls recommended && \
    ghcup set hls recommended

# Set working directory
WORKDIR /workspace
