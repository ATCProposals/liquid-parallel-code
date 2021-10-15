FROM ubuntu:18.04

# Install dependencies
# ~~~~~~~~~~~~~~~~~~~~

ARG CABAL_VERSION=3.4
ARG GHC_VERSION=8.6.5
ARG GCC_VERSION=7

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common && \
    add-apt-repository -y ppa:ubuntu-toolchain-r/test && \
    add-apt-repository -y ppa:hvr/ghc && \
    add-apt-repository -y ppa:plt/racket && \
    add-apt-repository -y ppa:matthew.fluet/mlton && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    vim tmux git-core curl wget make xz-utils bzip2 coreutils binutils unzip \
    racket gcc-$GCC_VERSION g++-$GCC_VERSION cabal-install-$CABAL_VERSION ghc-9.0.1 ghc-$GHC_VERSION mlton \
    libgc-dev libgmp-dev uthash-dev libgmp-dev m4 python3.8 numactl gnuplot

ENV PATH=$PATH:/opt/ghc/bin

# Update the default GCC to be gcc-7.
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-$GCC_VERSION 60
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-$GCC_VERSION 60
RUN update-alternatives --install /usr/bin/ghc ghc /opt/ghc/bin/ghc-$GHC_VERSION 60

# Setup benchmarks
# ~~~~~~~~~~~~~~~~

ENV GIBBONDIR="/icfp21-ParallelGibbon-artifact/tree-velocity/" \
    PLTADDONDIR="/icfp21-ParallelGibbon-artifact/tree-velocity/gibbon/.racket_sandbox"

RUN wget https://zenodo.org/record/5035518/files/paper-158-artifact-source.tar.gz && \
    tar xvf paper-158-artifact-source.tar.gz

RUN wget https://github.com/ocaml/opam/releases/download/2.0.8/opam-2.0.8-x86_64-linux && \
    install opam-2.0.8-x86_64-linux /usr/local/bin/opam

RUN cd /icfp21-ParallelGibbon-artifact/ && \
    opam init --auto-setup --disable-sandboxing --bare && \
    . ~/.profile && \
    opam switch create 4.10.0+multicore+no-effect-syntax --packages=ocaml-variants.4.10.0+multicore+no-effect-syntax --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default

RUN opam install --yes dune.2.7.1 domainslib.0.2.2 sexplib.v0.14.0 containers.2.8.1 && \
    eval $(opam env)

RUN echo "\n\neval $(opam env)" >> ~/.bashrc && \
    eval $(opam env) && \
    cd /icfp21-ParallelGibbon-artifact/ocaml && dune build

RUN cd /icfp21-ParallelGibbon-artifact/ && \
    make setup_ghc setup_gibbon setup_mpl

RUN cd /icfp21-ParallelGibbon-artifact/ && \
    make gen_data

RUN cd /icfp21-ParallelGibbon-artifact/ && \
    make build_gibbon build_mpl build_ghc build_bench_script
