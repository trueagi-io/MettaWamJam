# Dockerfile to create MettaWamJam docker server
#
# Includes:  PeTTa, MORK, PathMap

#################################################################
#    To create arm64 if building on amd64:
# docker buildx create --use --name mwjbuilder
# docker buildx build --platform linux/arm64 -t mwj:arm64 --load .
#################################################################

FROM swipl:latest

# Install system build tools, Python, etc
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      git \
      nano \
      build-essential \
      procps \
      curl \
      python3 \
      python3-pip \
      python3-dev \
      ca-certificates \
      pkg-config \
      cmake \
 && rm -rf /var/lib/apt/lists/*

# 👇 RUST INSTALL
# -----------------------------------------
#RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
#ENV PATH="/root/.cargo/bin:${PATH}"
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly
ENV PATH="/root/.cargo/bin:${PATH}"
# -----------------------------------------

# 👇 PATHMAP INSTALL
RUN git clone https://github.com/Adam-Vandervorst/PathMap.git /PathMap
WORKDIR /PathMap
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

# 👇 MORK INSTALL
RUN git clone https://github.com/trueagi-io/MORK.git /MORK
WORKDIR /MORK/kernel
RUN RUSTFLAGS="-C target-cpu=native" cargo build --release

# 👇 PETTA INSTALL
# Install janus-swi system-wide
RUN pip3 install --no-cache-dir --break-system-packages janus-swi 

# Clone PeTTa repository directly into /PeTTa
RUN git clone https://github.com/patham9/PeTTa.git /PeTTa
COPY mwj.pl /PeTTa

Build mork_ffi for PeTTa to access MORK
WORKDIR /PeTTa
RUN sh build.sh

# Install facebook vector embeddings tool for use by PeTTa
#RUN apt-get update \
# && apt-get install -y --no-install-recommends \
#      libopenblas-dev \
#      libblas-dev \
#      liblapack-dev \
#      gfortran \
# && rm -rf /var/lib/apt/lists/*
#WORKDIR /PeTTa/faiss_ffi
#RUN sh install_faiss.sh

WORKDIR /PeTTa

# The Prolog server listens on 5000
EXPOSE 5000

# Start server

#ENTRYPOINT ["swipl", "mwj.pl", "atomspace.metta"]  # Works with no MORK startup

# LD_PRELOAD needed for MORK
ENV LD_PRELOAD=/PeTTa/mork_ffi/target/release/libmork_ffi.so

# Start swipl with mwj.pl. If user connects an atomspace to /PeTTa/atomspace.metta mwj.pl loads.
ENTRYPOINT ["swipl","--stack_limit=8g","-q","-s", "mwj.pl","--","atomspace.metta","mork"]
      
