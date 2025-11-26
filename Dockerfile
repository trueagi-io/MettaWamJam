# Dockerfile to create MettaWamJam docker server
#
# Includes:  PeTTa, MORK, PathMap

FROM swipl:latest

# Install system build tools, Python, and Apache
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
WORKDIR /PeTTa

# The Prolog server listens on 5000
EXPOSE 5000

# Start the Metta WAM server when the container runs
#CMD ["swipl", "mwj.pl"]

# Always run your server
ENTRYPOINT ["swipl", "mwj.pl", "atomspace.metta"]

# 👇 **No default argument**
CMD []
      