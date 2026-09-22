FROM ubuntu:22.04

ARG LLVM_REVISION=eccf4d44d346eee498b0ff709e625e3104448751
ARG STP_REVISION=1bdfe5076c3f78e2bb7fccc5efb1a2a36c4068e6
ARG MINISAT_REVISION=14c78206cd12d1d36b7e042fa758747c135670a4

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
      bison ca-certificates cmake flex g++ git ninja-build perl python3 \
      z3=4.8.12-1 \
      zlib1g-dev libxml2-dev libzstd-dev libtinfo-dev && \
    rm -rf /var/lib/apt/lists/*

RUN git clone --filter=blob:none https://github.com/llvm/llvm-project.git \
      /opt/llvm-project && \
    git -C /opt/llvm-project checkout --detach "${LLVM_REVISION}" && \
    cmake -S /opt/llvm-project/llvm -B /opt/llvm-build -G Ninja \
      -DCMAKE_BUILD_TYPE=Release \
      -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" \
      -DLLVM_TARGETS_TO_BUILD=X86 \
      -DLLVM_ENABLE_ASSERTIONS=ON \
      -DLLVM_ENABLE_RTTI=OFF && \
    cmake --build /opt/llvm-build \
      --target clang clang-format clang-tidy opt FileCheck not -j2 && \
    test -x /opt/llvm-build/bin/llvm-lit

# STP is the required M2 production solver and also parses M1/M2 SMT-LIB tests.
# MiniSat is pinned because it changes solver behavior and performance.
RUN git clone --filter=blob:none https://github.com/stp/minisat.git \
      /opt/minisat-source && \
    git -C /opt/minisat-source checkout --detach "${MINISAT_REVISION}" && \
    cmake -S /opt/minisat-source -B /opt/minisat-build -G Ninja \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=/opt/stp-deps && \
    cmake --build /opt/minisat-build -j2 && \
    cmake --install /opt/minisat-build && \
    git clone --filter=blob:none https://github.com/stp/stp.git \
      /opt/stp-source && \
    git -C /opt/stp-source checkout --detach "${STP_REVISION}" && \
    git -C /opt/stp-source submodule update --init --depth 1 && \
    cmake -S /opt/stp-source -B /opt/stp-build -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_INSTALL_PREFIX=/opt/stp-install \
      -DCMAKE_INSTALL_RPATH=/opt/stp-install/lib:/opt/stp-deps/lib \
      -DMINISAT_INCLUDE_DIR=/opt/stp-deps/include \
      -DMINISAT_LIBRARY=/opt/stp-deps/lib/libminisat.so \
      -DNOCRYPTOMINISAT=ON \
      -DONLY_SIMPLE=OFF \
      -DENABLE_ASSERTIONS=ON \
      -DENABLE_PYTHON_INTERFACE=OFF \
      -DPYTHON_EXECUTABLE=/usr/bin/python3 \
      -DENABLE_TESTING=ON && \
    cmake -S /opt/stp-build/lib/extlib-abc \
      -B /opt/stp-build/lib/extlib-abc/build \
      -DREADLINE_FOUND=FALSE \
      -DABC_SKIP_TESTS=ON && \
    cmake --build /opt/stp-build -j2 && \
    ctest --test-dir /opt/stp-build --output-on-failure && \
    cmake --install /opt/stp-build && \
    /opt/stp-install/bin/stp --version

ENV PATH=/opt/llvm-build/bin:/opt/stp-install/bin:${PATH}
ENV LD_LIBRARY_PATH=/opt/stp-install/lib:/opt/stp-deps/lib
WORKDIR /workspace
