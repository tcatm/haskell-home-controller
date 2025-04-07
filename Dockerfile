# Stage 1: Build the Haskell binary
FROM haskell:latest AS builder
WORKDIR /app

COPY *.cabal ./
COPY cabal.project* ./

RUN cabal update
RUN cabal v2-build --dependencies-only

COPY src/ ./src/
RUN cabal v2-install --installdir=/app/bin --overwrite-policy=always .

# Stage 2: Create a minimal final image
FROM debian:bullseye-slim
WORKDIR /app

# Copy the binary from the builder stage.
COPY --from=builder /app/bin/homecontrol /usr/local/bin/homecontrol

# Copy webinterface
COPY web/ /app/web/

# Set the entrypoint.
ENTRYPOINT ["/usr/local/bin/homecontrol"]

