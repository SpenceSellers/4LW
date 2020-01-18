FROM haskell:8.0.1

# Stack really wanted to use its own version of GHC, which it didn't have any.
# Let's tell it to use the dockerfile's one instead.
RUN stack config set system-ghc --global true

COPY . .
RUN mkdir /dest
RUN stack install --local-bin-path /dest


# With stack:
# CMD ["stack", "exec", "4LW", "--", "data/asm.4lw"]

# It should be possible to multi-stage build this.

# Mount assembly file at data/asm.4lw
# You'll need to run as interactive since 4LW accepts user input
# (Todo: Add an option where the above isn't true.)
CMD ["./dest/4LW", "data/asm.4lw"]