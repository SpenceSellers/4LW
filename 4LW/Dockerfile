FROM haskell:8.0.1

# Stack really wanted to use its own version of GHC, which it didn't have any.
# Let's tell it to use the dockerfile's one instead.
RUN stack config set system-ghc --global true

COPY . .
RUN stack build

# Mount assembly file at data/asm.4lw
# You'll need to run as interactive since 4LW accepts user input
# (Todo: Add an option where the above isn't true.)

CMD ["stack", "exec", "4LW", "--", "data/asm.4lw"]