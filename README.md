# bucketgame

simple web implementation of the bucket game:

- server tracks a single game in memory
- client allows players to each add names to the game
- starting a round sets up a fresh bucket with all names shuffled inside
- players can then take names out of the bucket

to run, `stack run` brings the game server up on port 3000, then serve `index.html`, potentially by e.g. `python -m SimpleHTTPServer 8000`
