# cdda-dear-diary

[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

A personal and very simple CLI tool to parse CDDA's diary format and convert to JSON.

Note that I'm just doing this to explore CDDA diary format (idk what's the spec, so I'm just parsing them
by reading my own gameplay diaries. lol).

## Usage
```
Usage: cdd-cli DIARY_FILENAME [-o|--output-file ARG] [-c|--compact] 
               [-m|--extended-new-missions ARG]

  Convert CDDA's diary file to JSON format

Available options:
  DIARY_FILENAME           Diary file location
  -o,--output-file ARG     JSON output file location. This will write to the
                           output file instead of printing to STDOUT.
  -c,--compact             Output JSON in compact format
  -m,--extended-new-missions ARG
                           Extra new missions to support. Example: "Find a
                           cat,Hug A Star,Pat the dog"
  -h,--help                Show this help text


# Example
cdd-cli "$HOME/.config/cdda/save/WorldName/CharacterName.txt"
```

### Examples
See [Diary.txt](/examples/Diary.txt) and [Diary.json](/examples/json/Diary.json)
for conversion example.

## Caveats
Currently `newMissions` key depends on `allowedMissions` list within [CddaDearDiary.Internal.Parser.Body](/cdda-dear-diary/src/CddaDearDiary/Internal/Parser/Body.hs).

If your diary contains mission names that are not listed in that list, then this
program will most likely ended up generating garbled data format.

Good news is, while not optimal, you could extend allowed missions at runtime by passing
`cdd-cli --extended-new-missions "Shoot A Star,Pats A Cat"` to add missions that
are not listed.

This may get improved in future versions when I figure how to do this robustly
or if when `New missions:` raw text format will have a proper *ending* indicator.

However, for my own personal gameplay at the moment, this is more than sufficient.

## Pre-built Linux executable
See [Releases page](https://github.com/cloudyluna/cdda-dear-diary/releases).

## Requirements & building

### Requirements
- Haskell `ghc` >= 9.6 (if possible, installed through `ghcup`).
- `cabal` (if possible, installed through `ghcup`).
- Internet connection (of course).

### Building & install
1. `git clone https://github.com/cloudyluna/cdda-dear-diary`.
2. `cd cdda-dear-diary`.
3. `cabal install cdda-dear-diary-cli -O2`
4. `cdd-cli` now will be available in `PATH`. If not, see `$HOME/.local/bin/cdd-cli` if it exists.

You could also running without installing it by `cabal run cdda-dear-diary-cli -O2 -- DIARY_FILENAME`.


### If you use Nix flakes

This project is built with `nix` and flakes. You could use `nix develop` and get all the requirements
installed easily and continue on with building steps above.

### License

BSD-3-Clause

