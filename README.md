
# spacetraders

<!-- badges: start -->
<!-- badges: end -->

A janky interface for spacetraders.io, written in R/shiny.

![Shiny interface](https://github.com/jwinget/spacetraders_R/blob/main/spacedash/www/img/screenshot.PNG?raw=true)

## Installation

I don't know, I haven't tested it.
You can try installing it like a package, but that probably won't work.

The dependencies are listed in DESCRIPTION. Make sure those are installed.
Then you can either call the functions from the console or run the shiny app in `spacedash/`

## Examples

Here is roughly how to do the tutorial, but using this interface:

### Create a new agent
```
game(pool,
  new_game = TRUE,
  symbol = "MY_NAME",
  faction = "COSMIC")
```

## Known bugs

