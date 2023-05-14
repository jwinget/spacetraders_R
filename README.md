
# spacetraders

<!-- badges: start -->
<!-- badges: end -->

A janky interface for spacetraders.io, written in R/shiny

![Shiny interface](https://github.com/jwinget/spacetraders/blob/main/spacedash/www/img/screenshot.PNG?raw=true)

## Installation

I don't know, I haven't tested it.
You can try installing it like a package, but that probably won't work.

The dependencies are listed in DESCRIPTION. Make sure those are installed.
Then you can either call the functions from the console or run the shiny app in `spacedash/`

## Examples

Here is roughly how to do the tutorial, but using this interface:

### Create a new agent
`new_agent(base_url, "AGENT_NAME")`

Note: After creating a new agent you will need to load your `token.txt` using `token <- readLines(here::here("token.txt"))`. Afterward the token should be automatically read.

### See agent details
`agent_info(token, base_url)`

### Accept a contract
TBD. Can be done using raw `send_request()` functionality

### View contract information
`contracts(token, base_url)`

### Buy a ship
TBD. Can be done using raw `send_request()` functionality

### Navigation
`navigate(token, base_url, "BIONIC-1", "X1-ZA40-99095A")`

### Dock, Refuel, Orbit
```
dock(token, base_url, "BIONIC-1")
refuel(token, base_url, "BIONIC-1")
orbit(token, base_url, "BIONIC-1")
```

### Extract minerals
`extract(token, base_url, "BIONIC-1")`

### Run all ships in a simple mining & delivery loop
`run_swarm(token, base_url)`

## Known bugs

* Ships currently get "stuck" sometimes when delivering cargo. They can be "bumped" using the shiny app
