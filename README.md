# LUI - Laminar UI components

Scala library with a small number of UI components for laminar.

## Development

This is a sbt project configured specifically for Scala.js and Vite.

### Starting dev server

First thing in order to run this project, is to install vite dependency. To do it run
`npm install`
To start Scala.js - Vite developement server, you first need to start incremental compilation for code.
It is done by running `sbt ~fastLinkJS` directly from shell of just `~fastLinkJS` from SBT shell instance.
The next step is to start the server. To do it, run `yarn dev` or if you use npm `npm run dev` in your terminal.

## CSS dependency

The components from the Stack design depend on the Stack CSS. See package.json for the version.




