## PureScript Pong Example

A Pong-like game implemented by following this guide: https://robots.thoughtbot.com/pong-clone-in-javascript

I was never able to figure out the physics and collision detection in my past attempts at implementing Pong, so following the guide was really helpful and motivating. Thanks, Matt Mongeau!

### Building and running

```
# If you don't have dependencies installed, like PS and Yarn and psc-package
$ nix-shell nix
$ yarn install
$ npm run build
$ python -m SimpleHTTPServer 8080 # Or similar
# Now visit localhost:8080/index.html in a browser.
```
