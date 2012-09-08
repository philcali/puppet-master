# Puppet Master

A scripted [LMXML][lmxml] HTTP client that uses [dispatch][2] under the covers.

This is the web crawler to rule them all.

## Motivation

Every developer needs to write a screenscraper application once in a while. The
problem scope is usually a unique one (like download a series of mp3's,
for example). The actions that fulfill that problem scope could have been
rewritten as a series of miniature, composable actions.
That's what puppet master does.

Puppet Master uses open source technologies to transform an LMXML file
containing a series of recursive `actions`, to do something with a headless,
automated browser. The _program_ to download all of the html files from a
specific web page link is now contained in an LMXML file.

## Configuring

A puppet client is simply a [configured][3] `AsyncHttpClient`. Here's an
example LMXML configuring a client to follow redirects, keep alive's, and
setting the user agent to Mozilla:

```
browser
  config
    user-agent "Mozilla/5.0"
    follow-redirects "true"
    keep-alive "true"
```

__Note__: The configured client can be stored in a properties file and loaded
from there, via `browser @file="config.properties"`.

## Actions

A puppet `action` is simply a transformation function that transforms a parsed
node into controlled action on the configured browser. All actions are written
recursively under the `instructions` node.

```
browser @file="config.properties"
  instructions
    go @to="http://google.com" println
```

Puppet master ships with quite a few actions:

- `go`: makes a GET request
- `println`: prints out the source to stdout.
- `find`: finds specific html nodes by css, and loads them in `find-results`
- `set`: sets a value to be used later in the script context
- `each`: iterates over a collection of `find-results`
- `download`: writes response to a file

__Note__: There is a plan for a lot more default actions.

Instructions can be programmatically aliased, removed, or added.

``` scala
import actions._

val instructions = Instructions("get" -> GoAction)

val promises = Lmxml.fromFile("instructions.lmxml")(instructions)

promises foreach (_ foreach println)
```

[lmxml]: https://github.com/philcali/lmxml
[2]: https://github.com/dispatch/reboot
[3]: http://sonatype.github.com/async-http-client/configuring.html
