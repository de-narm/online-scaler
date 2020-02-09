# online-scaler

A [re-frame](https://github.com/day8/re-frame) application designed to ... well, that part is up to you.

## Development Mode

```
npm install -g karma-cli
```

The karma wrapper is needed globally to run tests.

```
export CHROME_BIN=chromium
```

CHROME_BIN needs to be set and therefore some kind of Chrome need to be installed.

### Run application:

```
lein clean
lein dev
```

shadow-cljs will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:8280](http://localhost:8280).

## Production Build

