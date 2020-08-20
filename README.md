## *Disclaimer*

*This source code was written as part of a project at the "Universität Kassel Fachgebiet Wissensverarbeitung Fachbereich 16" by Sebastian Benner. The project was supervised by Maximilan Felde.*

# online-scaler

A [re-frame](https://github.com/day8/re-frame) application designed to scale many-valued contexts into formal contexts.

## Development Mode

```
npm install -g karma-cli
```

The karma wrapper is needed globally to run tests.

```
export CHROME_BIN=chromium
```

CHROME_BIN needs to be set and therefore some kind of Chrome needs to be installed.

### Run application:

```
lein clean
lein dev
```

shadow-cljs will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:8280](http://localhost:8280).

## Testing

```
lein karma
```

Once again CHROME_BIN needs to be set.

## Production Build

