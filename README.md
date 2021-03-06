# haskell-and-elm

Example application using Haskell([servant](http://haskell-servant.readthedocs.io/en/stable/)) and Elm.

API functions in Elm is generated by [mattjbray/servant-elm](https://github.com/mattjbray/servant-elm)(Elm type is generated by [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export)).

Moreover, Type that communicated by API is written using [fumieval/extensible](https://github.com/fumieval/extensible) !!

Example application is referenced to [this post](http://qiita.com/lotz/items/883b41fa79f060e59efa).

## Run

```bash
$ stack build --test
  .
  .
  .
haskell-and-elm-0.1.0.0: test (suite: generate)

Writing: elm-src/Generated/TodoAPI.elm
Success! Compiled 2 modules.
Successfully generated static/main.js

Completed 2 action(s).

$ stack exec -- server
Listening on port 8080
```

![](asset/sample.jpg)
