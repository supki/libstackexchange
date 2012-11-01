#libstackexchange

Haskell interface to [Stack Exchange API v2.1][1]

##Getting started

You talk to StackExchange API server via `Request`s: quite simple data type
containing server host name, API call path, query parameters and so on. Simple
request would be like that:

```haskell
request = questions
```

Yeah, that simple. Furthermore, `Request` is a `Monoid`. This means you
can easy construct and reuse arbitrarily complex requests:

```haskell
common = site "stackoverflow" <> key "12345678"
questions' = questions <> common
answers' = answers <> common
```

To get response from request, you need to actually send request to Stack Exchange:

```haskell
response = askSE $ questions'
```



## Authentication

Some API calls require authentication. The main goal is to get `access_token` via quite
[sophisticated process][3]. After that, you can freely call this priviledged API:


```haskell
response = askSE $ token "12345678" $ me <> key "12345678"
```

Note, that

```haskell
response = askSE $ me <> key "12345678"
```

is a type error, so you are saved from accidentally malformed requests.

## Retrieving Data

StackExchange responses are, basically, wrapped [aeson][2] data structure, so you can
access data via ordinary aeson parsers. But, for /hardcore/ users, there are `field` and
`fields` actions (in [lens][4] sense) that cover the most common task of getting
specific JSON field(s).

 [1]: https://api.stackexchange.com/docs
 [2]: http://hackage.haskell.org/package/aeson
 [3]: https://api.stackexchange.com/docs/authentication
 [4]: http://hackage.haskell.org/package/lens