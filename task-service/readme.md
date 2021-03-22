# Task list service

## Nix notes

Project is organized this way: https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.htm

## TODO

Docker image: https://github.com/redoracle/nixos


Add Task:

```
curl -d '{"_name":"test4", "_description":"12345"}' -H "Content-type: application/json" -X POST http://localhost:8090/add
```


## Swagger

Положить https://github.com/swagger-api/swagger-ui/tree/master/dist в директорию `swagger`

```
npx http-server swagger
```

