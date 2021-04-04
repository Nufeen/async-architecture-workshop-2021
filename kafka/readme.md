# Queue

По гайду: https://kafka-tutorials.confluent.io/creating-first-apache-kafka-producer-application/kafka.html#get-confluent-platform

Start: `docker-compose up`

Создать топик:

```
docker-compose exec broker bash

kafka-topics --create --topic tasks --bootstrap-server broker:9092 --replication-factor 1 --partitions 1
```

либо:

```
nix-shell -p kafkacat

kafkacat -b localhost:9092 -t new_topic -P
```

Проверить что всё консьюмится:

```
nix-shell -p kafkacat

kafkacat -b localhost:29092 -t tasks -K:
```