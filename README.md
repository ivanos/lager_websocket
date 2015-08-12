# lager_websocket
lager event handler for websocket clients. lager_websocket is a lager sink that sends messages received from lager to clients connected via websocket.

% URL
.../lager/websocket - websocket

% Message format
Messages are delivered as JSON objects.

Field | Value
----- | -----
logid | log identifier - these increase sequentially
message | body of the message
date | date (YYYY-MM-DD format)
time | time (HH:MM:SS format)
severity | message severity (e.g., debug, info, warning, error)
metdata | JSON object with additional information about the message
```
{
    "logid":12,
    "message":"Lager installed handler lager_backend_throttle into lager_event"
    "date":"2015-08-11",
    "time":"15:43:08.227",
    "severity":"debug",
    "metadata":{
        "pid":"<0.144.0>",
        "line":94,
        "file":"src/lager_handler_watcher.erl",
        "module:"lager_handler_watcher"
    }
}
```

# Config Variables
Name | Value | Example | Description
---- | ----- | ------- | -----------
clean_interval_sec | integer | 1 | number of seconds between archive purges
max_retention_minutes | integer | 60 | oldest log record allowed in archive
new_message_count | integer | 5 | number of past messages to send when a client subscribes
