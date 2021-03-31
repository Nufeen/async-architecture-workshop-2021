## Event storming

Пока флексим все CUD Events, оставляя только BE

### BE

```
|- users -> {task service} addTask -> [Event] taskCreated -> {consumer: task service} setTaskPrice (в нашем примере флексим)

|- users -> {task service} closeTask -> [Event] tasksClosed -> {consumer: accounting service} charge

|- users[isAdmin | isModerator] -> {task service} reassignTasks -> [Event] taskReassigned -> {consumer: accounting service} writeOff

|- [Event] CRON -> {consumer: accounting service} clearBalanceSheet -> [Event] wagesPaid -> {consumer: accounting service} logPayments
                                                                                         -> {consumer: accounting service} notifyUsers
```
