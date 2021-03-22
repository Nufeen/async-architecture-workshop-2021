# Сервисы / домены

## Сервис авторизации

Table: users

#### API:

- login

## Таск-трекер

Table: tasks

#### API:

GET:

- getTasks
- getUserTasks

POST:

- addTask
- closeTask
- reassignTasks

SIDE EFFECTS:

- notifyUsers

## Сервис транзакций (аккаунтинг)

Table: transactions

#### API:

GET:

- getTransactions
- getUserInvoice
- getDailyManagementIncome
- getGoldenTask

POST:

- setTaskPrice
- writeOff
- charge
- clearBalanceSheet

SIDE EFFECTS:

- logPayments
- notifyUsers
