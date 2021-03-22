## Сервис авторизации

Авторизация [во всех сервисах] должна выполняться через общий сервис авторизации UberPopug Inc

- Actor: users
- Command: login
- Data: users [r]
- Event: loginSuccess | loginFailed

## Таск-трекер

1. Таск-трекер должен быть отдельным дашбордом и доступен всем сотрудникам (В таск-трекере должны быть только задачи. Проектов, скоупов и спринтов никому не надо, ибо минимализм)

- Actor: users
- Command: getTasks
- Data: tasks [r], users [r]
- Event: -

4. В таск-трекере новые таски может создавать кто угодно. У задачи должны быть описание, статус (выполнена или нет) и попуг, на которого заассайнена задача.

- Actor: users
- Command: addTask
- Data: tasks [w]
- Event: taskCreated

5. Менеджеры или администраторы должны иметь кнопку «заассайнить задачи», которая возьмет все открытые задачи и рандомно заассайнит каждую на любого из сотрудников. Не успел закрыть задачу до реассайна — сорян, делай следующую.

- Actor: [users | user.isAdmin or user.isModerator ]
- Command: reassignTasks
- Data: users [r], tasks [w]
- Event: tasksReassigned

6.  Каждый сотрудник должен иметь возможность видеть в отдельном месте список заассайненных на него задач + отметить задачу выполненной.

- Actor: users
- Command: getUserTasks
- Data: tasks [r]
- Event: -

>

- Actor: users
- Command: closeTask
- Data: tasks [w]
- Event: taskClosed

7. После ассайна новой задачи сотруднику должно приходить оповещение на почту, в слак и в смс.

- Actor: [Event] tasksReassigned
- Command: notifyUsers
- Data: tasks [r]
- Event: notificationsSent

## Аккаунтинг: кто сколько денег заработал

1. Аккаунтинг должен быть в отдельном дашборде и доступным только для администраторов и бухгалтеров.

- Actor: [user | user.isAdmin or user.isBooker]
- Command: getTransactions
- Data: transactions [r], users [r]
- Event: -

3. У каждого из сотрудников должен быть свой счет, который показывает, сколько за сегодня он получил денег. У счета должен быть аудитлог того, за что были списаны или начислены деньги, с подробным описанием каждой из задач.

- Actor: users
- Command: getUserInvoice
- Data: users, transactions, tasks
- Event: -

4. Расценки: цена на задачу определяется единоразово, в момент ее появления в системе (можно с минимальной задержкой)

- Actor: [Event] taskCreated
- Command: setTaskPrice
- Data: tasks [r]
- Event: -

**Дополнение:** деньги списываются сразу после ассайна на сотрудника, а начисляются после выполнения задачи.

- Actor: [Event] tasksReassigned
- Command: writeOff
- Data: transactions [w], tasks [r], users [r]
- Event: -

>

- Actor: [Event] tasksClosed
- Command: charge
- Data: transactions [w], tasks [r], users [r]
- Event: -

5. Вверху выводить количество заработанных топ менеджером за сегодня денег.

- Actor: [user | user.isAdmin or user.isBooker]
- Command: getDailyManagementIncome (противоречие в условиях)
- Data: tasks [r]
- Event: -

2. В конце дня необходимо считать, сколько денег сотрудник получил за рабочий день, слать на почту сумму выплаты.

- Actor: [Event] CRON
- Command: countDailyWages.map(notifyUsers)
- Data: transactions [r], tasks [r]
- Event: notificationsSent

7. После выплаты баланса (в конце дня) он должен обнуляться и в аудитлоге должно быть отображено, что была выплачена сумма.

- Actor: [Event] CRON
- Command: clearBalanceSheet
- Data: transactions [w]
- Event: wagesPaid

>

- Actor: [Event] wagesPaid
- Command: logPayments
- Data: transactions [r]
- Event: -

## Аналитика

1. Аналитика — это отдельный дашборд, доступный только админам.
2. Нужно указывать, сколько заработал топ-менеджмент за сегодня: сколько попугов ушло в минус.

- Actor: [user | user.isAdmin]
- Command: getDailyManagementIncome
- Data: transactions [r]
- Event: -

3. Нужно показывать самую дорогую задачу за: день, неделю и месяц.

- Actor: [user | user.isAdmin]
- Command: getGoldenTask
- Data: tasks [r]
- Event: -
