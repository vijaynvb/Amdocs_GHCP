# todo-app

A minimal Spring Boot Todo application.

To build and run:

```bash
mvn -f todo-app clean package
java -jar todo-app/target/todo-app-0.0.1-SNAPSHOT.jar
```

API endpoints:
- GET /api/todos
- GET /api/todos/{id}
- POST /api/todos
- PUT /api/todos/{id}
- DELETE /api/todos/{id}
