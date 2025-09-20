# Java Customer Billing (COBOL Migration)

Spring Boot app that migrates the COBOL Customer Billing sample to Java with H2. It loads customers from the fixed-width file and applies billing events from CSV, handling packed decimals via a utility.

## Run

```powershell
# From repo root (adjust paths if needed)
cd java-customer-billing
mvn -q -DskipTests spring-boot:run
```

H2 console: http://localhost:8080/h2 (JDBC URL `jdbc:h2:mem:billingdb`)

## Tests

```powershell
mvn -q test
```
