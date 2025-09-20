# Customer Billing Application

## Overview
This application is a Java-based migration of a COBOL batch application for processing customer billing data. It reads customer and billing records, applies billing rules, and updates customer balances.

## Prerequisites
- Java 17 or higher
- Maven
- Docker (optional, for containerization)

## Build Instructions
1. Navigate to the project directory:
   ```bash
   cd /path/to/java-migration
   ```
2. Build the project using Maven:
   ```bash
   mvn clean package
   ```

## Run Instructions
1. Run the application:
   ```bash
   java -jar target/customer-billing-1.0-SNAPSHOT.jar <customersFilePath> <billingFilePath>
   ```
   Replace `<customersFilePath>` and `<billingFilePath>` with the paths to your data files.

## Test Instructions
1. Run the unit tests:
   ```bash
   mvn test
   ```

## Docker Instructions
1. Build the Docker image:
   ```bash
   docker build -t customer-billing .
   ```
2. Run the Docker container:
   ```bash
   docker run -p 8080:8080 customer-billing
   ```

## Directory Structure
- `src/main/java`: Application source code
- `src/test/java`: Unit tests
- `Dockerfile`: Docker configuration
- `README.md`: Project documentation