---
description: "Prompt for converting existing COBOL-to-Java Spring Boot project from H2 to MongoDB, including importing COBOL data files into MongoDB collections."
mode: agent
tools: ['edit', 'search', 'new', 'runCommands', 'runTasks', 'usages', 'vscodeAPI', 'problems', 'changes', 'testFailure', 'openSimpleBrowser', 'fetch', 'githubRepo', 'extensions', 'todos', 'search']
model: GPT-4.1
---

# Conversion Prompt: H2 ➝ MongoDB

## Scenario
You already have a **Spring Boot project** that migrated COBOL Customer Billing to **H2 in-memory database**.  
Now you need to **replace H2 with MongoDB** and ensure that the **COBOL data files (`customers.dat`, `billing-input.dat`) are loaded directly into MongoDB collections**.

## Conversion Steps

1. **Update Dependencies**
   - Remove H2 dependency from `pom.xml`.
   - Add Spring Data MongoDB dependency:
     ```xml
     <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-mongodb</artifactId>
     </dependency>
     ```

2. **Update Configuration**
   - Remove H2 configuration (`application.properties`).
   - Add MongoDB configuration:
     ```properties
     spring.data.mongodb.uri=mongodb://localhost:27017/customer-billing
     ```

3. **Update Entities ➝ Documents**
   - Replace `@Entity` with `@Document`.
   - Replace `@Id` from `javax.persistence` with `@Id` from `org.springframework.data.annotation.Id`.
   - Example:
     ```java
     @Document(collection = "customers")
     public class Customer {
         @Id
         private String custId;
         private String custName;
         private String custAddr;
         private BigDecimal custBalance;
         private String custStatus;
     }
     ```

4. **Replace JPA Repositories ➝ Mongo Repositories**
   - Replace `extends JpaRepository<Customer, Long>` with `extends MongoRepository<Customer, String>`.

5. **Data Import from COBOL Files**
   - Use **JRecord** or a custom parser to read `customers.dat` and `billing-input.dat`.
   - Convert COBOL fields:
     - **Packed decimals (COMP-3)** ➝ `BigDecimal`
     - **Alphanumeric** ➝ `String` (trimmed)
   - Insert records into MongoDB using `MongoTemplate` or repository `.save()`.
   - Example:
     ```java
     mongoTemplate.save(customer, "customers");
     ```

6. **Adapt Business Logic**
   - Update service classes to fetch and update from MongoDB instead of H2.
   - Ensure billing updates persist back to MongoDB collections.

7. **Validation**
   - Verify COBOL records are visible in MongoDB Compass.
   - Run existing unit tests, update if needed for Mongo repositories.
   - Compare outputs with COBOL results to ensure correctness.

## Debugging Tips
- If data doesn’t appear, check collection names (`customers`, `billings`).
- If balances are wrong, check packed decimal decoding.
- Use `mongoTemplate.findAll(Customer.class, "customers")` for quick verification.

## Acceptance Criteria
- H2 is fully replaced by MongoDB in the existing project
- COBOL data files are parsed and imported into MongoDB collections
- Billing logic executes correctly using MongoDB
- Output matches COBOL results

---
_Use this prompt to convert an existing H2-based COBOL migration project into MongoDB with COBOL data files loaded into collections._
