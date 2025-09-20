---
description: "Prompt for migrating COBOL Customer Billing to Java Spring Boot with H2 database, focusing on packed decimals and alphanumerics."
mode: agent
tools: ['edit', 'search', 'new', 'runCommands', 'runTasks', 'usages', 'vscodeAPI', 'problems', 'changes', 'testFailure', 'openSimpleBrowser', 'fetch', 'githubRepo', 'extensions', 'todos', 'search']
model: GPT-4.1
---

# COBOL-to-Java Migration Prompt

## Project Overview
This prompt guides you through migrating the COBOL Customer Billing project to a Java Spring Boot application, using an H2 in-memory database. The focus is on tricky data conversions such as packed decimals and alphanumeric fields, and persisting customer and billing data in H2.

## Migration Scenario
You are tasked with converting the COBOL program (`customer-billing.cob`) and its data structures (from `CUSTOMER-RECORD.cbk`) to a Java Spring Boot application. The main challenges are:
- Handling packed decimal fields (COMP-3)
- Correctly reading and writing fixed-width alphanumeric fields
- Mapping COBOL data to JPA entities and storing them in H2

## Key Data Structures
- **CUST-ID**: Numeric, 6 digits
- **CUST-NAME**: Alphanumeric, 30 chars
- **CUST-ADDR**: Alphanumeric, 50 chars
- **CUST-BALANCE**: Packed decimal (COMP-3), 7 digits before and 2 after decimal
- **CUST-STATUS**: Alphanumeric, 1 char

## Migration Steps
1. **Set Up Spring Boot Project**
   - Create a new Spring Boot application with dependencies for Spring Data JPA and H2 database.

2. **Define JPA Entities**
   - Map COBOL data structures (from `CUSTOMER-RECORD.cbk`) to Java classes annotated with `@Entity`.
   - Use appropriate field types for packed decimals and alphanumerics.

3. **Read COBOL Data Files in Java**
   - Use a Java library (e.g., JRecord) to parse fixed-width and packed decimal fields.
   - For packed decimals, ensure correct decoding (do not treat as plain integer/float).
   - For alphanumerics, trim trailing spaces and handle encoding (EBCDIC/ASCII).
   - Store parsed records in H2 using Spring Data JPA repositories.

4. **Convert Business Logic**
   - Port the billing logic from COBOL to Java services, ensuring calculations and updates match COBOL results.
   - Persist updates to H2 database.

5. **Validate Output**
   - Compare Java output with COBOL output.
   - Write Java unit tests to check for correct decoding, business logic, and database persistence.

## Debugging Tips

- If balances are incorrect, check packed decimal decoding and JPA field mapping.
- If names/addresses look wrong, check for padding, encoding issues, and trimming in entity setters.
- Use sample data from `customers.dat` and `billing-input.dat` for testing.
- Use H2 console to inspect persisted data during development.

## Example Java Code Snippet
```java
// Example: Spring Boot JPA Entity for Customer
@Entity
public class Customer {
   @Id
   private Long custId;
   private String custName;
   private String custAddr;
   private BigDecimal custBalance; // decoded from packed decimal
   private String custStatus;
   // getters, setters, etc.
}

// Example: Decoding packed decimal using JRecord
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
// ...
// Use LayoutDetail to define COBOL copybook structure
// Read and decode packed decimal fields
```

## Acceptance Criteria
- Spring Boot application reads and processes COBOL data files
- Data is correctly persisted and updated in H2 database
- Output matches COBOL results
- All tricky fields (packed decimal, alphanumerics) are handled accurately

---
_Use this prompt to guide your migration and debugging process for COBOL-to-Java conversions in the Customer Billing project._
