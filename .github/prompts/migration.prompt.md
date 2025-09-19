---
description: "Prompt template for migrating a COBOL batch application to Java"
mode: agent
model: GPT-4o

---

Task: Migrate the COBOL batch project at /Users/vijay/Desktop/Amdocs/workspace/Amdocs_GHCP/02-cobol-project to a maintainable Java project (Maven/Gradle), preserving business logic, data semantics, and producing regression-equivalent outputs.

Project snapshot (important files)
- src/customer-billing.cob (main COBOL program)
- src/customer-billing-test.cob
- src/copybooks/CUSTOMER-RECORD.cbk
- data/billing-input.dat
- data/customers.dat
- Dockerfile, docs/customer_billing.png

Goals
1. Produce a Java implementation that reproduces COBOL behavior and output for provided data files.
2. Improve modularity, testability, and observability.
3. Prepare build, run, and containerization (Docker) steps; include unit and integration tests for regression verification.

Constraints & assumptions
- Keep file I/O semantics: line-sequential, plain-text input files in data/.
- Preserve numeric precision and string handling. Flag copybook numeric types (COMP, COMP-3) for special handling.
- No mainframe-only features assumed (no CICS/JCL). If EBCDIC/packed decimals exist, identify and plan conversions.
- Business rule note: FIND-CUSTOMER is a stub in COBOL; implementation must be defined/confirmed.

Acceptance criteria / deliverables
- Maven/Gradle Java project skeleton with src/main/java and src/test/java.
- Java service/class(es) that implement billing flow: read customers, read billing records, match customer, apply billing rules, update balances, counters.
- Unit tests that assert same counters and sample outputs as COBOL run against data/*.dat.
- Integration test or script that runs the Java app and verifies output equals COBOL output for baseline dataset.
- README with build/run/test instructions and Docker image build/run steps.

Migration approach (step-by-step)
1. Static analysis: parse COBOL source and copybook to extract record layouts, fields, and data types.
2. Map records to Java POJOs (use BigDecimal for monetary values; LocalDate for dates).
3. Implement file readers: streaming line-based readers that parse delimited lines into POJOs (handle UNSTRING, delimiters).
4. Implement services:
   - CustomerRepository (loads customers.dat into a Map by ID)
   - BillingProcessor (reads billing-input.dat, performs logic, updates balances, increments counters)
5. Translate control flow:
   - PERFORM loops -> for/while methods
   - READ/AT END -> BufferedReader with EOF handling
   - COMPUTE/FUNCTION NUMVAL-C -> BigDecimal parsing with locale-safe decimal separators
6. Implement logging, metrics, and error handling. Replace DISPLAY with structured logging.
7. Write unit tests for parsers, repository, and processor; add integration test comparing outputs.
8. Run profile/benchmark for performance; tune I/O and data structures.



Testing & regression
- Use the existing data/ files as golden input. Capture COBOL-run outputs and define them as expected fixtures.
- Add unit tests that cover edge cases: missing fields, zero/negative balances, inactive customers.
- Include a reproducible test that runs the Java app and diffs outputs against COBOL results.

Risks & open items
- Unclear copybook numeric formats (COMP-3 or packed decimals) — must inspect copybook.
- FIND-CUSTOMER logic not implemented in COBOL: need business owner clarification.
- Encoding differences (EBCDIC) — detect and handle if present.
- Non-functional requirements (throughput, concurrency) must be confirmed.

Acceptance questions for stakeholders (to resolve before coding)
- Confirm monetary precision and rounding rules.
- Confirm intended behavior of FIND-CUSTOMER and inactive customer handling.
- Confirm whether to preserve exact output formatting or only numeric/result parity.

Use this prompt to drive automated/code-assistant migration tasks, and attach COBOL source + data files for analysis. Run a COBOL-to-Java pilot for the customer-billing.cob flow first, then generalize.