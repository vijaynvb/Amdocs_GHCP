       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CUSTOMER-BILLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE    ASSIGN TO "data/billing-input.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUSTFILE  ASSIGN TO "data/customers.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  IN-REC           PIC X(200).
       FD  CUSTFILE.
       01  CUST-REC-FLAT    PIC X(200).

       WORKING-STORAGE SECTION.
       COPY "src/copybooks/CUSTOMER-RECORD.cbk".
       77  WS-INFILE-EOF    PIC X VALUE 'N'.
       77  WS-CUSTFILE-EOF  PIC X VALUE 'N'.
       77  WS-OUT-LINE      PIC X(200).
       01  WS-COUNTERS.
           05  TOTAL-READ   PIC 9(9) VALUE 0.
           05  TOTAL-BILLED PIC 9(9) VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PARA.
           DISPLAY "Starting MAIN-PARA"
           OPEN INPUT INFILE
           OPEN INPUT CUSTFILE
           PERFORM READ-CUSTOMERS
           PERFORM PROCESS-BILLING
           CLOSE INFILE
           CLOSE CUSTFILE
           DISPLAY "Processing complete. Total billed: " TOTAL-BILLED
           GOBACK.
           .

       READ-CUSTOMERS.
           MOVE 'N' TO WS-CUSTFILE-EOF
           DISPLAY "Starting READ-CUSTOMERS"
           PERFORM UNTIL WS-CUSTFILE-EOF = 'Y'
               READ CUSTFILE
                   AT END
                       MOVE 'Y' TO WS-CUSTFILE-EOF
                       DISPLAY "End of CUSTFILE in READ-CUSTOMERS"
                   NOT AT END
                       ADD 1 TO TOTAL-READ
                       DISPLAY "Read customer record, TOTAL-READ: " TOTAL-READ
               END-READ
           END-PERFORM
           .

       PROCESS-BILLING.
           MOVE 'N' TO WS-INFILE-EOF
           DISPLAY "Starting PROCESS-BILLING"
           PERFORM UNTIL WS-INFILE-EOF = 'Y'
               READ INFILE
                   AT END
                       MOVE 'Y' TO WS-INFILE-EOF
                       DISPLAY "End of INFILE in PROCESS-BILLING"
                   NOT AT END
                       ADD 1 TO TOTAL-READ
                       DISPLAY "Read billing record, TOTAL-READ: " TOTAL-READ
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM
           .

       PROCESS-RECORD.
           DISPLAY "Processing record: " IN-REC
           UNSTRING IN-REC DELIMITED BY ','
               INTO CUST-ID, WS-OUT-LINE
           END-UNSTRING
           IF CUST-ID NOT = 0
               PERFORM FIND-CUSTOMER
               IF CUST-STATUS = 'A' OR CUST-STATUS = ' '
                   COMPUTE CUST-BALANCE = CUST-BALANCE + FUNCTION NUMVAL-C(WS-OUT-LINE)
                   ADD 1 TO TOTAL-BILLED
                   DISPLAY "Billed customer: " CUST-ID " New balance: " CUST-BALANCE
               END-IF
           END-IF
           .

       FIND-CUSTOMER.
           DISPLAY "Starting FIND-CUSTOMER for: " CUST-ID
           DISPLAY "FIND-CUSTOMER logic not implemented"
           .
