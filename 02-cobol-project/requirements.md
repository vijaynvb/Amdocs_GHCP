You are tasked with building a COBOL-based customer billing application.

processes customer billing data from two input files: billing-input.dat and customers.dat available in the data directory. The application reads customer records and billing records, updates customer balances, and provides a summary of the total billed amount.

- Reads each billing record from billing-input.dat 
- Splits the billing record into customer ID and amount.
- If the customer ID is found in customers.dat and the customer is active, updates their balance and increments the total billed amount.