# Use an official Ubuntu base image
FROM ubuntu:22.04

# Install dependencies and GnuCOBOL
RUN apt-get update && \
    apt-get install -y gnucobol && \
    apt-get clean

# Set the working directory
WORKDIR /app

# Copy COBOL source, copybooks, and data files
COPY src/ ./src/
COPY data/ ./data/

# Copy test file
COPY src/customer-billing-test.cob ./src/

# Compile the COBOL program
#RUN cobc -free -x -o customer-billing ./src/customer-billing.cob

# Compile the COBOL program as a module (shared object)
RUN cobc -free -m -o CUSTOMER-BILLING.so ./src/customer-billing.cob

# Compile the COBOL test program
RUN cobc -free -x -o customer-billing-test ./src/customer-billing-test.cob

# Run the test and validate output
#RUN LD_LIBRARY_PATH=. ./customer-billing-test | grep 'TEST PASSED'

# Set the default command to run the program
CMD ["./customer-billing"]
