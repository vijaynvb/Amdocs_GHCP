package com.example.customerbilling.service;

import com.example.customerbilling.model.CustomerRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;

class BillingProcessorTest {

    private CustomerRepository customerRepository;
    private BillingProcessor billingProcessor;

    @BeforeEach
    void setUp() {
        customerRepository = new CustomerRepository();
        billingProcessor = new BillingProcessor(customerRepository);
    }

    @Test
    void testProcessBilling() throws IOException {
        String customersFilePath = Objects.requireNonNull(getClass().getClassLoader().getResource("test-customers.dat")).getPath();
        String billingFilePath = Objects.requireNonNull(getClass().getClassLoader().getResource("test-billing.dat")).getPath();

        customerRepository.loadCustomers(customersFilePath);
        billingProcessor.processBilling(billingFilePath);

        CustomerRecord customer = customerRepository.findCustomerById(123456);
        assertNotNull(customer);
        assertEquals(new BigDecimal("1500.00"), customer.getCustomerBalance());
    }

    @Test
    void testProcessBilling_CustomerNotFound() throws IOException {
        String customersFilePath = Objects.requireNonNull(getClass().getClassLoader().getResource("test-customers.dat")).getPath();
        String billingFilePath = Objects.requireNonNull(getClass().getClassLoader().getResource("test-billing-invalid.dat")).getPath();

        customerRepository.loadCustomers(customersFilePath);
        billingProcessor.processBilling(billingFilePath);

        // Ensure no exceptions and proper logging for invalid customer IDs
    }
}