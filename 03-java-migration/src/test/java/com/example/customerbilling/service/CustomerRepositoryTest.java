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

class CustomerRepositoryTest {

    private CustomerRepository customerRepository;

    @BeforeEach
    void setUp() {
        customerRepository = new CustomerRepository();
    }

    @Test
    void testLoadCustomers() throws IOException {
        String testFilePath = Objects.requireNonNull(getClass().getClassLoader().getResource("test-customers.dat")).getPath();
        customerRepository.loadCustomers(testFilePath);

        CustomerRecord customer = customerRepository.findCustomerById(123456);
        assertNotNull(customer);
        assertEquals("John Doe", customer.getCustomerName());
        assertEquals(new BigDecimal("1000.00"), customer.getCustomerBalance());
    }

    @Test
    void testFindCustomerById_NotFound() {
        assertNull(customerRepository.findCustomerById(999999));
    }
}