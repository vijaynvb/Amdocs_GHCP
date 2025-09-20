package com.example.billing;

import com.example.billing.domain.Customer;
import com.example.billing.repo.CustomerRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
class DataLoaderIntegrationTest {

    @Autowired
    CustomerRepository repo;

    @Test
    void loadsAndAppliesBilling() {
    // DataLoader runs @PostConstruct, so data should be loaded
    assertTrue(repo.count() >= 2);
    Customer c1 = repo.findById("000001").orElseThrow();
    Customer c2 = repo.findById("000002").orElseThrow();
    assertNotNull(c1.getCustBalance());
    assertNotNull(c2.getCustBalance());
    // Based on sample input: 1: 150.75 + 49.95 = 200.70, 2: 200.00
    assertEquals(new BigDecimal("200.70"), c1.getCustBalance());
    assertEquals(new BigDecimal("200.00"), c2.getCustBalance());
    }
}
