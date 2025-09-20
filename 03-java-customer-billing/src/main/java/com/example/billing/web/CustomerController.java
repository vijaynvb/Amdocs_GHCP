package com.example.billing.web;

import com.example.billing.domain.Customer;
import com.example.billing.repo.CustomerRepository;
import com.example.billing.web.dto.CustomerBalanceDto;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;


/**
 * REST controller for managing customer data and balances.
 * <p>
 * Provides endpoints to retrieve all customers, customer by ID, and customer balances.
 */
@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    /** Repository for customer data. */
    private final CustomerRepository repo;

    /**
     * Constructs a CustomerController with the given repository.
     * @param repo the customer repository
     */
    public CustomerController(CustomerRepository repo) {
        this.repo = repo;
    }

    /**
     * Retrieves all customers.
     * @return a list of all customers
     */
    @GetMapping
    public List<Customer> all() {
        return repo.findAll();
    }

    /**
     * Retrieves a customer by ID.
     * @param id the customer ID
     * @return the customer if found, or 404 if not found
     */
    @GetMapping("/{id}")
    public ResponseEntity<Customer> byId(@PathVariable("id") String id) {
        return repo.findById(id).map(ResponseEntity::ok).orElse(ResponseEntity.notFound().build());
    }

    /**
     * Retrieves the balances for all customers.
     * @return a list of customer balance DTOs
     */
    @GetMapping("/balances")
    public List<CustomerBalanceDto> balances() {
        return repo.findAll().stream()
                .map(c -> new CustomerBalanceDto(
                        c.getCustId(),
                        c.getCustName(),
                        c.getCustBalance() == null ? BigDecimal.ZERO : c.getCustBalance()
                ))
                .collect(Collectors.toList());
    }
}
