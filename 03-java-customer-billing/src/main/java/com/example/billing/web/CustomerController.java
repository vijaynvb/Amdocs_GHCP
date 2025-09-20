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

@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    private final CustomerRepository repo;

    public CustomerController(CustomerRepository repo) {
        this.repo = repo;
    }

    @GetMapping
    public List<Customer> all() {
        return repo.findAll();
    }

    @GetMapping("/{id}")
    public ResponseEntity<Customer> byId(@PathVariable("id") long id) {
        return repo.findById(id).map(ResponseEntity::ok).orElse(ResponseEntity.notFound().build());
    }

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
