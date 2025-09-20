package com.example.billing.service;

import com.example.billing.domain.Customer;
import com.example.billing.repo.CustomerRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import jakarta.annotation.PostConstruct;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Optional;


/**
 * Loads customer and billing data from files into the database at application startup.
 * <p>
 * This component reads customer and billing information from data files and populates the MongoDB database.
 */
@Component
public class DataLoader {

    /** Repository for customer data. */
    private final CustomerRepository repo;

    /** Resource for the customers data file. */
    @Value("classpath:data/customers.dat")
    private Resource customersFile;

    /** Resource for the billing input data file. */
    @Value("classpath:data/billing-input.dat")
    private Resource billingFile;

    /**
     * Constructs a DataLoader with the given repository.
     * @param repo the customer repository
     */
    public DataLoader(CustomerRepository repo) {
        this.repo = repo;
    }

    /**
     * Initializes the data loader after bean construction.
     * Loads customers and processes billing data.
     * @throws IOException if an I/O error occurs
     */
    @PostConstruct
    public void init() throws IOException {
        System.out.println("Loading customers from: " + customersFile.getURI());
        int customers = loadCustomers();
        System.out.println("Loaded customers: " + customers);
        System.out.println("Processing billing from: " + billingFile.getURI());
        long billed = processBilling();
        System.out.println("Processing complete. Total billed: " + billed);
    }

    /**
     * Loads customer data from the customers file into the database.
     * @return the number of customers loaded
     * @throws IOException if an I/O error occurs
     */
    int loadCustomers() throws IOException {
        int count = 0;
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(customersFile.getInputStream(), StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) continue;
                Customer c = new Customer();
                String idStr = line.substring(0, 6).trim();
                c.setCustId(idStr);
                c.setCustName(line.substring(6, 36));
                c.setCustAddr(line.substring(36, 86));
                c.setCustStatus(line.length() > 91 ? line.substring(91, 92) : "A");
                BigDecimal balance = BigDecimal.ZERO;
                if (line.length() >= 6) {
                    String balCents = line.substring(line.length() - 6);
                    try { balance = new BigDecimal(balCents).movePointLeft(2); } catch (NumberFormatException ignored) {}
                }
                c.setCustBalance(balance);
                repo.save(c);
                count++;
            }
        }
        return count;
    }

    /**
     * Processes billing data from the billing file and updates customer balances.
     * @return the number of customers billed
     * @throws IOException if an I/O error occurs
     */
    long processBilling() throws IOException {
        long totalBilled = 0;
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(billingFile.getInputStream(), StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) continue;
                String[] parts = line.split(",");
                if (parts.length < 2) continue;
                String idStr = parts[0].trim();
                String amtStr = parts[1].trim();
                if (idStr.isEmpty()) continue;
                BigDecimal amt;
                try {
                    amt = new BigDecimal(amtStr);
                } catch (NumberFormatException e) {
                    // Log or skip invalid input
                    continue;
                }
                Optional<Customer> optCustomer = repo.findById(idStr);
                if (optCustomer.isPresent()) {
                    Customer customer = optCustomer.get();
                    String status = customer.getCustStatus();
                    System.out.println("Customer found: ID=" + customer.getCustId() + ", Status='" + status + "', Balance=" + customer.getCustBalance());
                    if (status == null || status.isBlank() || status.equals("A")) {
                        BigDecimal newBalance = customer.getCustBalance() == null ? amt : customer.getCustBalance().add(amt);
                        System.out.println("Updating balance for customer ID " + customer.getCustId() + ": Old Balance=" + customer.getCustBalance() + ", Amount=" + amt + ", New Balance=" + newBalance);
                        customer.setCustBalance(newBalance);
                        repo.save(customer);
                        System.out.println("Saved balance for customer ID " + customer.getCustId() + ": " + customer.getCustBalance());
                        totalBilled++;
                    }
                }
            }
        }
        return totalBilled;
    }
}
