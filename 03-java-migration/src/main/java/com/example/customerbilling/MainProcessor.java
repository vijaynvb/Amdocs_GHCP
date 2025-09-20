package com.example.customerbilling;

import com.example.customerbilling.service.BillingProcessor;
import com.example.customerbilling.service.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MainProcessor {

    private static final Logger logger = LoggerFactory.getLogger(MainProcessor.class);

    public static void main(String[] args) {
        if (args.length < 2) {
            logger.error("Usage: java MainProcessor <customersFilePath> <billingFilePath>");
            return;
        }

        String customersFilePath = args[0];
        String billingFilePath = args[1];

        CustomerRepository customerRepository = new CustomerRepository();
        BillingProcessor billingProcessor = new BillingProcessor(customerRepository);

        try {
            logger.info("Starting processing...");

            // Load customers
            customerRepository.loadCustomers(customersFilePath);
            logger.info("Customers loaded successfully.");

            // Process billing
            billingProcessor.processBilling(billingFilePath);
            logger.info("Billing processed successfully.");

        } catch (Exception e) {
            logger.error("An error occurred: {}", e.getMessage(), e);
        }
    }
}