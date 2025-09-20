package com.example.customerbilling.service;

import com.example.customerbilling.model.CustomerRecord;
import com.example.customerbilling.util.FileReaderUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;

public class BillingProcessor {

    private static final Logger logger = LoggerFactory.getLogger(BillingProcessor.class);

    private final CustomerRepository customerRepository;

    public BillingProcessor(CustomerRepository customerRepository) {
        this.customerRepository = customerRepository;
    }

    public void processBilling(String billingFilePath) throws IOException {
        List<String> billingRecords = FileReaderUtil.readBillingRecords(billingFilePath);
        for (String record : billingRecords) {
            try {
                processRecord(record);
            } catch (Exception e) {
                logger.error("Failed to process record: {}", record, e);
            }
        }
    }

    private void processRecord(String record) {
        String[] fields = record.split(",");
        int customerId = Integer.parseInt(fields[0].trim());
        BigDecimal amount = new BigDecimal(fields[1].trim());

        CustomerRecord customer = customerRepository.findCustomerById(customerId);
        if (customer != null && ("A".equals(customer.getCustomerStatus()) || customer.getCustomerStatus().isBlank())) {
            customer.setCustomerBalance(customer.getCustomerBalance().add(amount));
            logger.info("Billed customer: {} New balance: {}", customerId, customer.getCustomerBalance());
        } else {
            logger.warn("Customer not found or inactive: {}", customerId);
        }
    }
}