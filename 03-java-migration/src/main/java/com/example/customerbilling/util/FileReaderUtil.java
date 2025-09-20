package com.example.customerbilling.util;

import com.example.customerbilling.model.CustomerRecord;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class FileReaderUtil {

    public static List<CustomerRecord> readCustomers(String filePath) throws IOException {
        List<CustomerRecord> customers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) continue;
                CustomerRecord customer = parseCustomer(line);
                customers.add(customer);
            }
        }
        return customers;
    }

    private static CustomerRecord parseCustomer(String line) {
        CustomerRecord customer = new CustomerRecord();
        customer.setCustomerId(Integer.parseInt(line.substring(0, 6).trim()));
        customer.setCustomerName(line.substring(6, 36).trim());
        customer.setCustomerAddress(line.substring(36, 86).trim());
        customer.setCustomerBalance(new BigDecimal(line.substring(86, 95).trim()));
        customer.setCustomerStatus(line.substring(95).trim());
        return customer;
    }

    public static List<String> readBillingRecords(String filePath) throws IOException {
        List<String> billingRecords = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) continue;
                billingRecords.add(line);
            }
        }
        return billingRecords;
    }
}