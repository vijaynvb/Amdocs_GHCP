package com.example.customerbilling.service;

import com.example.customerbilling.model.CustomerRecord;
import com.example.customerbilling.util.FileReaderUtil;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CustomerRepository {

    private final Map<Integer, CustomerRecord> customerMap = new HashMap<>();

    public void loadCustomers(String filePath) throws IOException {
        List<CustomerRecord> customers = FileReaderUtil.readCustomers(filePath);
        for (CustomerRecord customer : customers) {
            customerMap.put(customer.getCustomerId(), customer);
        }
    }

    public CustomerRecord findCustomerById(int customerId) {
        return customerMap.get(customerId);
    }
}