package com.example.billing.web.dto;

import java.math.BigDecimal;

public class CustomerBalanceDto {
    private String custId;
    private String custName;
    private BigDecimal balance;

    public CustomerBalanceDto(String custId, String custName, BigDecimal balance) {
        this.custId = custId;
        this.custName = custName;
        this.balance = balance;
    }

    public String getCustId() { return custId; }
    public String getCustName() { return custName; }
    public BigDecimal getBalance() { return balance; }
}
