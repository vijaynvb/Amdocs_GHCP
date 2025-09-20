package com.example.billing.web.dto;

import java.math.BigDecimal;

public class CustomerBalanceDto {
    private Long custId;
    private String custName;
    private BigDecimal balance;

    public CustomerBalanceDto(Long custId, String custName, BigDecimal balance) {
        this.custId = custId;
        this.custName = custName;
        this.balance = balance;
    }

    public Long getCustId() { return custId; }
    public String getCustName() { return custName; }
    public BigDecimal getBalance() { return balance; }
}
