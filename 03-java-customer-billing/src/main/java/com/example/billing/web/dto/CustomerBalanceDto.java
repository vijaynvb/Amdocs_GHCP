package com.example.billing.web.dto;

import java.math.BigDecimal;


/**
 * Data Transfer Object (DTO) for exposing customer balance information via the API.
 */
public class CustomerBalanceDto {
    /** Customer ID. */
    private String custId;
    /** Customer name. */
    private String custName;
    /** Customer balance. */
    private BigDecimal balance;

    /**
     * Constructs a new CustomerBalanceDto.
     * @param custId the customer ID
     * @param custName the customer name
     * @param balance the customer balance
     */
    public CustomerBalanceDto(String custId, String custName, BigDecimal balance) {
        this.custId = custId;
        this.custName = custName;
        this.balance = balance;
    }

    /**
     * Gets the customer ID.
     * @return the customer ID
     */
    public String getCustId() { return custId; }

    /**
     * Gets the customer name.
     * @return the customer name
     */
    public String getCustName() { return custName; }

    /**
     * Gets the customer balance.
     * @return the customer balance
     */
    public BigDecimal getBalance() { return balance; }
}
