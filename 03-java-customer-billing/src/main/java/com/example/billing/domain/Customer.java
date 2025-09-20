package com.example.billing.domain;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.annotation.Id;
import java.math.BigDecimal;


/**
 * Represents a customer entity in the billing system.
 * <p>
 * Maps to the 'customers' collection in MongoDB.
 * <ul>
 *   <li>custId: Customer ID (PIC 9(06))</li>
 *   <li>custName: Customer Name (PIC X(30))</li>
 *   <li>custAddr: Customer Address (PIC X(50))</li>
 *   <li>custBalance: Customer Balance (PIC S9(7)V99 COMP-3 decoded)</li>
 *   <li>custStatus: Customer Status (PIC X)</li>
 * </ul>
 */
@Document(collection = "customers")
public class Customer {
    /** Customer ID (PIC 9(06)) */
    @Id
    private String custId;
    /** Customer Name (PIC X(30)) */
    private String custName;
    /** Customer Address (PIC X(50)) */
    private String custAddr;
    /** Customer Balance (PIC S9(7)V99 COMP-3 decoded) */
    private BigDecimal custBalance;
    /** Customer Status (PIC X) */
    private String custStatus;

    /**
     * Gets the customer ID.
     * @return the customer ID
     */
    public String getCustId() { return custId; }

    /**
     * Sets the customer ID.
     * @param custId the customer ID
     */
    public void setCustId(String custId) { this.custId = custId; }

    /**
     * Gets the customer name.
     * @return the customer name
     */
    public String getCustName() { return custName; }

    /**
     * Sets the customer name. Trims whitespace if not null.
     * @param custName the customer name
     */
    public void setCustName(String custName) { this.custName = custName != null ? custName.trim() : null; }

    /**
     * Gets the customer address.
     * @return the customer address
     */
    public String getCustAddr() { return custAddr; }

    /**
     * Sets the customer address. Trims whitespace if not null.
     * @param custAddr the customer address
     */
    public void setCustAddr(String custAddr) { this.custAddr = custAddr != null ? custAddr.trim() : null; }

    /**
     * Gets the customer balance.
     * @return the customer balance
     */
    public BigDecimal getCustBalance() { return custBalance; }

    /**
     * Sets the customer balance.
     * @param custBalance the customer balance
     */
    public void setCustBalance(BigDecimal custBalance) { this.custBalance = custBalance; }

    /**
     * Gets the customer status.
     * @return the customer status
     */
    public String getCustStatus() { return custStatus; }

    /**
     * Sets the customer status.
     * @param custStatus the customer status
     */
    public void setCustStatus(String custStatus) { this.custStatus = custStatus; }
}
