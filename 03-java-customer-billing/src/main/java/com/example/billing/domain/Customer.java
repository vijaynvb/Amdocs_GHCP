package com.example.billing.domain;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.annotation.Id;
import java.math.BigDecimal;

@Document(collection = "customers")
public class Customer {
    @Id
    private String custId;          // PIC 9(06)
    private String custName;      // PIC X(30)
    private String custAddr;      // PIC X(50)
    private BigDecimal custBalance; // PIC S9(7)V99 COMP-3 decoded
    private String custStatus;    // PIC X

    public String getCustId() { return custId; }
    public void setCustId(String custId) { this.custId = custId; }

    public String getCustName() { return custName; }
    public void setCustName(String custName) { this.custName = custName != null ? custName.trim() : null; }

    public String getCustAddr() { return custAddr; }
    public void setCustAddr(String custAddr) { this.custAddr = custAddr != null ? custAddr.trim() : null; }

    public BigDecimal getCustBalance() { return custBalance; }
    public void setCustBalance(BigDecimal custBalance) { this.custBalance = custBalance; }

    public String getCustStatus() { return custStatus; }
    public void setCustStatus(String custStatus) { this.custStatus = custStatus; }
}
