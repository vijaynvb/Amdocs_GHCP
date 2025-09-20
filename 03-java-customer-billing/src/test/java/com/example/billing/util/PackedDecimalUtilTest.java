package com.example.billing.util;

import org.junit.jupiter.api.Test;
import java.math.BigDecimal;
import static org.junit.jupiter.api.Assertions.*;

class PackedDecimalUtilTest {
    @Test
    void decodePositive() {
        // 12345 with scale 2 => 123.45; packed bytes: 0x12 0x34 0x5C
        byte[] packed = new byte[]{0x12, 0x34, 0x5C};
        assertEquals(new BigDecimal("123.45"), PackedDecimalUtil.decode(packed, 2));
    }

    @Test
    void decodeNegative() {
    // 9876 with scale 2 => -98.76; packed bytes: 0x09 0x87 0x6D (where 0xD is the negative sign nibble)
    byte[] packed = new byte[]{(byte)0x09, (byte)0x87, (byte)0x6D};
    assertEquals(new BigDecimal("-98.76"), PackedDecimalUtil.decode(packed, 2));
    }
}
