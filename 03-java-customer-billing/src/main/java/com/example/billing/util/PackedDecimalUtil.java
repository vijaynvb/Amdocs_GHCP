package com.example.billing.util;

import java.math.BigDecimal;

public final class PackedDecimalUtil {
    private PackedDecimalUtil() {}

    // Decode IBM/COBOL COMP-3 packed decimal to BigDecimal with scale
    public static BigDecimal decode(byte[] packed, int scale) {
        if (packed == null || packed.length == 0) return BigDecimal.ZERO;
        StringBuilder digits = new StringBuilder(packed.length * 2);
        int sign = 1;
        for (int i = 0; i < packed.length; i++) {
            int b = packed[i] & 0xFF;
            int high = (b >> 4) & 0x0F;
            int low = b & 0x0F;
            if (i == packed.length - 1) {
                // last nibble is sign
                switch (low) {
                    case 0x0D: // negative
                    case 0x0B:
                        sign = -1; break;
                    default: // 0x0C, 0x0F positive
                        sign = 1; break;
                }
                digits.append(high);
            } else {
                digits.append(high).append(low);
            }
        }
        if (digits.length() == 0) return BigDecimal.ZERO;
        BigDecimal unscaled = new BigDecimal(digits.toString());
        BigDecimal scaled = unscaled.movePointLeft(scale);
        return sign < 0 ? scaled.negate() : scaled;
    }
}
