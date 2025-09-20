you are doing a migration activity from cobol to java.
Donot do changes in data input folder files

Key COBOL-to-Java mapping guidance (examples)
- PIC X(n) -> String (trim/length rules)
- PIC 9(n) / signed -> BigInteger/BigDecimal depending on fractional usage
- COMP-3 -> unpacked decimal; require binary/packed handling library
- PERFORM UNTIL -> while loops or stream processing
- UNSTRING DELIMITED BY ',' -> String.split or CSV parser (handle embedded commas/quotes)
- COPYBOOK -> Java POJO with annotated field positions or CSV mapping