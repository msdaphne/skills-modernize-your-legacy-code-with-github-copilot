# Account Management System - Test Plan

## Overview

This test plan covers all business logic and functionality of the COBOL Account Management System. It can be used to validate the current implementation with business stakeholders and will serve as the foundation for creating unit and integration tests in the Node.js migration.

## Test Environment

- **Initial Balance**: 1000.00
- **Maximum Balance**: 999,999.99
- **Maximum Transaction Amount**: 999,999.99
- **Decimal Precision**: 2 decimal places

---

## Test Cases

### 1. Application Startup and Menu Display

| Test Case ID | TC-001 |
|--------------|--------|
| **Test Case Description** | Verify that the application starts successfully and displays the main menu |
| **Pre-conditions** | Application is compiled and ready to run |
| **Test Steps** | 1. Start the application |
| **Expected Result** | Application displays menu with options:<br/>1. View Balance<br/>2. Credit Account<br/>3. Debit Account<br/>4. Exit |
| **Actual Result** | |
| **Status** | |
| **Comments** | |

---

### 2. View Balance - Initial Balance

| Test Case ID | TC-002 |
|--------------|--------|
| **Test Case Description** | Verify that the initial account balance is displayed correctly |
| **Pre-conditions** | Application is running with default initial balance |
| **Test Steps** | 1. Select option 1 (View Balance)<br/>2. Observe displayed balance |
| **Expected Result** | Display shows: "Current balance: 1000.00" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates default balance initialization |

---

### 3. View Balance - After Transactions

| Test Case ID | TC-003 |
|--------------|--------|
| **Test Case Description** | Verify that the balance reflects previous transactions correctly |
| **Pre-conditions** | 1. Application is running<br/>2. Previous transactions have been performed (e.g., credit of 500.00) |
| **Test Steps** | 1. Perform a credit transaction of 500.00<br/>2. Select option 1 (View Balance)<br/>3. Observe displayed balance |
| **Expected Result** | Display shows: "Current balance: 1500.00" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates balance persistence within session |

---

### 4. Credit Account - Valid Amount

| Test Case ID | TC-004 |
|--------------|--------|
| **Test Case Description** | Verify that a valid credit transaction updates the balance correctly |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 250.50<br/>3. Observe confirmation message |
| **Expected Result** | Display shows: "Amount credited. New balance: 1250.50" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates basic credit functionality |

---

### 5. Credit Account - Large Amount

| Test Case ID | TC-005 |
|--------------|--------|
| **Test Case Description** | Verify that a large credit amount (within limits) is processed correctly |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 100000.00<br/>3. Observe confirmation message |
| **Expected Result** | Display shows: "Amount credited. New balance: 101000.00" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates handling of large transaction amounts |

---

### 6. Credit Account - Small Decimal Amount

| Test Case ID | TC-006 |
|--------------|--------|
| **Test Case Description** | Verify that small decimal amounts are handled with correct precision |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 0.01<br/>3. Observe confirmation message |
| **Expected Result** | Display shows: "Amount credited. New balance: 1000.01" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates decimal precision handling |

---

### 7. Credit Account - Zero Amount

| Test Case ID | TC-007 |
|--------------|--------|
| **Test Case Description** | Verify system behavior when crediting zero amount |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 0.00<br/>3. Observe system response |
| **Expected Result** | Display shows: "Amount credited. New balance: 1000.00" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Edge case - zero transaction |

---

### 8. Credit Account - Maximum Allowed Amount

| Test Case ID | TC-008 |
|--------------|--------|
| **Test Case Description** | Verify that crediting the maximum allowed amount works correctly |
| **Pre-conditions** | Application is running with balance of 0.00 |
| **Test Steps** | 1. Debit entire balance to reach 0.00<br/>2. Select option 2 (Credit Account)<br/>3. Enter amount: 999999.99<br/>4. Observe confirmation message |
| **Expected Result** | Display shows: "Amount credited. New balance: 999999.99" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates maximum balance limit |

---

### 9. Debit Account - Valid Amount with Sufficient Funds

| Test Case ID | TC-009 |
|--------------|--------|
| **Test Case Description** | Verify that a valid debit transaction with sufficient funds updates the balance correctly |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 300.00<br/>3. Observe confirmation message |
| **Expected Result** | Display shows: "Amount debited. New balance: 700.00" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates basic debit functionality |

---

### 10. Debit Account - Exact Balance Amount

| Test Case ID | TC-010 |
|--------------|--------|
| **Test Case Description** | Verify that debiting the exact balance amount results in zero balance |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 1000.00<br/>3. Observe confirmation message |
| **Expected Result** | Display shows: "Amount debited. New balance: 0.00" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Edge case - complete balance withdrawal |

---

### 11. Debit Account - Insufficient Funds (Overdraft Prevention)

| Test Case ID | TC-011 |
|--------------|--------|
| **Test Case Description** | Verify that debit transaction is rejected when amount exceeds balance |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 1500.00<br/>3. Observe error message<br/>4. Select option 1 to verify balance unchanged |
| **Expected Result** | 1. Display shows: "Insufficient funds for this debit."<br/>2. Balance remains: 1000.00 |
| **Actual Result** | |
| **Status** | |
| **Comments** | Critical business rule - no overdraft allowed |

---

### 12. Debit Account - Amount Just Below Balance

| Test Case ID | TC-012 |
|--------------|--------|
| **Test Case Description** | Verify that debit transaction works when amount is just below balance |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 999.99<br/>3. Observe confirmation message |
| **Expected Result** | Display shows: "Amount debited. New balance: 0.01" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Boundary condition testing |

---

### 13. Debit Account - Amount Just Above Balance

| Test Case ID | TC-013 |
|--------------|--------|
| **Test Case Description** | Verify that debit transaction is rejected when amount is just above balance |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 1000.01<br/>3. Observe error message |
| **Expected Result** | Display shows: "Insufficient funds for this debit."<br/>Balance remains: 1000.00 |
| **Actual Result** | |
| **Status** | |
| **Comments** | Boundary condition testing |

---

### 14. Debit Account - Zero Amount

| Test Case ID | TC-014 |
|--------------|--------|
| **Test Case Description** | Verify system behavior when debiting zero amount |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 0.00<br/>3. Observe system response |
| **Expected Result** | Display shows: "Amount debited. New balance: 1000.00" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Edge case - zero transaction |

---

### 15. Debit Account - Small Decimal Amount

| Test Case ID | TC-015 |
|--------------|--------|
| **Test Case Description** | Verify that small decimal debit amounts are handled with correct precision |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 0.01<br/>3. Observe confirmation message |
| **Expected Result** | Display shows: "Amount debited. New balance: 999.99" |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates decimal precision handling |

---

### 16. Debit Account - Insufficient Funds with Zero Balance

| Test Case ID | TC-016 |
|--------------|--------|
| **Test Case Description** | Verify that debit is rejected when balance is zero |
| **Pre-conditions** | Application is running with balance of 0.00 (debited entire initial balance) |
| **Test Steps** | 1. Debit entire balance to reach 0.00<br/>2. Select option 3 (Debit Account)<br/>3. Enter amount: 0.01<br/>4. Observe error message |
| **Expected Result** | Display shows: "Insufficient funds for this debit."<br/>Balance remains: 0.00 |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates no negative balance allowed |

---

### 17. Multiple Consecutive Credit Transactions

| Test Case ID | TC-017 |
|--------------|--------|
| **Test Case Description** | Verify that multiple credit transactions accumulate correctly |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Credit 100.00 (balance: 1100.00)<br/>2. Credit 200.00 (balance: 1300.00)<br/>3. Credit 150.50 (balance: 1450.50)<br/>4. View balance to confirm |
| **Expected Result** | Final balance displays: 1450.50 |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates transaction accumulation |

---

### 18. Multiple Consecutive Debit Transactions

| Test Case ID | TC-018 |
|--------------|--------|
| **Test Case Description** | Verify that multiple debit transactions reduce balance correctly |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Debit 100.00 (balance: 900.00)<br/>2. Debit 200.00 (balance: 700.00)<br/>3. Debit 150.00 (balance: 550.00)<br/>4. View balance to confirm |
| **Expected Result** | Final balance displays: 550.00 |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates sequential debits |

---

### 19. Mixed Credit and Debit Transactions

| Test Case ID | TC-019 |
|--------------|--------|
| **Test Case Description** | Verify that alternating credit and debit transactions calculate correctly |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Credit 500.00 (balance: 1500.00)<br/>2. Debit 300.00 (balance: 1200.00)<br/>3. Credit 100.00 (balance: 1300.00)<br/>4. Debit 800.00 (balance: 500.00)<br/>5. View balance to confirm |
| **Expected Result** | Final balance displays: 500.00 |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates real-world transaction patterns |

---

### 20. Invalid Menu Option - Below Range

| Test Case ID | TC-020 |
|--------------|--------|
| **Test Case Description** | Verify that entering a menu option below valid range shows error |
| **Pre-conditions** | Application is running and displaying menu |
| **Test Steps** | 1. Enter choice: 0<br/>2. Observe error message<br/>3. Verify menu redisplays |
| **Expected Result** | Display shows: "Invalid choice, please select 1-4."<br/>Menu redisplays |
| **Actual Result** | |
| **Status** | |
| **Comments** | Input validation testing |

---

### 21. Invalid Menu Option - Above Range

| Test Case ID | TC-021 |
|--------------|--------|
| **Test Case Description** | Verify that entering a menu option above valid range shows error |
| **Pre-conditions** | Application is running and displaying menu |
| **Test Steps** | 1. Enter choice: 5<br/>2. Observe error message<br/>3. Verify menu redisplays |
| **Expected Result** | Display shows: "Invalid choice, please select 1-4."<br/>Menu redisplays |
| **Actual Result** | |
| **Status** | |
| **Comments** | Input validation testing |

---

### 22. Invalid Menu Option - Non-Numeric Input

| Test Case ID | TC-022 |
|--------------|--------|
| **Test Case Description** | Verify system behavior when non-numeric input is entered for menu |
| **Pre-conditions** | Application is running and displaying menu |
| **Test Steps** | 1. Enter choice: 'A' or other non-numeric character<br/>2. Observe system response |
| **Expected Result** | System handles gracefully, either shows error or treats as invalid (displays invalid choice message) |
| **Actual Result** | |
| **Status** | |
| **Comments** | Edge case - data type validation |

---

### 23. Exit Application

| Test Case ID | TC-023 |
|--------------|--------|
| **Test Case Description** | Verify that selecting exit option terminates the application properly |
| **Pre-conditions** | Application is running and displaying menu |
| **Test Steps** | 1. Select option 4 (Exit)<br/>2. Observe exit message<br/>3. Verify application terminates |
| **Expected Result** | Display shows: "Exiting the program. Goodbye!"<br/>Application terminates cleanly |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates clean shutdown |

---

### 24. Session Persistence - Balance Retained

| Test Case ID | TC-024 |
|--------------|--------|
| **Test Case Description** | Verify that balance is retained throughout the session across multiple operations |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Credit 500.00<br/>2. View balance<br/>3. Return to menu<br/>4. Debit 200.00<br/>5. View balance<br/>6. Return to menu<br/>7. View balance again |
| **Expected Result** | All balance views show consistent value: 1300.00 after step 2, 1100.00 after steps 5 and 7 |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates data persistence within session |

---

### 25. Decimal Rounding and Precision

| Test Case ID | TC-025 |
|--------------|--------|
| **Test Case Description** | Verify that all calculations maintain 2 decimal place precision |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. Credit 33.33<br/>2. Credit 33.33<br/>3. Credit 33.34<br/>4. View balance |
| **Expected Result** | Balance displays: 1100.00 (correct decimal handling) |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates financial precision requirements |

---

### 26. Maximum Balance - Credit Beyond Limit

| Test Case ID | TC-026 |
|--------------|--------|
| **Test Case Description** | Verify system behavior when attempting to credit beyond maximum balance |
| **Pre-conditions** | Application is running with balance of 999000.00 |
| **Test Steps** | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 2000.00<br/>3. Observe system response |
| **Expected Result** | System either:<br/>- Accepts transaction and displays balance (if overflow allowed), or<br/>- Rejects with appropriate error message (if limit enforced) |
| **Actual Result** | |
| **Status** | |
| **Comments** | Business decision needed on overflow handling |

---

### 27. Balance Display Format

| Test Case ID | TC-027 |
|--------------|--------|
| **Test Case Description** | Verify that balance is displayed in correct monetary format |
| **Pre-conditions** | Application is running |
| **Test Steps** | 1. View initial balance<br/>2. Perform transactions with various amounts<br/>3. Check balance display format |
| **Expected Result** | All balances display with 2 decimal places (e.g., 1000.00, 1234.56, 0.01) |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates output formatting |

---

### 28. Concurrent Operations - Menu Loop

| Test Case ID | TC-028 |
|--------------|--------|
| **Test Case Description** | Verify that menu redisplays correctly after each operation until exit |
| **Pre-conditions** | Application is running |
| **Test Steps** | 1. Perform view balance operation<br/>2. Verify menu redisplays<br/>3. Perform credit operation<br/>4. Verify menu redisplays<br/>5. Perform debit operation<br/>6. Verify menu redisplays |
| **Expected Result** | Menu redisplays correctly after each operation with all 4 options visible |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates program control flow |

---

### 29. Data Isolation - Read Operation

| Test Case ID | TC-029 |
|--------------|--------|
| **Test Case Description** | Verify that view balance operation does not modify the balance |
| **Pre-conditions** | Application is running with balance of 1000.00 |
| **Test Steps** | 1. View balance (displays 1000.00)<br/>2. View balance again (displays 1000.00)<br/>3. View balance third time (displays 1000.00)<br/>4. Credit 100.00<br/>5. View balance (displays 1100.00) |
| **Expected Result** | Balance remains unchanged by read operations, only changes after credit |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates read-only operations don't mutate data |

---

### 30. Rejected Transaction - Balance Unchanged

| Test Case ID | TC-030 |
|--------------|--------|
| **Test Case Description** | Verify that when a debit is rejected, the balance remains completely unchanged |
| **Pre-conditions** | Application is running with balance of 500.00 |
| **Test Steps** | 1. Note current balance: 500.00<br/>2. Attempt to debit 600.00<br/>3. Observe rejection message<br/>4. View balance<br/>5. Perform valid credit of 100.00<br/>6. View balance |
| **Expected Result** | Balance after rejection: 500.00<br/>Balance after credit: 600.00<br/>(Rejection had no effect) |
| **Actual Result** | |
| **Status** | |
| **Comments** | Validates transaction atomicity |

---

## Test Execution Summary

| **Total Test Cases** | 30 |
|---------------------|-----|
| **Passed** | |
| **Failed** | |
| **Blocked** | |
| **Not Executed** | |

---

## Test Coverage Summary

### Functional Areas Covered

| Functional Area | Test Cases | Coverage |
|----------------|------------|----------|
| **Menu Display & Navigation** | TC-001, TC-020, TC-021, TC-022, TC-023, TC-028 | 6 test cases |
| **View Balance** | TC-002, TC-003, TC-027, TC-029 | 4 test cases |
| **Credit Transactions** | TC-004, TC-005, TC-006, TC-007, TC-008, TC-017, TC-019 | 7 test cases |
| **Debit Transactions** | TC-009, TC-010, TC-011, TC-012, TC-013, TC-014, TC-015, TC-016, TC-018, TC-019 | 10 test cases |
| **Business Rules** | TC-011, TC-013, TC-016, TC-026, TC-030 | 5 test cases |
| **Data Precision** | TC-006, TC-015, TC-025, TC-027 | 4 test cases |
| **Session Management** | TC-024, TC-029 | 2 test cases |

### Business Rules Validated

1. ✓ Initial balance of 1000.00
2. ✓ No overdraft/negative balance allowed
3. ✓ Balance must be between 0.00 and 999,999.99
4. ✓ Transaction amounts up to 999,999.99
5. ✓ Two decimal place precision
6. ✓ Insufficient funds rejection
7. ✓ Balance persistence within session
8. ✓ Read operations don't modify balance
9. ✓ Atomic transaction processing

---

## Notes for Node.js Migration

### Recommended Test Framework
- **Unit Tests**: Jest or Mocha
- **Integration Tests**: Supertest (for API testing) or Cypress (for UI testing)
- **Test Coverage**: Istanbul/NYC

### Test Implementation Priorities

**High Priority** (Critical Business Logic):
- TC-011: Insufficient funds validation
- TC-009, TC-010: Basic debit operations
- TC-004: Basic credit operation
- TC-002: Initial balance
- TC-030: Transaction atomicity

**Medium Priority** (Edge Cases):
- TC-012, TC-013: Boundary conditions
- TC-025: Decimal precision
- TC-017, TC-018, TC-019: Multiple transactions
- TC-016: Zero balance scenarios

**Low Priority** (User Interface):
- TC-020, TC-021, TC-022: Input validation
- TC-001, TC-023, TC-028: Menu operations

### Additional Test Considerations for Node.js

1. **Async Operations**: Test asynchronous database/storage operations
2. **Concurrency**: Test simultaneous transaction attempts
3. **API Endpoints**: Test REST API request/response handling
4. **Error Handling**: Test comprehensive error scenarios
5. **Data Persistence**: Test database transactions and rollbacks
6. **Security**: Test authentication and authorization
7. **Performance**: Load testing for concurrent users
8. **Input Sanitization**: Test for injection attacks and malformed data

---

## Approval

| Role | Name | Signature | Date |
|------|------|-----------|------|
| **Business Stakeholder** | | | |
| **QA Lead** | | | |
| **Development Lead** | | | |
| **Product Owner** | | | |

---

## Revision History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2025-12-02 | | Initial test plan created from COBOL application analysis |

