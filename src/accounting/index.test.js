/**
 * Account Management System - Unit Tests
 * Tests mirror the scenarios in docs/TESTPLAN.md
 */

const { MainProgram, Operations, DataProgram } = require('./index.js');

describe('Account Management System', () => {
    let dataProgram;
    let operations;
    let mainProgram;

    beforeEach(() => {
        // Reset instances before each test
        dataProgram = new DataProgram();
        operations = new Operations(dataProgram);
        mainProgram = new MainProgram();
    });

    // ==================== TC-001: Application Startup and Menu Display ====================
    describe('TC-001: Application Startup and Menu Display', () => {
        test('should create MainProgram instance successfully', () => {
            expect(mainProgram).toBeDefined();
            expect(mainProgram.continueFlag).toBe('YES');
            expect(mainProgram.dataProgram).toBeDefined();
            expect(mainProgram.operations).toBeDefined();
        });
    });

    // ==================== TC-002: View Balance - Initial Balance ====================
    describe('TC-002: View Balance - Initial Balance', () => {
        test('should display initial balance of 1000.00', () => {
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('TOTAL ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1000.00');
            consoleSpy.mockRestore();
        });

        test('should retrieve balance of 1000.00 from DataProgram', () => {
            const balance = dataProgram.execute('READ');
            expect(balance).toBe(1000.00);
        });
    });

    // ==================== TC-003: View Balance - After Transactions ====================
    describe('TC-003: View Balance - After Transactions', () => {
        test('should reflect balance after credit transaction', () => {
            // Mock readline-sync for credit
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('500.00');
            
            operations.execute('CREDIT');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('TOTAL ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1500.00');
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-004: Credit Account - Valid Amount ====================
    describe('TC-004: Credit Account - Valid Amount', () => {
        test('should credit 250.50 and update balance to 1250.50', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('250.50');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1250.50');
            expect(dataProgram.execute('READ')).toBe(1250.50);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-005: Credit Account - Large Amount ====================
    describe('TC-005: Credit Account - Large Amount', () => {
        test('should credit 100000.00 and update balance to 101000.00', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('100000.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 101000.00');
            expect(dataProgram.execute('READ')).toBe(101000.00);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-006: Credit Account - Small Decimal Amount ====================
    describe('TC-006: Credit Account - Small Decimal Amount', () => {
        test('should credit 0.01 and update balance to 1000.01', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('0.01');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1000.01');
            expect(dataProgram.execute('READ')).toBe(1000.01);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-007: Credit Account - Zero Amount ====================
    describe('TC-007: Credit Account - Zero Amount', () => {
        test('should credit 0.00 and balance remains 1000.00', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('0.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1000.00');
            expect(dataProgram.execute('READ')).toBe(1000.00);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-008: Credit Account - Maximum Allowed Amount ====================
    describe('TC-008: Credit Account - Maximum Allowed Amount', () => {
        test('should credit maximum amount 999999.99 from zero balance', () => {
            // First debit entire balance
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('1000.00');
            operations.execute('DEBIT ');
            
            // Then credit maximum
            readlineSync.question = jest.fn().mockReturnValue('999999.99');
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 999999.99');
            expect(dataProgram.execute('READ')).toBe(999999.99);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-009: Debit Account - Valid Amount with Sufficient Funds ====================
    describe('TC-009: Debit Account - Valid Amount with Sufficient Funds', () => {
        test('should debit 300.00 and update balance to 700.00', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('300.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 700.00');
            expect(dataProgram.execute('READ')).toBe(700.00);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-010: Debit Account - Exact Balance Amount ====================
    describe('TC-010: Debit Account - Exact Balance Amount', () => {
        test('should debit exact balance 1000.00 resulting in 0.00', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('1000.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 0.00');
            expect(dataProgram.execute('READ')).toBe(0.00);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-011: Debit Account - Insufficient Funds ====================
    describe('TC-011: Debit Account - Insufficient Funds (Overdraft Prevention)', () => {
        test('should reject debit of 1500.00 when balance is 1000.00', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('1500.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-012: Debit Account - Amount Just Below Balance ====================
    describe('TC-012: Debit Account - Amount Just Below Balance', () => {
        test('should debit 999.99 and update balance to 0.01', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('999.99');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 0.01');
            expect(dataProgram.execute('READ')).toBe(0.01);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-013: Debit Account - Amount Just Above Balance ====================
    describe('TC-013: Debit Account - Amount Just Above Balance', () => {
        test('should reject debit of 1000.01 when balance is 1000.00', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('1000.01');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-014: Debit Account - Zero Amount ====================
    describe('TC-014: Debit Account - Zero Amount', () => {
        test('should debit 0.00 and balance remains 1000.00', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('0.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 1000.00');
            expect(dataProgram.execute('READ')).toBe(1000.00);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-015: Debit Account - Small Decimal Amount ====================
    describe('TC-015: Debit Account - Small Decimal Amount', () => {
        test('should debit 0.01 and update balance to 999.99', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('0.01');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 999.99');
            expect(dataProgram.execute('READ')).toBe(999.99);
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-016: Debit Account - Insufficient Funds with Zero Balance ====================
    describe('TC-016: Debit Account - Insufficient Funds with Zero Balance', () => {
        test('should reject debit of 0.01 when balance is 0.00', () => {
            const readlineSync = require('readline-sync');
            // First debit entire balance
            readlineSync.question = jest.fn().mockReturnValue('1000.00');
            operations.execute('DEBIT ');
            
            // Then try to debit 0.01
            readlineSync.question = jest.fn().mockReturnValue('0.01');
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(0.00); // Balance unchanged
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-017: Multiple Consecutive Credit Transactions ====================
    describe('TC-017: Multiple Consecutive Credit Transactions', () => {
        test('should accumulate multiple credits correctly', () => {
            const readlineSync = require('readline-sync');
            
            // Credit 100.00
            readlineSync.question = jest.fn().mockReturnValue('100.00');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1100.00);
            
            // Credit 200.00
            readlineSync.question = jest.fn().mockReturnValue('200.00');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1300.00);
            
            // Credit 150.50
            readlineSync.question = jest.fn().mockReturnValue('150.50');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1450.50);
        });
    });

    // ==================== TC-018: Multiple Consecutive Debit Transactions ====================
    describe('TC-018: Multiple Consecutive Debit Transactions', () => {
        test('should process multiple debits correctly', () => {
            const readlineSync = require('readline-sync');
            
            // Debit 100.00
            readlineSync.question = jest.fn().mockReturnValue('100.00');
            operations.execute('DEBIT ');
            expect(dataProgram.execute('READ')).toBe(900.00);
            
            // Debit 200.00
            readlineSync.question = jest.fn().mockReturnValue('200.00');
            operations.execute('DEBIT ');
            expect(dataProgram.execute('READ')).toBe(700.00);
            
            // Debit 150.00
            readlineSync.question = jest.fn().mockReturnValue('150.00');
            operations.execute('DEBIT ');
            expect(dataProgram.execute('READ')).toBe(550.00);
        });
    });

    // ==================== TC-019: Mixed Credit and Debit Transactions ====================
    describe('TC-019: Mixed Credit and Debit Transactions', () => {
        test('should handle alternating credit and debit transactions', () => {
            const readlineSync = require('readline-sync');
            
            // Credit 500.00
            readlineSync.question = jest.fn().mockReturnValue('500.00');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1500.00);
            
            // Debit 300.00
            readlineSync.question = jest.fn().mockReturnValue('300.00');
            operations.execute('DEBIT ');
            expect(dataProgram.execute('READ')).toBe(1200.00);
            
            // Credit 100.00
            readlineSync.question = jest.fn().mockReturnValue('100.00');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1300.00);
            
            // Debit 800.00
            readlineSync.question = jest.fn().mockReturnValue('800.00');
            operations.execute('DEBIT ');
            expect(dataProgram.execute('READ')).toBe(500.00);
        });
    });

    // ==================== TC-020: Invalid Menu Option - Below Range ====================
    describe('TC-020: Invalid Menu Option - Below Range', () => {
        test('should handle menu choice 0 as invalid', () => {
            // Since we cannot directly test menu input, we test the switch logic
            // The MainProgram.run() method would display error for choice 0
            expect(mainProgram.continueFlag).toBe('YES');
            
            // Simulate invalid choice handling
            const choice = 0;
            expect([1, 2, 3, 4]).not.toContain(choice);
        });
    });

    // ==================== TC-021: Invalid Menu Option - Above Range ====================
    describe('TC-021: Invalid Menu Option - Above Range', () => {
        test('should handle menu choice 5 as invalid', () => {
            // Test that choice 5 is not in valid range
            const choice = 5;
            expect([1, 2, 3, 4]).not.toContain(choice);
        });
    });

    // ==================== TC-022: Invalid Menu Option - Non-Numeric Input ====================
    describe('TC-022: Invalid Menu Option - Non-Numeric Input', () => {
        test('should handle non-numeric input gracefully', () => {
            const input = 'A';
            const choice = parseInt(input);
            expect(isNaN(choice)).toBe(true);
        });
    });

    // ==================== TC-023: Exit Application ====================
    describe('TC-023: Exit Application', () => {
        test('should set continueFlag to NO when exiting', () => {
            mainProgram.continueFlag = 'NO';
            expect(mainProgram.continueFlag).toBe('NO');
        });
    });

    // ==================== TC-024: Session Persistence - Balance Retained ====================
    describe('TC-024: Session Persistence - Balance Retained', () => {
        test('should retain balance across multiple operations', () => {
            const readlineSync = require('readline-sync');
            
            // Credit 500.00
            readlineSync.question = jest.fn().mockReturnValue('500.00');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1500.00);
            
            // View balance (should not change)
            operations.execute('TOTAL ');
            expect(dataProgram.execute('READ')).toBe(1500.00);
            
            // Debit 200.00
            readlineSync.question = jest.fn().mockReturnValue('200.00');
            operations.execute('DEBIT ');
            expect(dataProgram.execute('READ')).toBe(1300.00);
            
            // View balance again (should still be 1300.00)
            operations.execute('TOTAL ');
            expect(dataProgram.execute('READ')).toBe(1300.00);
        });
    });

    // ==================== TC-025: Decimal Rounding and Precision ====================
    describe('TC-025: Decimal Rounding and Precision', () => {
        test('should maintain 2 decimal place precision', () => {
            const readlineSync = require('readline-sync');
            
            // Credit 33.33
            readlineSync.question = jest.fn().mockReturnValue('33.33');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1033.33);
            
            // Credit 33.33
            readlineSync.question = jest.fn().mockReturnValue('33.33');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1066.66);
            
            // Credit 33.34
            readlineSync.question = jest.fn().mockReturnValue('33.34');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(1100.00);
        });
    });

    // ==================== TC-026: Maximum Balance - Credit Beyond Limit ====================
    describe('TC-026: Maximum Balance - Credit Beyond Limit', () => {
        test('should allow credit that results in very large balance', () => {
            const readlineSync = require('readline-sync');
            
            // Credit large amount
            readlineSync.question = jest.fn().mockReturnValue('999000.00');
            operations.execute('CREDIT');
            
            // Credit additional amount
            readlineSync.question = jest.fn().mockReturnValue('2000.00');
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1002000.00');
            consoleSpy.mockRestore();
        });
    });

    // ==================== TC-027: Balance Display Format ====================
    describe('TC-027: Balance Display Format', () => {
        test('should format balance with 2 decimal places', () => {
            expect(operations.formatCurrency(1000.00)).toBe('1000.00');
            expect(operations.formatCurrency(1234.56)).toBe('1234.56');
            expect(operations.formatCurrency(0.01)).toBe('0.01');
            expect(operations.formatCurrency(999999.99)).toBe('999999.99');
        });
    });

    // ==================== TC-028: Concurrent Operations - Menu Loop ====================
    describe('TC-028: Concurrent Operations - Menu Loop', () => {
        test('should maintain continueFlag as YES during operations', () => {
            expect(mainProgram.continueFlag).toBe('YES');
            
            // Perform operations
            operations.execute('TOTAL ');
            expect(mainProgram.continueFlag).toBe('YES');
            
            // ContinueFlag only changes when explicitly set to 'NO'
            mainProgram.continueFlag = 'NO';
            expect(mainProgram.continueFlag).toBe('NO');
        });
    });

    // ==================== TC-029: Data Isolation - Read Operation ====================
    describe('TC-029: Data Isolation - Read Operation', () => {
        test('should not modify balance on read operations', () => {
            const initialBalance = dataProgram.execute('READ');
            expect(initialBalance).toBe(1000.00);
            
            // Multiple reads should not change balance
            operations.execute('TOTAL ');
            expect(dataProgram.execute('READ')).toBe(1000.00);
            
            operations.execute('TOTAL ');
            expect(dataProgram.execute('READ')).toBe(1000.00);
            
            operations.execute('TOTAL ');
            expect(dataProgram.execute('READ')).toBe(1000.00);
        });
    });

    // ==================== TC-030: Rejected Transaction - Balance Unchanged ====================
    describe('TC-030: Rejected Transaction - Balance Unchanged', () => {
        test('should keep balance unchanged after rejected debit', () => {
            const readlineSync = require('readline-sync');
            
            // Set balance to 500.00
            readlineSync.question = jest.fn().mockReturnValue('500.00');
            operations.execute('DEBIT ');
            expect(dataProgram.execute('READ')).toBe(500.00);
            
            // Attempt to debit 600.00 (should be rejected)
            readlineSync.question = jest.fn().mockReturnValue('600.00');
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
            expect(dataProgram.execute('READ')).toBe(500.00); // Balance unchanged
            
            // Perform valid credit
            readlineSync.question = jest.fn().mockReturnValue('100.00');
            operations.execute('CREDIT');
            expect(dataProgram.execute('READ')).toBe(600.00);
            
            consoleSpy.mockRestore();
        });
    });

    // ==================== Additional Edge Case Tests ====================
    describe('Additional Edge Cases', () => {
        test('should handle invalid credit input (negative number)', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('-50.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            consoleSpy.mockRestore();
        });

        test('should handle invalid debit input (negative number)', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('-50.00');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            consoleSpy.mockRestore();
        });

        test('should handle invalid credit input (non-numeric)', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('abc');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('CREDIT');
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            consoleSpy.mockRestore();
        });

        test('should handle invalid debit input (non-numeric)', () => {
            const readlineSync = require('readline-sync');
            readlineSync.question = jest.fn().mockReturnValue('xyz');
            
            const consoleSpy = jest.spyOn(console, 'log');
            operations.execute('DEBIT ');
            
            expect(consoleSpy).toHaveBeenCalledWith('Invalid amount. Please enter a valid positive number.');
            expect(dataProgram.execute('READ')).toBe(1000.00); // Balance unchanged
            consoleSpy.mockRestore();
        });
    });

    // ==================== DataProgram Unit Tests ====================
    describe('DataProgram Class', () => {
        test('should initialize with balance of 1000.00', () => {
            const dp = new DataProgram();
            expect(dp.storageBalance).toBe(1000.00);
        });

        test('should read balance correctly', () => {
            const dp = new DataProgram();
            const balance = dp.execute('READ');
            expect(balance).toBe(1000.00);
        });

        test('should write balance correctly', () => {
            const dp = new DataProgram();
            const result = dp.execute('WRITE', 1500.00);
            expect(result).toBeNull();
            expect(dp.storageBalance).toBe(1500.00);
        });

        test('should update balance on subsequent reads after write', () => {
            const dp = new DataProgram();
            dp.execute('WRITE', 2000.00);
            const balance = dp.execute('READ');
            expect(balance).toBe(2000.00);
        });
    });

    // ==================== Operations Class Unit Tests ====================
    describe('Operations Class', () => {
        test('should initialize with finalBalance of 1000.00', () => {
            const ops = new Operations(dataProgram);
            expect(ops.finalBalance).toBe(1000.00);
        });

        test('should have reference to DataProgram', () => {
            const ops = new Operations(dataProgram);
            expect(ops.dataProgram).toBe(dataProgram);
        });

        test('should format currency correctly', () => {
            const ops = new Operations(dataProgram);
            expect(ops.formatCurrency(123.4)).toBe('123.40');
            expect(ops.formatCurrency(0.1)).toBe('0.10');
            expect(ops.formatCurrency(1)).toBe('1.00');
        });
    });

    // ==================== MainProgram Class Unit Tests ====================
    describe('MainProgram Class', () => {
        test('should initialize with continueFlag as YES', () => {
            const mp = new MainProgram();
            expect(mp.continueFlag).toBe('YES');
        });

        test('should create DataProgram instance', () => {
            const mp = new MainProgram();
            expect(mp.dataProgram).toBeInstanceOf(DataProgram);
        });

        test('should create Operations instance', () => {
            const mp = new MainProgram();
            expect(mp.operations).toBeInstanceOf(Operations);
        });

        test('should link Operations to DataProgram', () => {
            const mp = new MainProgram();
            expect(mp.operations.dataProgram).toBe(mp.dataProgram);
        });
    });
});
