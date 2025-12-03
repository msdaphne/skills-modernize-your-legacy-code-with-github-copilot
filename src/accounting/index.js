/**
 * Account Management System - Node.js Implementation
 * Converted from COBOL legacy application
 * 
 * This application preserves the original three-tier architecture:
 * - MainProgram: User interface and menu system
 * - Operations: Business logic layer
 * - DataProgram: Data access layer
 */

const readline = require('readline-sync');

/**
 * DataProgram - Data Access Layer
 * Manages account balance storage and persistence
 * Equivalent to data.cob (DataProgram)
 */
class DataProgram {
    constructor() {
        // STORAGE-BALANCE: PIC 9(6)V99 VALUE 1000.00
        this.storageBalance = 1000.00;
    }

    /**
     * Performs READ or WRITE operations on the balance
     * @param {string} operation - 'READ' or 'WRITE'
     * @param {number|null} balance - Balance value for WRITE operation
     * @returns {number|null} Balance for READ operation, null for WRITE
     */
    execute(operation, balance = null) {
        if (operation === 'READ') {
            // MOVE STORAGE-BALANCE TO BALANCE
            return this.storageBalance;
        } else if (operation === 'WRITE') {
            // MOVE BALANCE TO STORAGE-BALANCE
            this.storageBalance = balance;
            return null;
        }
    }
}

/**
 * Operations - Business Logic Layer
 * Handles all account operations: view balance, credit, and debit
 * Equivalent to operations.cob (Operations)
 */
class Operations {
    constructor(dataProgram) {
        this.dataProgram = dataProgram;
        // FINAL-BALANCE: PIC 9(6)V99 VALUE 1000.00
        this.finalBalance = 1000.00;
    }

    /**
     * Executes the specified operation
     * @param {string} operationType - 'TOTAL ', 'CREDIT', or 'DEBIT '
     */
    execute(operationType) {
        if (operationType === 'TOTAL ') {
            this.viewBalance();
        } else if (operationType === 'CREDIT') {
            this.creditAccount();
        } else if (operationType === 'DEBIT ') {
            this.debitAccount();
        }
    }

    /**
     * View Balance Operation
     * Retrieves and displays current balance
     */
    viewBalance() {
        // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        this.finalBalance = this.dataProgram.execute('READ');
        // DISPLAY "Current balance: " FINAL-BALANCE
        console.log(`Current balance: ${this.formatCurrency(this.finalBalance)}`);
    }

    /**
     * Credit Account Operation
     * Prompts for amount, adds to balance, and persists
     */
    creditAccount() {
        // DISPLAY "Enter credit amount: "
        const input = readline.question('Enter credit amount: ');
        // ACCEPT AMOUNT
        const amount = parseFloat(input);

        // Validate input
        if (isNaN(amount) || amount < 0) {
            console.log('Invalid amount. Please enter a valid positive number.');
            return;
        }

        // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        this.finalBalance = this.dataProgram.execute('READ');
        
        // ADD AMOUNT TO FINAL-BALANCE
        this.finalBalance += amount;
        
        // Ensure 2 decimal places precision
        this.finalBalance = Math.round(this.finalBalance * 100) / 100;
        
        // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        this.dataProgram.execute('WRITE', this.finalBalance);
        
        // DISPLAY "Amount credited. New balance: " FINAL-BALANCE
        console.log(`Amount credited. New balance: ${this.formatCurrency(this.finalBalance)}`);
    }

    /**
     * Debit Account Operation
     * Prompts for amount, validates sufficient funds, subtracts from balance
     */
    debitAccount() {
        // DISPLAY "Enter debit amount: "
        const input = readline.question('Enter debit amount: ');
        // ACCEPT AMOUNT
        const amount = parseFloat(input);

        // Validate input
        if (isNaN(amount) || amount < 0) {
            console.log('Invalid amount. Please enter a valid positive number.');
            return;
        }

        // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        this.finalBalance = this.dataProgram.execute('READ');
        
        // IF FINAL-BALANCE >= AMOUNT
        if (this.finalBalance >= amount) {
            // SUBTRACT AMOUNT FROM FINAL-BALANCE
            this.finalBalance -= amount;
            
            // Ensure 2 decimal places precision
            this.finalBalance = Math.round(this.finalBalance * 100) / 100;
            
            // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
            this.dataProgram.execute('WRITE', this.finalBalance);
            
            // DISPLAY "Amount debited. New balance: " FINAL-BALANCE
            console.log(`Amount debited. New balance: ${this.formatCurrency(this.finalBalance)}`);
        } else {
            // DISPLAY "Insufficient funds for this debit."
            console.log('Insufficient funds for this debit.');
        }
    }

    /**
     * Formats number as currency with 2 decimal places
     * Ensures PIC 9(6)V99 format compliance
     * @param {number} value - The value to format
     * @returns {string} Formatted currency string
     */
    formatCurrency(value) {
        return value.toFixed(2);
    }
}

/**
 * MainProgram - User Interface Controller
 * Displays menu, accepts user input, and routes to operations
 * Equivalent to main.cob (MainProgram)
 */
class MainProgram {
    constructor() {
        // Initialize data layer
        this.dataProgram = new DataProgram();
        // Initialize business logic layer
        this.operations = new Operations(this.dataProgram);
        // CONTINUE-FLAG: PIC X(3) VALUE 'YES'
        this.continueFlag = 'YES';
    }

    /**
     * Main program loop
     * Displays menu and processes user choices until exit
     */
    run() {
        // PERFORM UNTIL CONTINUE-FLAG = 'NO'
        while (this.continueFlag === 'YES') {
            this.displayMenu();
            
            // ACCEPT USER-CHOICE
            const userChoice = readline.question('Enter your choice (1-4): ');
            
            // Parse as integer
            const choice = parseInt(userChoice);
            
            console.log(); // Blank line for readability
            
            // EVALUATE USER-CHOICE
            switch (choice) {
                case 1:
                    // CALL 'Operations' USING 'TOTAL '
                    this.operations.execute('TOTAL ');
                    break;
                case 2:
                    // CALL 'Operations' USING 'CREDIT'
                    this.operations.execute('CREDIT');
                    break;
                case 3:
                    // CALL 'Operations' USING 'DEBIT '
                    this.operations.execute('DEBIT ');
                    break;
                case 4:
                    // MOVE 'NO' TO CONTINUE-FLAG
                    this.continueFlag = 'NO';
                    break;
                default:
                    // DISPLAY "Invalid choice, please select 1-4."
                    console.log('Invalid choice, please select 1-4.');
            }
            
            console.log(); // Blank line for readability
        }
        
        // DISPLAY "Exiting the program. Goodbye!"
        console.log('Exiting the program. Goodbye!');
        // STOP RUN
    }

    /**
     * Displays the main menu
     */
    displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
    }
}

/**
 * Application Entry Point
 */
function main() {
    const app = new MainProgram();
    app.run();
}

// Run the application if this is the main module
if (require.main === module) {
    main();
}

// Export for testing
module.exports = { MainProgram, Operations, DataProgram };
