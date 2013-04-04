package com.raytheon.uf.edex.database.dao;

import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Utility class for debugging Spring transactions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public final class SpringTransactionUtils {

    /**
     * Set this property to "true" in your Run Configuration to enable Spring
     * transaction debugging.
     */
    public static final String SPRING_TRANSACTION_DEBUGGING_FLAG = "spring.transaction.debugging";

    private static final boolean transactionDebugging = Boolean
            .getBoolean(SPRING_TRANSACTION_DEBUGGING_FLAG);

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SpringTransactionUtils.class);

    /**
     * Log the status of a Spring transaction.
     * 
     * @param message
     *            the unique message identifying the location in code
     */
    public static void showTransactionStatus(String message) {
        if (!transactionDebugging) {
            return;
        }

        statusHandler.info(((isTransactionActive()) ? "[+] " : "[-] ") + message);
    }

    /**
     * Verify a Spring transaction is active.
     * 
     * @param message
     *            the unique message identifying the location in code
     * @throws IllegalStateException
     *             if a transaction is not active
     */
    public static void transactionRequired(String message)
            throws IllegalStateException {
        if (!transactionDebugging) {
            return;
        }

        showTransactionStatus(message);

        if (!isTransactionActive()) {
            throw new IllegalStateException(
                    "Transaction required but not active [" + message + "]");
        }
    }

    /**
     * Check whether a Spring transaction is active.
     * 
     * @return true if a Spring transaction is active
     */
    private static boolean isTransactionActive() {
        try {
            return TransactionSynchronizationManager
                    .isActualTransactionActive();
        } catch (Throwable t) {
            throw new IllegalStateException(
                    "Unable to determine transaction status", t);
        }
    }

    /**
     * Prevent construction.
     */
    private SpringTransactionUtils() {
    }

}
