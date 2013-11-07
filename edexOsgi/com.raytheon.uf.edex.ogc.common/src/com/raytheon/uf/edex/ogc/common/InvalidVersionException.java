/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common;

/**
 * Exception thrown when parsing version numbers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class InvalidVersionException extends Exception {

    private static final long serialVersionUID = -7212986504698593811L;

    /**
     * 
     */
    public InvalidVersionException() {
    }

    /**
     * @param message
     */
    public InvalidVersionException(String message) {
        super(message);
    }

    /**
     * @param cause
     */
    public InvalidVersionException(Throwable cause) {
        super(cause);
    }

    /**
     * @param message
     * @param cause
     */
    public InvalidVersionException(String message, Throwable cause) {
        super(message, cause);
    }

}
