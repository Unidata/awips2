/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.stats;

/**
 * Type of operation for which statistics are being measured
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2013            behemmi     Initial creation
 * 
 * </pre>
 * 
 * @author behemmi
 * @version 1.0
 */
public enum OperationType {
    STORE, QUERY, DELETE,
    // special case for encompassing all values
    OPEN;

    public static OperationType permissiveValueOf(String name) {
        for (OperationType e : values()) {
            if (e.name().equals(name)) {
                return e;
            }
        }
        return null;
    }
}
