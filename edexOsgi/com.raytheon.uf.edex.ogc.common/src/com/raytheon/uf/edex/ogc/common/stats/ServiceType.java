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
 * Enumeration representing the types of operations that statistics are recorded
 * for within the IGCServiceRecorder
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
public enum ServiceType {
    WFS, WCS, OGC, REGISTRY,
    // special case for encompassing all values
    OPEN;

    public static ServiceType permissiveValueOf(String name) {
        for (ServiceType e : values()) {
            if (e.name().equals(name)) {
                return e;
            }
        }
        return null;
    }

}
