/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class VerticalLevelLookup {

    public static Reference getReference(String masterLevel) {
        if ("FH".equalsIgnoreCase(masterLevel)) {
            return Reference.ABOVE_MSL;
        }
        if ("MB".equalsIgnoreCase(masterLevel)) {
            return Reference.PRESSURE_LEVEL;
        }
        if ("FHAG".equalsIgnoreCase(masterLevel)) {
            return Reference.ABOVE_GROUND;
        }
        return Reference.UNKNOWN;
    }

}
