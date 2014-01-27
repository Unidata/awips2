/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.edex.ogc.common.OgcException;

/**
 * Interface for retrieving geotools coordinate reference system objects using
 * native CRS URNs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface NativeCrsAuthority {

    public static final String NATIVE_CRS_PREFIX = "urn:x-wxsrv:"
            + "crs:";

    public CoordinateReferenceSystem lookup(String URN) throws OgcException;

    public String getId();

}
