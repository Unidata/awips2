/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.uf.common.mpe.constants;

/**
 * 
 * Constants for RFC QPE
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2017 17911      wkwock      Initial creation
 *
 * </pre>
 *
 * @author wkwock
 */
public class RFCQPEConstants {
    /** color user name */
    public static final String CVUSE = "PRECIP_ACCUM";

    /** duration in second */
    public static final int CVDUR = 3600;

    /** missing value constant */
    public static final float MISSING_VALUE = -8888.0f;

    /** missing value constant */
    public static final float OPAQUE_MISSING_VALUE = -9999.0f;
}
