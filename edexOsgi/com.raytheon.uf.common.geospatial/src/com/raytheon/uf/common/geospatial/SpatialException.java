/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.common.geospatial;

/**
 * Base exception for Viz
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/7/09        2596            dhladky    Initial Creation.
 *    
 * </pre>
 * 
 * @author dhladky
 * 
 */
public class SpatialException extends Exception {

    private static final long serialVersionUID = 142562654L;

    /**
     * Default Constructor
     * 
     */
    public SpatialException() {
        super();
    }

    /**
     * @param message
     */
    public SpatialException(String message) {
        super(message);
    }

    /**
     * @param message
     * @param cause
     */
    public SpatialException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param cause
     */
    public SpatialException(Throwable cause) {
        super(cause);
    }
}

