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
package com.raytheon.uf.viz.core.datastructure;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Exception for data cubes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2007            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class VizDataCubeException extends VizException {
    
    private static final long serialVersionUID = 1L;

    /**
     * Default Constructor
     * 
     */
    public VizDataCubeException() {
        super();
    }

    /**
     * @param message
     */
    public VizDataCubeException(String message) {
        super(message);
    }

    /**
     * @param message
     * @param cause
     */
    public VizDataCubeException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param cause
     */
    public VizDataCubeException(Throwable cause) {
        super(cause);
    }


}
