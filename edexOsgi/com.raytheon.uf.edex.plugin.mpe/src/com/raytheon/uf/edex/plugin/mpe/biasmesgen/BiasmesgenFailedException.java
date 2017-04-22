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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

/**
 * Signals that the Biasmesgen application has failed to finish.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2016 5576       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasmesgenFailedException extends Exception {

    private static final long serialVersionUID = -2396307235025920279L;

    public BiasmesgenFailedException(String message) {
        super(message);
    }

    public BiasmesgenFailedException(String message, Throwable cause) {
        super(message, cause);
    }
}