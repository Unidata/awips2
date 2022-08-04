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
package com.raytheon.uf.viz.alertviz;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Implements an exception for Alertviz
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2008            chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class AlertvizException extends VizException {
    private static final long serialVersionUID = 1L;

    /**
     * Default Constructor
     * 
     */
    public AlertvizException() {
        super();
    }

    /**
     * @param message
     */
    public AlertvizException(String message) {
        super(message);
    }

    /**
     * @param message
     * @param cause
     */
    public AlertvizException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param cause
     */
    public AlertvizException(Throwable cause) {
        super(cause);
    }
}
