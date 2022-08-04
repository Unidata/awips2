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
package com.raytheon.edex.plugin.gfe.exception;

/**
 * Exception thrown when a database table referenced in localMaps.py cannot be
 * located in the maps database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class MissingLocalMapsException extends GfeConfigurationException {

    private static final long serialVersionUID = 1L;

    public MissingLocalMapsException() {
        super();
    }

    public MissingLocalMapsException(String message) {
        super(message);
    }

    public MissingLocalMapsException(String message, Throwable cause) {
        super(message, cause);
    }

    public MissingLocalMapsException(Throwable cause) {
        super(cause);
    }
}
