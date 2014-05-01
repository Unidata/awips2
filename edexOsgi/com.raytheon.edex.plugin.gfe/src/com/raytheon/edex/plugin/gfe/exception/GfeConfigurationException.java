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

import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;

/**
 * Exception class for exceptions pertaining the GFE server configuration
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 03/28/13     #1837      dgilling    Implement missing constructors from
 *                                     super-class.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GfeConfigurationException extends GfeException {

    private static final long serialVersionUID = 1L;

    public GfeConfigurationException() {
        super();
    }

    /**
     * @param aCause
     */
    public GfeConfigurationException(String aCause) {
        super(aCause);
    }

    /**
     * Parser exception set with a cause and an existing exception. Used for
     * exception chaining to preserve state.
     * 
     * @param aCause
     * @param anException
     */
    public GfeConfigurationException(String aCause, Throwable anException) {
        super(aCause, anException);
    }

    public GfeConfigurationException(Throwable anException) {
        super(anException);
    }
}
