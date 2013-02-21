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

package com.raytheon.uf.common.gridcoverage.exception;

import com.raytheon.uf.common.dataplugin.PluginException;

/**
 * 
 * An exception that occurs when there are errors intializing grid coverages.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridCoverageException extends PluginException {

    /**
     * Default serial version id
     */
    private static final long serialVersionUID = 1L;

    /**
     * Parser exception set with a cause.
     * 
     * @param aCause
     *            The cause of the exception
     */
    public GridCoverageException(String aCause) {
        super(aCause);
    }

    /**
     * Parser exception set with a cause and an existing exception. Used for
     * exception chaining to preserve state.
     * 
     * @param aCause
     *            The cause of the exception
     * @param anException
     *            The exception object
     */
    public GridCoverageException(String aCause, Exception anException) {
        super(aCause, anException);
    }
}
