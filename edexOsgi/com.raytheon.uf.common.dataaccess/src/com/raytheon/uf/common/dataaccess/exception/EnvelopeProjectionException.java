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
package com.raytheon.uf.common.dataaccess.exception;

/**
 * An exception for when an IDataFactory cannot handle a request because the
 * requested envelope cannot be mapped onto available data. For example it is
 * valid for a factory to throw this exception if data in a mercator projection
 * is requested over the pole, which cannot be represented in mercator
 * projections.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 04, 2014  2672     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class EnvelopeProjectionException extends DataAccessException {

    private static final long serialVersionUID = 1;

    public EnvelopeProjectionException(String message, Throwable cause) {
        super(message, cause);
    }

    public EnvelopeProjectionException(String message) {
        super(message);
    }

}
