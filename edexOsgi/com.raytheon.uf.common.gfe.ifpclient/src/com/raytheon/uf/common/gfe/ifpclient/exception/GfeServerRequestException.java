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
package com.raytheon.uf.common.gfe.ifpclient.exception;

import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;

/**
 * GFE exception type when an error occurs while processing a GFE request on the
 * EDEX server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 04, 2015  #5129     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GfeServerRequestException extends GfeException {

    private static final long serialVersionUID = 8249693117179046717L;

    /**
     * 
     */
    public GfeServerRequestException() {
        super();
    }

    /**
     * @param aCause
     */
    public GfeServerRequestException(String aCause) {
        super(aCause);
    }

    /**
     * @param aCause
     * @param anException
     */
    public GfeServerRequestException(String aCause, Throwable anException) {
        super(aCause, anException);
    }

    /**
     * @param anException
     */
    public GfeServerRequestException(Throwable anException) {
        super(anException);
    }
}
