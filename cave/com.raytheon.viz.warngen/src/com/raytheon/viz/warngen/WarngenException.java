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
package com.raytheon.viz.warngen;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * WarngenException
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 21, 2007             chammack    Initial Creation.
 *   
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class WarngenException extends VizException {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public WarngenException() {
        super();
    }

    public WarngenException(String message, Throwable cause) {
        super(message, cause);
    }

    public WarngenException(String message) {
        super(message);
    }

    public WarngenException(Throwable cause) {
        super(cause);
    }

}
