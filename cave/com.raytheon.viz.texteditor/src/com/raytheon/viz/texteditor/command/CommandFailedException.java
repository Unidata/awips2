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
package com.raytheon.viz.texteditor.command;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * CommandFailedException
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    7/30/2009    2191        rjpeter     Initial Creation.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1
 */
public class CommandFailedException extends VizException {

    private static final long serialVersionUID = 1L;

    public CommandFailedException() {
        super();
    }

    public CommandFailedException(String message, Throwable cause) {
        super(message, cause);
    }

    public CommandFailedException(String message) {
        super(message);
    }

    public CommandFailedException(Throwable cause) {
        super(cause);
    }

}