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
package com.raytheon.viz.gfe.smarttool;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Exception for smart tools
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2008            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolException extends VizException {

    private static final long serialVersionUID = 1L;

    private ErrorType errorType = ErrorType.NONE;

    public enum ErrorType {
        NONE, SKIPPED_GRID, LOCKED_GRID, NO_DATA
    };

    public SmartToolException(String message) {
        super(message);
    }

    public SmartToolException(String message, VizException e) {
        super(message, e);
    }

    public SmartToolException(String message, ErrorType type) {
        super(message);
        errorType = type;
    }

    public SmartToolException(String message, VizException e, ErrorType type) {
        super(message, e);
        errorType = type;
    }

    public ErrorType getErrorType() {
        return errorType;
    }

}
