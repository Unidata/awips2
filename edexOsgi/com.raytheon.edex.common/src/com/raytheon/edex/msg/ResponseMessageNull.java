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
package com.raytheon.edex.msg;

import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.util.StringUtil;

/**
 * A response message that allows the &mu;Engine to respond when 
 * no results are available.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 14May2007    70          MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class ResponseMessageNull extends AbstractResponseMessage {
    private String time = null;
    private String message = null;
    /**
     * Constructor. No arg constructor.
     */
    public ResponseMessageNull() {
        // intentionally empty
    }
    /**
     * Constructor. Private constructor used by @{link {@link #generateNullResponse(String, String, String)}.
     * 
     * @param message The message for the response.
     * @param uri the data uri from the request.
     * @param time the time of the response.
     */
    private ResponseMessageNull(String message, String uri, String time) {
        this.fileType="ascii";
        this.dataURI = StringUtil.isEmptyString(uri)?"":uri;
        this.time = StringUtil.isEmptyString(time)?"":time;
        this.message = StringUtil.isEmptyString(message)?"":message;
    }
    /**
     * Static method that creates an null Response Message. Uses the
     * private constructor to create the object.
     * 
     * @param message The message for the response.
     * @param uri the data uri from the request.
     * @param time the time of the response.
     * 
     * @return fully formed null response message
     */
    public static ResponseMessageNull generateNullResponse(String message, String uri, String time) {
        return new ResponseMessageNull(message,uri,time);
    }
    
    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }
    /**
     * @param message the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }
    /**
     * @return the time
     */
    public String getTime() {
        return time;
    }
    /**
     * @param time the time to set
     */
    public void setTime(String time) {
        this.time = time;
    }
    
}
