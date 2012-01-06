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

import java.util.Date;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;

/**
 * Represent a subscription response for the server. This message
 * is returned in response to a subscribe/unsubscribe request. 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01Sep2006    #18         MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class ResponseMessageSubscription extends AbstractResponseMessage {
    private Boolean status;
    private String statusMessage;
    /**
     * Constructor.
     */
    public ResponseMessageSubscription() {
        // intentionally empty
    }
    /**
     * Constructor. Private constructor used by the generate message
     * method {@link #generateSubscriptionResponse(boolean, boolean, Exception)}.
     * 
     * @param status status of the subscription request
     * @param message client created message
     * @param cause the exception that caused the subscription to fail
     */
    private ResponseMessageSubscription(boolean status, 
                                        String message, 
                                        Exception cause) {
        this.statusMessage = message;
        if (cause != null) {
            this.statusMessage += " Cause was " + Util.printString(cause);
            if(cause.getCause() != null) {
                this.statusMessage += " " + Util.printString(cause.getCause());
            }
        }

        this.status = status;
        /*
         * safe values for base class attributes
         */
        this.fileType = "text";
        this.dataURI = "";
        this.validTime = new Date();
    }
    /**
     * Convience method to allow creation of an Subscription Response Message.
     * Clients may use this method to avoid some of the details of the message
     * creation. The {@code cause } argument should be {@code null } when no 
     * exception is to be passed along.
     * 
     * @param subscribe indicates type of subscription action. Use {@code true}
     *                  for a subscribe action, use {@code false} for an unsubscribe
     *                  action 
     * @param status indicates if the subscription request was successful
     * @param cause (optional) the cause if the request failed.
     *   
     * @return the Subscription Response Message
     */
    public static ResponseMessageSubscription generateSubscriptionResponse(boolean subscribe,
                                                                           boolean status,
                                                                           Exception cause) {
        String message = (subscribe ? "Subscribe ":"Unsubscribe ") + 
                         (status ? "request successful."
                                   : "request failed.");
        return new ResponseMessageSubscription(status, message, cause);
    }
    /**
     * @return the message
     */
    public String getStatusMessage() {
        return statusMessage;
    }
    /**
     * @param message the message to set
     */
    public void setStatusMessage(String message) {
        this.statusMessage = message;
    }
    /**
     * @return the status
     */
    public Boolean isStatus() {
        return status;
    }
    /**
     * @param status the status to set
     */
    public void setStatus(Boolean status) {
        this.status = status;
    }
    
}
