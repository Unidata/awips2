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
 */
package com.raytheon.uf.edex.ingest.notification;

/**
 * Thrown if a configuration is invalid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2013 2170       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class InvalidNotificationConfigException extends Exception {

    private static final long serialVersionUID = 1L;

    public InvalidNotificationConfigException() {
        super();
    }

    public InvalidNotificationConfigException(String message, Throwable cause) {
        super(message, cause);
    }

    public InvalidNotificationConfigException(String message) {
        super(message);
    }

    public InvalidNotificationConfigException(Throwable cause) {
        super(cause);
    }
}
