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
package com.raytheon.uf.edex.registry.ebxml.services.notification;

/**
 * Container class for holding notification destinations. Intentionally
 * package-private, as it's an implementation detail.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2013 1672       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class NotificationDestination {

    /** The type of endpoint */
    private final String endpointType;

    /** The address of the endpoint */
    private final String endpointAddress;

    /** The email notification formatter **/
    private String emailNotificationFormatter;

    /**
     * Creates a new destination
     * 
     * @param endpointType
     *            The endpoint type
     * @param endpointAddress
     *            The endpoint address
     */
    public NotificationDestination(String endpointType, String endpointAddress) {
        this.endpointType = endpointType;
        this.endpointAddress = endpointAddress;
    }

    public String getEndpointType() {
        return endpointType;
    }

    public String getDestination() {
        return endpointAddress;
    }

    public String getEmailNotificationFormatter() {
        return emailNotificationFormatter;
    }

    public void setEmailNotificationFormatter(String emailNotificationFormatter) {
        this.emailNotificationFormatter = emailNotificationFormatter;
    }

}
