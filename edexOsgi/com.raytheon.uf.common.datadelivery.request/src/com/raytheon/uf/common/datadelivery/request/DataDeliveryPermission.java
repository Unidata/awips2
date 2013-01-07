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
package com.raytheon.uf.common.datadelivery.request;

/**
 * Consolidate string system role usage into an enum.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1241      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public enum DataDeliveryPermission {
    SUBSCRIPTION_APPROVE_USER("subscription.approve.user"), SUBSCRIPTION_APPROVE_SITE(
            "subscription.approve.site"), SUBSCRIPTION_APPROVE_VIEW(
            "subscription.approve.view"), SUBSCRIPTION_DATASET_BROWSER(
            "subscription.dataset.browser"), NOTIFICATION_VIEW(
            "notification.view"), SUBSCRIPTION_VIEW("subscription.view"), SUBSCRIPTION_EDIT(
            "subscription.edit"), SYSTEM_MANAGEMENT_VIEW(
            "systemmanagement.view"), SUBSCRIPTION_CREATE("subscription.create"), SUBSCRIPTION_ACTIVATE(
            "subscription.activate"), SUBSCRIPTION_DELETE("subscription.delete"), SYSTEM_MANAGEMENT_CREATE(
            "systemmanagement.create");

    private String stringValue;

    private DataDeliveryPermission(String stringValue) {
        this.stringValue = stringValue;
    }

    @Override
    public String toString() {
        return stringValue;
    }

    /**
     * Retrieve the {@link DataDeliveryPermission} enum from its string value.
     * 
     * @param string
     *            the string
     * @return the permission
     * @throws IllegalArgumentException
     *             if no enum has the string value
     */
    public static DataDeliveryPermission fromString(String string) {
        // TODO: Create a Map<String, DataDeliveryPermission> of these for
        // convenience?
        for (DataDeliveryPermission permission : DataDeliveryPermission
                .values()) {
            if (permission.toString().equals(string)) {
                return permission;
            }
        }

        throw new IllegalArgumentException("No enum with toString() value of ["
                + string + "].");
    }
}
