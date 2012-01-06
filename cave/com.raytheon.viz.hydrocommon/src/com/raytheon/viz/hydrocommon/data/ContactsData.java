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
package com.raytheon.viz.hydrocommon.data;

import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;

/**
 * This class contains the contacts data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20 Nov 2008             lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ContactsData extends HydroDBData {
    /**
     * Location ID.
     */
    private String lid;

    /**
     * Contact name.
     */
    private String contact;

    /**
     * Phone number.
     */
    private String phone;

    /**
     * Email address.
     */
    private String email;

    /**
     * Remarks/Concerns.
     */
    private String remark;

    /**
     * Priority.
     */
    private int priority;

    /**
     * Constructor.
     */
    public ContactsData() {
        lid = "";
        contact = "";
        phone = "";
        email = "";
        remark = "";
        priority = 9;
    }

    /**
     * Constructor.
     * 
     * @param data
     *            Result data.
     * @param dataMap
     *            Column to Index map.
     */
    public ContactsData(QueryResultRow data, Map<String, Integer> dataMap) {
        setLid(getDBValue("lid", data, dataMap, "ZZZZZ"));
        setContact(getDBValue("contact", data, dataMap, ""));
        setPhone(getDBValue("phone", data, dataMap, ""));
        setEmail(getDBValue("email", data, dataMap, ""));
        setRemark(getDBValue("remark", data, dataMap, ""));
        setPriority(getDBValue("priority", data, dataMap, 9));
    }

    /**
     * Get the Location ID
     * 
     * @return The location ID.
     */
    public String getLid() {
        return lid;
    }

    /**
     * Set the Location ID.
     * 
     * @param lid
     *            The Location ID.
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * Get the contact name.
     * 
     * @return The contact name.
     */
    public String getContact() {
        return contact;
    }

    /**
     * Set the contact name.
     * 
     * @param contact
     *            The contact name.
     */
    public void setContact(String contact) {
        this.contact = contact;
    }

    /**
     * Get the phone number.
     * 
     * @return The phone number.
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Set the phone number.
     * 
     * @param phone
     *            The phone number.
     */
    public void setPhone(String phone) {
        this.phone = phone;
    }

    /**
     * Get the email address.
     * 
     * @return The email address.
     */
    public String getEmail() {
        return email;
    }

    /**
     * Set the email address.
     * 
     * @param email
     *            The email address.
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Get the remarks/concerns.
     * 
     * @return The remarks/concerns
     */
    public String getRemark() {
        return remark;
    }

    /**
     * Set the remarks/concerns.
     * 
     * @param remark
     *            The remarks/concerns.
     */
    public void setRemark(String remark) {
        this.remark = remark;
    }

    /**
     * Get the priority.
     * 
     * @return The priority.
     */
    public int getPriority() {
        return priority;
    }

    /**
     * Set the priority.
     * 
     * @param priority
     *            The priority.
     */
    public void setPriority(int priority) {
        this.priority = priority;
    }

    /**
     * Get the priority, contact name, and phone number in a formatted string
     * for the display.
     * 
     * @return The formatted data.
     */
    public String toString() {
        return String.format("%-15d %-40S %S", priority, contact, phone);
    }
}
