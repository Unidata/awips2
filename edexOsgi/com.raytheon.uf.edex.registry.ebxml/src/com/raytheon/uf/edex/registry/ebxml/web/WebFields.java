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
package com.raytheon.uf.edex.registry.ebxml.web;

/**
 * Utility enum class used to defined the fields on the Data Delivery Registry
 * Web interface pages
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 7/30/2012    724        bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public enum WebFields {

    /** The user ID field */
    ID("id", TYPE.Text),

    /**
     * The object type metadata information. Valid values are User and
     * Organization
     */
    OBJ_TYPE("objType", TYPE.Meta),

    /** The action being performed on the web page */
    ACTION("action", TYPE.Meta),

    /** The first name field */
    FIRST_NAME("firstName", TYPE.Text),

    /** The middle name field */
    MIDDLE_NAME("middlName", TYPE.Text),

    /** The last name field */
    LAST_NAME("lastName", TYPE.Text),

    /** The user organization field */
    USER_ORG("userOrg", TYPE.Select),

    /** The organization name field */
    ORGANIZATION_NAME("organizationName", TYPE.Text),

    /** The user role field */
    USER_ROLE("userRole", TYPE.Select),

    /** The address type field */
    ADDRESS_TYPE("addressType", TYPE.Select),

    /** The address 1 field */
    ADDRESS_1("streetAddress1", TYPE.Text),

    /** The address 2 field */
    ADDRESS_2("streetAddress2", TYPE.Text),

    /** The city field */
    CITY("city", TYPE.Text),

    /** The state field */
    STATE("state", TYPE.Select),

    /** The country field */
    COUNTRY("country", TYPE.Select),

    /** The postal code field */
    POSTAL_CODE("postalCode", TYPE.Text),

    /** The telephone type field */
    TELEPHONE_TYPE("telephoneType", TYPE.Select),

    /** The area code field */
    AREA_CODE("areaCode", TYPE.Text),

    /** The telephone number prefix field */
    PHONE_1("phone1", TYPE.Text),

    /** The telephone number suffix field */
    PHONE_2("phone2", TYPE.Text),

    /** The telephone extension field */
    EXTENSION("extension", TYPE.Text),

    /** The email type field */
    EMAIL_TYPE("emailType", TYPE.Select),

    /** The email address field */
    EMAIL("email", TYPE.Text),

    /** The primary contact field */
    PRIMARY_CONTACT("primaryContact", TYPE.Span);

    /**
     * The type of HTML element
     * 
     * @author bphillip
     * 
     */
    private enum TYPE {
        /** A text field */
        Text,

        /** A select box */
        Select,

        /** An HTML span */
        Span,

        /** A metadata element not displayed on the web page */
        Meta
    }

    /** The name of the field on the web page */
    private final String fieldName;

    /** The type of the field on the web page */
    private final TYPE fieldType;

    /**
     * Constructs a new web field with the given name and type
     * 
     * @param fieldName
     *            The name of the field
     * @param fieldType
     *            The type of the field
     */
    WebFields(String fieldName, TYPE fieldType) {
        this.fieldName = fieldName;
        this.fieldType = fieldType;
    }

    /**
     * Gets the name of the field
     * 
     * @return The name of the field
     */
    public String fieldName() {
        return this.fieldName;
    }

    /**
     * Gets the type of the field
     * 
     * @return The type of the field
     */
    public String fieldType() {
        return this.fieldType.toString();
    }

    /**
     * Gets the name and type of the field concatenated together
     * 
     * @return The name and type of the field concatenated together
     */
    public String field() {
        return this.fieldName + fieldType.toString();
    }
}
