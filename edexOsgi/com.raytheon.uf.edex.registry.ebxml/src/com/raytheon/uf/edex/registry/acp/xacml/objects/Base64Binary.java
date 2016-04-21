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
package com.raytheon.uf.edex.registry.acp.xacml.objects;

import java.util.Arrays;

import org.opensaml.xml.util.Base64;

/**
 * 
 * Class used to encapsulate the http://www.w3.org/2001/XMLSchema#base64Binary
 * type
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Base64Binary {

    /** The byte array for this object */
    private byte[] array;

    /** The String representation of this object */
    private String stringRepresentation;

    /**
     * Constructor to create a new Base64Binary from the String representation
     * 
     * @param stringRepresentation
     *            The string representation
     */
    public Base64Binary(String stringRepresentation) {
        this.stringRepresentation = stringRepresentation;
        this.array = Base64.decode(stringRepresentation);
    }

    /**
     * Constructor to create a new Base64Binary from the byte array
     * 
     * @param array
     *            The byte array
     */
    public Base64Binary(byte[] array) {
        this.stringRepresentation = Base64.encodeBytes(array);
        this.array = array;
    }

    /**
     * Gets the byte array
     * 
     * @return The byte array
     */
    public byte[] getArray() {
        return array;
    }

    /**
     * Sets the byte array
     * 
     * @param array
     *            The byte array
     */
    public void setArray(byte[] array) {
        this.array = array;
    }

    /**
     * Gets the String representation
     * 
     * @return The String representation
     */
    public String getStringRepresentation() {
        return stringRepresentation;
    }

    /**
     * Sets the string representation
     * 
     * @param stringRepresentation
     *            The string representation
     */
    public void setStringRepresentation(String stringRepresentation) {
        this.stringRepresentation = stringRepresentation;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof Base64Binary)) {
            return false;
        }
        Base64Binary rVal = (Base64Binary) obj;
        return Arrays.equals(array, rVal.getArray());
    }

}
