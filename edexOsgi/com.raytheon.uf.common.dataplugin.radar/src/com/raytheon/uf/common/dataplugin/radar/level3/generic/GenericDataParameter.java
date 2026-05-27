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
package com.raytheon.uf.common.dataplugin.radar.level3.generic;

import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Structure to handle the Product Parameter Data Structure of the Generic
 * Packet.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2009            askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

@DynamicSerialize
public class GenericDataParameter implements ISerializableObject {
    public static enum AttributeNames {
        NAME, TYPE, UNITS, RANGE, VALUE, DEFAULT, ACCURACY, DESCRIPTION, CONVERSION, EXCEPTION
    }

    @DynamicSerializeElement
    private String id;

    // Hashmap to relate the name and description pairs of the Parameter
    @DynamicSerializeElement
    private HashMap<String, String> attributes = new HashMap<String, String>();

    private static final String pairPattern = "([\\w*\\s*?]*)\\=([(\\<|\\{|\\[|\\()?\\w*\\s*?\\.?\\,?\\-?\\/?\\%?(\\>|\\}|\\]|\\))?]*)";

    private static final Pattern PARAM_PATTERN = Pattern.compile(pairPattern);

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the attributes
     */
    public HashMap<String, String> getAttributes() {
        return attributes;
    }

    /**
     * @param attributes
     *            the attributes to set
     */
    public void setAttributes(HashMap<String, String> attributes) {
        this.attributes = attributes;
    }

    /**
     * Takes a full parameter attribute pairs String in the format: name =
     * description; name = description; etc.
     * 
     * @param attributePairs
     */
    public void addAttributePairs(String attributePairs) {
        Matcher matcher = PARAM_PATTERN.matcher(attributePairs);

        while (matcher.find()) {
            attributes.put(matcher.group(1).trim(), matcher.group(2).trim());
        }
    }

    /**
     * Convenience method for retrieving the value of the parameter
     * 
     * @return The String containing the "attribute description" for the
     *         "attribute name" "Value".
     */
    public String getValue() {
        return getAttributeDescription(AttributeNames.VALUE.toString());
    }

    /**
     * Convenience method for retrieving the description of the parameter
     * 
     * @return The String containing the "attribute description" for the
     *         "attribute name" "Description".
     */
    public String getDescription() {
        return getAttributeDescription(AttributeNames.DESCRIPTION.toString());
    }

    /**
     * Convenience method for retrieving the name of the parameter
     * 
     * @return The String containing the "attribute description" for the
     *         "attribute name" "Name".
     */
    public String getName() {
        return getAttributeDescription(AttributeNames.NAME.toString());
    }

    /**
     * Convenience method for retrieving the type (i.e. Float, String, int, s,
     * etc.) of the parameter
     * 
     * @return The String containing the "attribute description" for the
     *         "attribute name" "Type".
     */
    public String getType() {
        return getAttributeDescription(AttributeNames.TYPE.toString());
    }

    /**
     * Convenience method for retrieving the unit of the parameter
     * 
     * @return The String containing the "attribute description" for the
     *         "attribute name" "Unit".
     */
    public String getUnit() {
        return getAttributeDescription(AttributeNames.UNITS.toString());
    }

    /**
     * Method for retrieving the "attribute description" for the corresponding
     * "attribute name".
     * 
     * @param attributeName
     *            The case-insensitive "attribute name".
     * @return The String containing the "attribute description" for the
     *         "attribute name" specified.
     */
    public String getAttributeDescription(String attributeName) {
        String rval = "";

        for (String name : attributes.keySet()) {
            if (name.equalsIgnoreCase(attributeName)) {
                rval = attributes.get(name);
                break;
            }
        }

        return rval;
    }

    @Override
    public String toString() {
        StringBuffer rval = new StringBuffer("Generic Parameter - ID: " + id);

        for (String name : attributes.keySet()) {
            rval.append(String.format("\n\t\"%s\" = \"%s\"", name, attributes
                    .get(name)));
        }

        return rval.toString();
    }
}