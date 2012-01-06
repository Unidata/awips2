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

package com.raytheon.edex.uengine;

/**
 * Convenience class. This class is a container that simplifies setting
 * {@link org.apache.commons.digester.Digester} rules for handling nested
 * "property like" tags, which are similar to Apache ANT's {@literal <property />}
 * tags. A property tag as foot print {@literal <tag_name name="..." value="..." />}
 * and is used to specify "key/value" pairs into an underlying property structure.
 * This class captures a key/value pair into a single object for processing by
 * the tag object. That is, a single instance of this class represents a single
 * key.value property.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 23Oct2006    TO4         MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */
public class TaskParam {
    /*
     * the property name (key).
     */
    private String name = null;
    /**
     * the property value.
     */
    private Object value = null;
    
    /**
     * Constructor. Creates an empty {@code TaskParam} object.
     */
    public TaskParam() {}
    /**
     * @return the property name
     */
    public String getName() {
        return name;
    }
    /**
     * @param name the property name to set
     */
    public void setName(String name) {
        this.name = name;
    }
    /**
     * @return the property value
     */
    public Object getValue() {
        return value;
    }
    /**
     * @param value the property value to set
     */
    public void setValue(Object value) {
        this.value = value;
    }
}
