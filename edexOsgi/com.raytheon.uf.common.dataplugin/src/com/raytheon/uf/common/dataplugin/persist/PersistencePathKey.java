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
package com.raytheon.uf.common.dataplugin.persist;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * PluginDataObject key to use in IDataStore's directory path.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation
 * Nov 08, 2013 2361       njensen     Remove ISerializableObject
 * 
 * </pre>
 * 
 */
@XmlRootElement(name = "pathKey")
@XmlAccessorType(XmlAccessType.NONE)
public class PersistencePathKey implements Comparable<PersistencePathKey> {

    @XmlElement
    private String key;

    @XmlElement
    private int order;

    public PersistencePathKey() {

    }

    public PersistencePathKey(String key, int order) {
        this.key = key;
        this.order = order;
    }

    /**
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * @return the order
     */
    public int getOrder() {
        return order;
    }

    /**
     * @param order
     *            the order to set
     */
    public void setOrder(int order) {
        this.order = order;
    }

    @Override
    public int compareTo(PersistencePathKey o) {
        // Less than
        if (this.getOrder() < o.getOrder()) {
            return -1;
        }
        // Equal
        else if (this.getOrder() == o.getOrder()) {
            return 0;
        }
        // Greater than
        else if (this.getOrder() > o.getOrder()) {
            return 1;
        }

        // Should never get here
        return 0;
    }

}
