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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Set of PersistencePathKeys
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
@XmlRootElement(name = "pathKeySet")
@XmlAccessorType(XmlAccessType.NONE)
public class PersistencePathKeySet {

    @XmlElements({ @XmlElement(name = "pathKey", type = PersistencePathKey.class) })
    private List<PersistencePathKey> pathKeys;

    /**
     * @return the pathKeys
     */
    public List<PersistencePathKey> getPathKeys() {
        return pathKeys;
    }

    /**
     * @param pathKeys
     *            the pathKeys to set
     */
    public void setPathKeys(List<PersistencePathKey> pathKeys) {
        this.pathKeys = pathKeys;
    }

}
