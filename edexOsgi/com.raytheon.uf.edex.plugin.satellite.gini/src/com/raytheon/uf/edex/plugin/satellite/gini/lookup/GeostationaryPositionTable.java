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
package com.raytheon.uf.edex.plugin.satellite.gini.lookup;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Config XML object for GINI satellite geostationary position table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 4, 2014  2714      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class GeostationaryPositionTable {

    @XmlElement(name = "entry")
    private List<GeostationaryPosition> entries;

    /**
     * 
     */
    public GeostationaryPositionTable() {
    }

    /**
     * @return table entries mapped by id
     */
    public Map<String, GeostationaryPosition> createMap() {
        Map<String, GeostationaryPosition> rval;
        if (entries == null) {
            rval = Collections.emptyMap();
        } else {
            rval = new ConcurrentHashMap<>(entries.size());
            for (GeostationaryPosition pos : entries) {
                rval.put(pos.getId(), pos);
            }
        }
        return rval;
    }

    /**
     * @return the entries
     */
    public List<GeostationaryPosition> getEntries() {
        return entries;
    }

    /**
     * @param entries
     *            the entries to set
     */
    public void setEntries(List<GeostationaryPosition> entries) {
        this.entries = entries;
    }

}
