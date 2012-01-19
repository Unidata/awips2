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
package com.raytheon.edex.util.grib;

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Contains a map of short level names to their full name in the database.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 5, 2008				njensen	    Initial creation
 * Aug 22, 2008 1502        dglazesk    JAXB Annotations added
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
/**
 * @XmlAccessType intentionally left off so that the default JAXB marshallers
 *                and unmarshallers can properly deal with the XML.
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "levels")
public class GridLevels implements ISerializableObject {

    private HashMap<String, String> map = new HashMap<String, String>();

    /**
     * Constructor for JAXB.
     */
    public GridLevels() {
    }

    /**
     * @return the map
     */
    public HashMap<String, String> getMap() {
        return map;
    }

    /**
     * @param map
     *            the map to set
     */
    public void setMap(HashMap<String, String> map) {
        this.map = map;
    }

}
