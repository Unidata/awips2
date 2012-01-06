/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.edex.util.ncgrib;

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
@XmlRootElement(name = "nclevels")
public class NcgridLevels implements ISerializableObject {

    private HashMap<String, String> map = new HashMap<String, String>();

    /**
     * Constructor for JAXB.
     */
    public NcgridLevels() {
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
