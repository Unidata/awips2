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
package com.raytheon.uf.common.dataplugin.warning.gis;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Collection of GeospatialTime representing a set of generated geospatial data
 * for warngen configurations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "geoTimeSet")
public class GeospatialTimeSet {
    @XmlElement(name = "geoTime")
    private List<GeospatialTime> data;

    public List<GeospatialTime> getData() {
        return data;
    }

    public void setData(List<GeospatialTime> data) {
        this.data = data;
    }

    public Map<GeospatialMetadata, GeospatialTime> getDataAsMap() {
        Map<GeospatialMetadata, GeospatialTime> rval = null;
        if (data != null) {
            int size = (int) (data.size() * 1.25) + 1;
            rval = new HashMap<GeospatialMetadata, GeospatialTime>(size);
            for (GeospatialTime time : data) {
                rval.put(time.getMetaData(), time);
            }
        }

        return rval;
    }
}
