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
package com.raytheon.edex.plugin.grib.spatial;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Default sub grid center point. If latitude/longitude may be null. This would
 * show that the wfo center point should be looked up.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2012 977        rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class DefaultSubGridCenterPoint implements ISerializableObject {
    private Double centerLatitude;

    private Double centerLongitude;

    public Double getCenterLatitude() {
        return centerLatitude;
    }

    @XmlElement
    public void setCenterLatitude(final Double centerLatitude) {
        this.centerLatitude = centerLatitude;
        if (this.centerLatitude != null) {
            this.centerLatitude = new Double(MapUtil.correctLat(centerLatitude
                    .doubleValue()));
        }
    }

    public Double getCenterLongitude() {
        return centerLongitude;
    }

    @XmlElement
    public void setCenterLongitude(final Double centerLongitude) {
        this.centerLongitude = centerLongitude;
        if (this.centerLongitude != null) {
            this.centerLongitude = new Double(
                    MapUtil.correctLon(centerLongitude.doubleValue()));
        }
    }
}
