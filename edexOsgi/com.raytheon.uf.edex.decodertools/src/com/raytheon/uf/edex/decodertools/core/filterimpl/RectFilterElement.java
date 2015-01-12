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
package com.raytheon.uf.edex.decodertools.core.filterimpl;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.vividsolutions.jts.geom.Geometry;

/**
 * This element implements a simple rectangular area filter that checks if
 * a specified PluginDataObject is located within the rectangle.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RectFilterElement extends AbstractFilterElement implements ISerializableObject {

    private static final String FMT = "RectFilterElement:%s[%8.4f,%9.4f]-[%8.4f,%9.4f]";

    @XmlElement
    @DynamicSerializeElement
    private double upperLeftLat = 0;
    
    @XmlElement
    @DynamicSerializeElement
    private double upperLeftLon = 0;
    
    @XmlElement
    @DynamicSerializeElement
    private double lowerRightLat = 0;

    @XmlElement
    @DynamicSerializeElement
    private double lowerRightLon = 0;

    /**
     * Executes this filter element against the supplied report data. The
     * supplied report is returned if it matches the filter criteria. A null
     * report reference is returned if the report fails.
     * 
     * @param report
     * @return may be null
     */
    @Override
    public PluginDataObject filter(PluginDataObject report) {
        boolean pass = false;
        
        if(report instanceof ISpatialEnabled) {
            ISpatialObject loc = ((ISpatialEnabled) report).getSpatialObject();
            if(loc != null) {
                Geometry geometry = loc.getGeometry();
                if(geometry != null) {

                    double lat = DecoderTools.getCoordinateLatitude(geometry.getCoordinate());
                    double lon = DecoderTools.getCoordinateLongitude(geometry.getCoordinate());

                    if((lat <= upperLeftLat)&&(lat >= lowerRightLat)) {
                        pass = ((lon >= upperLeftLon)&&(lon <= lowerRightLon));
                    }
                }
            }
        }
        return (pass ? report : null) ;
    }

    /**
     * @return the upperLeftLat
     */
    public double getUpperLeftLat() {
        return upperLeftLat;
    }

    /**
     * @param upperLeftLat the upperLeftLat to set
     */
    public void setUpperLeftLat(double upperLeftLat) {
        this.upperLeftLat = upperLeftLat;
    }

    /**
     * @return the upperLeftLon
     */
    public double getUpperLeftLon() {
        return upperLeftLon;
    }

    /**
     * @param upperLeftLon the upperLeftLon to set
     */
    public void setUpperLeftLon(double upperLeftLon) {
        this.upperLeftLon = upperLeftLon;
    }

    /**
     * @return the lowerRightLat
     */
    public double getLowerRightLat() {
        return lowerRightLat;
    }

    /**
     * @param lowerRightLat the lowerRightLat to set
     */
    public void setLowerRightLat(double lowerRightLat) {
        this.lowerRightLat = lowerRightLat;
    }

    /**
     * @return the lowerRightLon
     */
    public double getLowerRightLon() {
        return lowerRightLon;
    }

    /**
     * @param lowerRightLon the lowerRightLon to set
     */
    public void setLowerRightLon(double lowerRightLon) {
        this.lowerRightLon = lowerRightLon;
    }

    public String toString() {
        return String.format(FMT,getFilterElementName(),upperLeftLat,upperLeftLon,lowerRightLat,lowerRightLon); 
    }
    
}
