/*****************************************************************************************
 * COPYRIGHT (c), 2006-2009, RAYTHEON COMPANY
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
package com.raytheon.uf.edex.decodertools.core.filterimpl;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Filter {@link PluginDataObject}s based off their distance from a specific
 * point.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 23, 2009           jkorman     Initial creation
 * Jun 11, 2014  2061     bsteffen    Remove IDecoderGettable
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RadiusFilterElement extends AbstractFilterElement {

    private static final String FMT = "RadiusFilterElement:%s[%8.4f,%9.4f,%5.1f]";
    
    private double radSqr = 0;
    
    // radius in nautical miles.
    @XmlElement
    @DynamicSerializeElement
    private double radius = 1;

    // latitude in radians.
    @XmlElement
    @DynamicSerializeElement
    private double pointLat = 0;

    // longitude in radians.
    @XmlElement
    @DynamicSerializeElement
    private double pointLon = 0;
    
    private double lonAdjust = 0;
    /**
     * 
     */
    public RadiusFilterElement () {
    }
    
    /**
     * 
     * @param lat
     * @param lon
     * @param radius
     */
    public RadiusFilterElement (double lat, double lon, double radius) {
        this.radius = radius;
        radSqr = radius * radius;
        pointLat = lat;
        pointLon = lon;
        
        lonAdjust = (60 * Math.cos(Math.toRadians(pointLat)));
    }
    
    /**
     * 
     * 
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
                    
                    lat = Math.abs(pointLat - lat);
                    lat = lat * 60;
                    lon = Math.abs(pointLon - lon);
                    lon = lon * lonAdjust;
                    
                    pass = (radSqr - (lat*lat + lon * lon)) > 0;
                }
            }
        }
        return (pass ? report : null) ;
    }

    /**
     * 
     * @param radius
     */
    public void setRadius(double radius) {
        this.radius = radius;
        radSqr = radius * radius;
    }
    
    /**
     * @return the radius
     */
    public double getRadius() {
        return radius;
    }

    /**
     * Set the latitude in degrees.
     * @param lat The latitude in degrees.
     */
    public void setPointLat(double lat) {
        pointLat = Math.toRadians(lat);
    }

    /**
     * @return the pointLat
     */
    public double getPointLat() {
        return pointLat;
    }
    
    /**
     * Set the longitude in degrees.
     * @param lon The longitude in degrees.
     */
    public void setPointLon(double lon) {
        pointLon = Math.toRadians(lon);
    }

    /**
     * @return the pointLon
     */
    public double getPointLon() {
        return pointLon;
    }

    @Override
    public String toString() {
        return String.format(FMT,getFilterElementName(),pointLat,pointLon,radius); 
    }
    
}
