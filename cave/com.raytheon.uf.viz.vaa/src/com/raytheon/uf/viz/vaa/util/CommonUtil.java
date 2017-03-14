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
package com.raytheon.uf.viz.vaa.util;

import java.util.Set;

import com.raytheon.uf.common.dataplugin.vaa.VAALocation;
import com.raytheon.uf.common.dataplugin.vaa.VAARecord;
import com.raytheon.uf.common.dataplugin.vaa.VAASubPart;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Common utilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2009 3268       jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class CommonUtil {
    
    public static Polygon getPolygon(VAARecord record, String type) {
        for(VAASubPart subPart : record.getSubParts()){
            if(subPart.getSubText().endsWith(type)){
                Coordinate[] coordinates = getCoordinates(subPart.getLocations());
                GeometryFactory factory = new GeometryFactory();
                LinearRing ring = factory.createLinearRing(coordinates);
                return factory.createPolygon(ring, null);
            }
        }     
        return null;
    }
    
    public static Polygon worldToPixel(Polygon llPolygon, MapDescriptor descriptor) {
        Coordinate[] llCoords = llPolygon.getCoordinates();
        Coordinate[] pixelCoords = new Coordinate[llCoords.length];
        for (int i = 0; i < llCoords.length; i++) {
            double[] pixelCoord = descriptor.worldToPixel(new double[] {
                    llCoords[i].x, llCoords[i].y });
            pixelCoords[i] = new Coordinate(pixelCoord[0], pixelCoord[1]);
        }
        GeometryFactory factory = new GeometryFactory();
        LinearRing ring = factory.createLinearRing(pixelCoords);
        return factory.createPolygon(ring, null);
    }
    
    public static Coordinate[] getCoordinates(Set<VAALocation> locations){
        Coordinate coordinates[] = new Coordinate[locations.size()];
        for(VAALocation location : locations){
            coordinates[location.getIndex()] = new Coordinate(location.getLongitude(),location.getLatitude()); 
        }
        return coordinates;
    }
}
