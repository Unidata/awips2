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
package com.raytheon.uf.viz.bufrsigwx.common;

import java.util.Formatter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provides a resource that will display troppopause height/locations
 * data for a given reference time.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  09/25/2009             jsanchez    Initial creation.
 *  07/24/2014  3429       mapeters    Updated deprecated drawLine() calls.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class SigWxCommon {
    
    public static final double MISSING = -9999.0; 
    
    /**
     * Converts a coordinate's longitude and latitude into
     * pixel coordinates relative to the world.
     * @param lon longitude
     * @param lat latitude
     * @return world pixel
     */
    public static double[] lonLatToWorldPixel(MapDescriptor d, double lon, double lat) {
        Coordinate location = new Coordinate(lon, lat);
        double[] locationLatLon = {location.x, location.y};
        return d.worldToPixel(locationLatLon);
    }
    
    /**
     * Formats the value according to the format specifier
     * @param value the value to be formated (123.45678....)
     * @param format the format specifier
     * @return formatted value
     */
    public static String format(double value, String format) {
        StringBuilder sb = new StringBuilder();
        Formatter formatter = new Formatter(sb);
        formatter.format(format, value);
        return sb.toString();
    }
    
    public static void paintArrowHead(IGraphicsTarget target, double[] center,
            Double length, Double dir, RGB color) throws VizException {
        double[] pointPixel = target.getPointOnCircle(center[0], center[1],
                center[2], length, dir + 210);
        DrawableLine line = new DrawableLine();
        line.setCoordinates(pointPixel[0], pointPixel[1], pointPixel[2]);
        line.addPoint(center[0],  center[1], center[2]);
        double[] pointPixel2 = target.getPointOnCircle(center[0], center[1],
                center[2],
                length, dir + 150);
        line.addPoint(pointPixel2[0], pointPixel2[1], pointPixel2[2]);
        line.basics.color = color;
        line.width = 1.5f;
        target.drawLine(line);
    }
}
