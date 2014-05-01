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

package com.raytheon.uf.viz.core.drawables;

import org.eclipse.swt.graphics.RGB;

import com.vividsolutions.jts.geom.LineString;

/**
 * Represents a shaded shape
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public interface IShadedShape extends IShape {

    /**
     * Add a sequence of latitude and longitudes which form a (closed) polygon
     * 
     * @param lineString
     *            the sequence of shape rings
     * @param color
     *            the color of the polygon
     */
    public abstract void addPolygon(LineString[] lineString, RGB color);

    /**
     * Add a sequence of points in the pixel space
     * 
     * @param contours
     *            the sequence of shape rings
     * @param color
     *            the color of the polygon
     */
    public abstract void addPolygonPixelSpace(LineString[] contours, RGB color);

    /**
     * Specify a fill pattern to use, rather than the default--a flat fill
     * pattern
     * 
     * @param pattern
     *            the 32x32 bitpattern to use for filling
     */
    public abstract void setFillPattern(byte[] pattern);

}
