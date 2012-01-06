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
package com.raytheon.viz.gfe.contours.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * An implementation of a contour line. A contour line is a LineString with a
 * single level or weight. A CLine also has a flag that indicates if the line
 * has been modified. The flag should be set (true) for a new CLine. A client
 * that modifies the CLine should change the modified flag to true.
 * <P>
 * This class is a port of the original C++ CLine class from gfe. It does not
 * use Java 6 style generics since only float data is used in gfe code that uses
 * this data structure.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Mar2008    968        MW Fegan    Initial Creation.
 * 22Jan2009               wldougher   Inherit from LineString instead of PolyLine
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class CLine {

    private static final long serialVersionUID = 1L;

    private static final GeometryFactory factory = new GeometryFactory();

    private LineString lineString;

    /**
     * @return the lineString
     */
    public LineString getLineString() {
        return lineString;
    }

    /** the level of the CLine. Default is 0.0 */
    private float contourLevel;

    /** modified flag. Should be true for new or modified contour. */
    private boolean modified = true;

    /**
     * Constructor. Creates a CLine object from the specified points having the
     * specified contour level and modified flag.
     * 
     * @param c
     * @param contourLevel
     * @param modified
     */
    public CLine(Coordinate[] coords, float contourLevel, boolean modified) {
        this.lineString = factory.createLineString(coords);
        this.contourLevel = contourLevel;
        this.modified = modified;
    }

    /**
     * Returns the contour level for this CLine.
     */
    public float getContourLevel() {
        return this.contourLevel;
    }

    /**
     * Sets the contour level for this CLine.
     */
    public void setContourLevel(float contourLevel) {
        this.contourLevel = contourLevel;
    }

    /**
     * Sets the modified flag for this CLine.
     */
    public void setModified(boolean modified) {
        this.modified = modified;
    }

    /**
     * Returns the value of the modified flag for this CLine.
     */
    public boolean isModified() {
        return this.modified;
    }

    /**
     * Replace a specified range of the sequence with a new range. Elements
     * [lo]..[hi] (inclusive) of the sequence are replaced by the entire
     * sequence provided as an argument. In general, this will result in
     * changing the length of the sequence, and elements above [hi] will have
     * new indices.
     * 
     * @param lo
     * @param hi
     * @param coords
     */
    public void replace(int lo, int hi, List<Coordinate> coords) {
        int start = Math.min(lo, hi);
        int end = Math.max(lo, hi);

        start = Math.max(0, start);
        end = Math.min(this.lineString.getNumPoints(), end + 1);

        List<Coordinate> currentCoords = Arrays.asList(this.lineString
                .getCoordinates());
        List<Coordinate> first = currentCoords.subList(0, start);
        List<Coordinate> last = currentCoords
                .subList(end, currentCoords.size());
        List<Coordinate> newCoords = new ArrayList<Coordinate>(first.size()
                + coords.size() + last.size());
        newCoords.addAll(first);
        newCoords.addAll(coords);
        newCoords.addAll(last);

        this.lineString = factory.createLineString(newCoords
                .toArray(new Coordinate[newCoords.size()]));
    }

    /**
     * Remove a specified range of the sequence.
     * 
     * Elements [lo]..[hi] (inclusive) of the sequence are deleted.
     * 
     * In general, this will shorten the sequence, causing elements above [hi]
     * to have new, lower, indices.
     * 
     * @param lo
     * @param hi
     */
    public void remove(int lo, int hi) {
        replace(lo, hi, new ArrayList<Coordinate>());
    }

    /**
     * Append a list or coordinates to the end of the sequence.
     * 
     * @param coords
     */
    public void append(List<Coordinate> coords) {
        replace(this.lineString.getNumPoints(), this.lineString.getNumPoints(),
                coords);
    }
}
