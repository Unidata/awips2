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
package com.raytheon.viz.core.contours.util;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * An object that contains a list of coordinates used to draw stream lines over
 * a grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2013  #1999     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class StreamLineContainer {

    public static class StreamLinePoint {

        private final float x;

        private final float y;

        public StreamLinePoint(float xPoint, float yPoint) {
            this.x = xPoint;
            this.y = yPoint;
        }

        public float getX() {
            return x;
        }

        public float getY() {
            return y;
        }

    }

    public final List<List<StreamLinePoint>> streamLines;

    public StreamLineContainer() {
        this(new LinkedList<List<StreamLinePoint>>());
    }

    private StreamLineContainer(List<List<StreamLinePoint>> listImpl) {
        streamLines = listImpl;
    }

    public static StreamLineContainer emptyContainer() {
        List<List<StreamLinePoint>> listImpl = Collections.emptyList();
        return new StreamLineContainer(listImpl);
    }

    public boolean addLine(List<StreamLinePoint> points) {
        return streamLines.add(points);
    }
}
