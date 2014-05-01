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

import java.util.ArrayList;
import java.util.List;

/**
 * Contains a list of the contours and their associated values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010 #4583      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class ContourContainer {
    public final List<float[]> xyContourPoints;

    public final List<Float> contourVals;

    public final List<float[]> minLabelPoints;

    public final List<float[]> maxLabelPoints;

    public final List<Float> minVals;

    public final List<Float> maxVals;

    public ContourContainer(int capacity) {
        xyContourPoints = new ArrayList<float[]>(capacity);
        contourVals = new ArrayList<Float>(capacity);
        minLabelPoints = new ArrayList<float[]>();
        maxLabelPoints = new ArrayList<float[]>();
        minVals = new ArrayList<Float>();
        maxVals = new ArrayList<Float>();
    }
}
