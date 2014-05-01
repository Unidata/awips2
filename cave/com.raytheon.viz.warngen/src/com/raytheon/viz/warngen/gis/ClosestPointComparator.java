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
package com.raytheon.viz.warngen.gis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 3, 2011            jsanchez     Initial creation
 * Sep 25, 2012 15425     Qinglu Lin   Implemented sorting on 'gid' in ascending order.
 * Oct 17, 2012           jsanchez     Refactored the enum sort to be more flexible.
 * Feb 14, 2013 1605      jsanchez     Updated the distance comparison for more accuracy.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class ClosestPointComparator implements Comparator<ClosestPoint> {

    public static enum Sort {
        NAME, POPULATION, DISTANCE, WARNGENLEV, LAT, LON, AREA, PARENTAREA, GID
    }

    private final ArrayList<Sort> list;

    private int counter;

    public ClosestPointComparator() {
        counter = 0;
        list = new ArrayList<Sort>();
    }

    public ClosestPointComparator(List<String> fields) {
        counter = 0;
        list = new ArrayList<Sort>();
        for (String field : fields) {
            if (field.equalsIgnoreCase(Sort.NAME.toString())) {
                list.add(Sort.NAME);
            } else if (field.equalsIgnoreCase(Sort.POPULATION.toString())) {
                list.add(Sort.POPULATION);
            } else if (field.equalsIgnoreCase(Sort.DISTANCE.toString())) {
                list.add(Sort.DISTANCE);
            } else if (field.equalsIgnoreCase(Sort.WARNGENLEV.toString())
                    || field.equalsIgnoreCase("watch_warn")) {
                list.add(Sort.WARNGENLEV);
            } else if (field.equalsIgnoreCase(Sort.LAT.toString())) {
                list.add(Sort.LAT);
            } else if (field.equalsIgnoreCase(Sort.LON.toString())) {
                list.add(Sort.LON);
            } else if (field.equalsIgnoreCase(Sort.AREA.toString())) {
                list.add(Sort.AREA);
            } else if (field.equalsIgnoreCase(Sort.PARENTAREA.toString())) {
                list.add(Sort.PARENTAREA);
            } else if (field.equalsIgnoreCase(Sort.GID.toString())) {
                list.add(Sort.GID);
            }
        }
    }

    public ClosestPoint[] combine(ClosestPoint[] array1, ClosestPoint[] array2,
            String... fields) {

        ClosestPoint[] points = null;
        if (array1 != null && array2 != null) {
            points = (ClosestPoint[]) ArrayUtils.addAll(array1, array2);
        } else if (array1 != null && array2 == null) {
            points = array1;
        } else if (array1 == null && array2 != null) {
            points = array2;
        } else {
            return null;
        }

        counter = 0;
        list.clear();

        ClosestPointComparator comparator = new ClosestPointComparator(
                Arrays.asList(fields));
        Collections.sort(Arrays.asList(points), comparator);

        return points;
    }

    @Override
    public int compare(ClosestPoint cp1, ClosestPoint cp2) {
        if (list.isEmpty()) {
            return cp1.compareTo(cp2);
        }

        int value = 0;
        switch (list.get(counter)) {
        case NAME:
            if (cp1.name.matches("((-|\\+)?[0-9]+(\\.[0-9]+)?)+")
                    && cp2.name.matches("((-|\\+)?[0-9]+(\\.[0-9]+)?)+")) {
                value = Double.valueOf(cp1.name).compareTo(
                        Double.valueOf(cp2.name));
            } else {
                value = cp1.name.compareTo(cp2.name);
            }
            break;
        case POPULATION:
            value = -1 * Double.compare(cp1.population, cp2.population);
            break;
        case WARNGENLEV:
            value = Double.compare(cp1.warngenlev, cp2.warngenlev);
            break;
        case LAT:
            value = Double.compare(cp1.point.y, cp2.point.y);
            break;
        case LON:
            value = Double.compare(cp1.point.x, cp2.point.x);
            break;
        case AREA:
            value = cp1.area.compareTo(cp2.area);
            break;
        case PARENTAREA:
            value = cp1.parentArea.compareTo(cp2.parentArea);
            break;
        case DISTANCE:
            value = new Double(cp1.distance).compareTo(cp2.distance);
            break;
        case GID:
            value = new Integer(cp1.gid).compareTo(cp2.gid);
            break;
        }

        if (value == 0 && counter + 1 < list.size()) {
            counter++;
            value = this.compare(cp1, cp2);
        }
        counter = 0;
        return value;
    }
}
