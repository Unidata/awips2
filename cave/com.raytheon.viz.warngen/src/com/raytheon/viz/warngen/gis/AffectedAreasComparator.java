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
import java.util.Comparator;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2011            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class AffectedAreasComparator implements Comparator<AffectedAreas> {

    private enum Sort {
        NAME, PARENT, SIZE, PART_OF_AREA, PART_OF_PARENT, AREA_NOTATION, FIPS
    }

    private ArrayList<Sort> list;

    private int counter;

    public AffectedAreasComparator(ArrayList<String> fields) {
        counter = 0;
        list = new ArrayList<Sort>();
        for (String field : fields) {
            if (field.equalsIgnoreCase("name")) {
                list.add(Sort.NAME);
            } else if (field.equalsIgnoreCase("size")) {
                list.add(Sort.SIZE);
            } else if (field.equalsIgnoreCase("parent")) {
                list.add(Sort.PARENT);
            } else if (field.equalsIgnoreCase("partOfArea")) {
                list.add(Sort.PART_OF_AREA);
            } else if (field.equalsIgnoreCase("partOfParent")) {
                list.add(Sort.PART_OF_PARENT);
            } else if (field.equalsIgnoreCase("areaNotation")) {
                list.add(Sort.AREA_NOTATION);
            } else if (field.equalsIgnoreCase("fips")) {
                list.add(Sort.FIPS);
            }
        }
    }

    public int compare(AffectedAreas a1, AffectedAreas a2) {
        if (list.isEmpty()) {
            return a1.name.compareTo(a2.name);
        }

        int value = 0;
        switch (list.get(counter)) {
        case NAME:
            value = a1.name.compareTo(a2.name);
            break;
        case PARENT:
            value = a1.parentRegion.compareTo(a2.parentRegion);
            break;
        case AREA_NOTATION:
            value = a1.areaNotation.compareTo(a2.areaNotation);
            break;
        case FIPS:
            value = a1.fips.compareTo(a2.fips);
            break;
        case PART_OF_AREA:
            String s1 = "";
            for (String s : a1.partOfArea) {
                if (s.equalsIgnoreCase("EXTREME") == false)
                    s1 += (s + " ");
            }

            String s2 = "";
            for (String s : a2.partOfArea) {
                if (s.equalsIgnoreCase("EXTREME") == false)
                    s2 += (s + " ");
            }

            value = s1.trim().compareTo(s2.trim());
            break;
        case PART_OF_PARENT:
            String p1 = "";
            for (String s : a1.partOfParentRegion) {
                if (s.equalsIgnoreCase("EXTREME") == false)
                    p1 += (s + " ");
            }

            String p2 = "";
            for (String s : a2.partOfParentRegion) {
                if (s.equalsIgnoreCase("EXTREME") == false)
                    p2 += (s + " ");
            }

            value = p1.trim().compareTo(p2.trim());
            break;
        }

        if (value == 0 && counter + 1 < list.size()) {
            counter++;
            value = this.compare(a1, a2);
        }
        counter = 0;

        return value;
    }
}
