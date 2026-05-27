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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.gif;

import java.util.List;
import java.util.ListIterator;
import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Used to track/lookup {@link DataLevel}s to map gif renderables to a defined
 * legend.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class LegendDataLevelRange {

    private final List<DataLevel> legendDataLevels = new ArrayList<>();

    public LegendDataLevelRange() {
    }

    public void addDataLevel(final DataLevel dataLevel) {
        legendDataLevels.add(dataLevel);
    }

    public Iterator<DataLevel> getDataLevelIterator() {
        List<DataLevel> dataLevelsList = new ArrayList<>(legendDataLevels);
        return dataLevelsList.listIterator();
    }

    public Color lookupLegendColor(final double scaleFactor,
            final short value) {
        ListIterator<DataLevel> dataLevelItr = legendDataLevels.listIterator();
        Color legendColor = Color.BLACK;
        while (dataLevelItr.hasNext()) {
            DataLevel next = dataLevelItr.next();
            if (value >= (next.getBegin() * scaleFactor)) {
                legendColor = next.getLegendColor();
            } else {
                break;
            }
        }
        return legendColor;
    }
}