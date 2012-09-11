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
package com.raytheon.viz.grid.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.core.rsc.VizGroupResourceData;


/**
 * FFG Resource Data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2011            mpduff      Initial creation.
 * Sep 11, 2012   1162     mpduff      Override getAvailableTimes method.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class FfgVizGroupResourceData extends VizGroupResourceData {
    public FfgVizGroupResourceData() {
        // Make name generator here
        nameGenerator = new FfgGridNameGenerator();
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        Set<DataTime> baseTimes = new HashSet<DataTime>();
        Iterator<ResourcePair> rpIter = resourceList.iterator();
        super.mergeMetadataMap();

        List<DataTime> availableTimes = new ArrayList<DataTime>();

        while (rpIter.hasNext()) {
            ResourcePair rp = rpIter.next();

            if (rp.getResourceData() instanceof AbstractRequestableResourceData) {
                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) rp
                        .getResourceData();
                Collection<DataTime> times = Arrays.asList(arrd
                        .getAvailableTimes());

                baseTimes.addAll(times);
            }
        }

        availableTimes.addAll(baseTimes);
        Collections.sort(availableTimes);

        return availableTimes.toArray(new DataTime[availableTimes.size()]);
    }
}
