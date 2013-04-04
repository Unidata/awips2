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
package com.raytheon.viz.radar.textcontributors;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;


/**
 * 
 * Format GFM detection count for the upper text
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/05/2013   DCS51      zwang       Initial creation
 * 
 * </pre>
 * 
 * @author n
 * @version 1.0
 */
public class GfmTextContributor implements IRadarTextContributor {

    @Override
    public String contributeText(RadarRecord record) {
         
    	// Get the count of the GFM 
    	int gfmCount = 0;
    	// One GFM detect ID has 3 Forecast time: 0, 10 minutes, 20 minutes
        List<String> stormIDList = record.getStormIDList();
        
        if (stormIDList.size() == 0) {
        	return "GF detection count: " + gfmCount;
        }
        
        Set<String> gfmIDSet = new HashSet<String> ();

        // get GFM IDs
        // GFM storm ID is in format gfmID:forecastTime (1:10)
        for (int i=0; i<stormIDList.size(); i++) {
            String[] temp = stormIDList.get(i).split(":");
            gfmIDSet.add(temp[0]);
        }

        return "GF detection count: " + gfmIDSet.size();
    }
}
