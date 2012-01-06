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
package com.raytheon.uf.viz.monitor.ffmp.fffg;

import com.raytheon.uf.viz.monitor.ffmp.fffg.FFMPBasins.BasinSort;

public class FFMPBasinNameIdData implements Comparable<FFMPBasinNameIdData>
{
    private Long pfaf = Long.MIN_VALUE;
    private String basinName;
    
    public FFMPBasinNameIdData(Long pfaf, String basinName)
    {
        this.pfaf = pfaf;
        this.basinName = basinName;
    }

    public Long getPfaf() {
        return pfaf;
    }

    public String getBasinName() {
        return basinName;
    }

    @Override
    public int compareTo(FFMPBasinNameIdData obj)
    {
        if (FFMPBasins.getSortBy() == BasinSort.NAME)
        {
            return this.basinName.compareTo(obj.getBasinName());
        }
        else if (FFMPBasins.getSortBy() == BasinSort.ID)
        {
            if (this.pfaf < obj.getPfaf())
            {
                return -1;
            }
            else if (this.pfaf > obj.getPfaf())
            {
                return 1;
            }
        }
        
        return 0;
    }
}
