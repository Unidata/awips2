package com.raytheon.uf.common.dataplugin.ffmp;

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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <pre>
 * Aggregate record implementation for FFMP data
 *  
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/27/13     1478        D. Hladky   Created to reduce memory and disk read/writes for FFMP
 * Jul 15, 2013 2184        dhladky     Remove all HUC's for storage except ALL
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

@DynamicSerialize
public class FFMPAggregateRecord implements ISerializableObject {

    @DynamicSerializeElement
    private String wfo;

    @DynamicSerializeElement
    private String sourceSiteDataKey;
    
    /**
     * Aggregate Record implementation for FFMP
     */
    public FFMPAggregateRecord() {
        
    }
   
    @DynamicSerializeElement
    private FFMPBasinData basins;
    
    public FFMPBasinData getBasins() {
        return basins;
    }

    public void setBasins(FFMPBasinData basins) {
        this.basins = basins;
    }

    @DynamicSerializeElement
    private List<Long> times = new ArrayList<Long>();

    /**
     * Sets the times array
     * @param times
     */
    public void setTimes(List<Long> times) {
        this.times = times;
    }

    /**
     * Gets the times array
     * @return
     */
    public List<Long> getTimes() {
        return times;
    }
    
    /**
     * WFO designator
     * @return
     */
    public String getWfo() {
        return wfo;
    }

    /**
     * Sets the WFO designator
     * @param wfo
     */
    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    /**
     * Sets the source / site / data key 
     * @param sourceSiteDataKey
     */
    public void setSourceSiteDataKey(String sourceSiteDataKey) {
        this.sourceSiteDataKey = sourceSiteDataKey;
    }

    /**
     * Gets the source / site / data key
     * @return
     */
    public String getSourceSiteDataKey() {
        return sourceSiteDataKey;
    }

  
}
