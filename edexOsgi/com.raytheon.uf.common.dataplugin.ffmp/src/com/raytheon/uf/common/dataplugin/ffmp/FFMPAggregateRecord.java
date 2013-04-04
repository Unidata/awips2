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
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Cache Record implementation for FFMP plugin
 * Eventually this will become a full record implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 01/27/13     1478        D. Hladky   Created to reduce memory and disk read/writes for FFMP
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

@DynamicSerialize
public class FFMPAggregateRecord implements ISerializableObject {

    
    private static final long serialVersionUID = 76774564363471L;
    
    /**
     * 
     */
    public FFMPAggregateRecord() {
        
    }
   
    @DynamicSerializeElement
    private HashMap<String, FFMPBasinData> basinsMap = new HashMap<String, FFMPBasinData>();
    
    @DynamicSerializeElement
    private List<Long> times = new ArrayList<Long>();

    public void setTimes(List<Long> times) {
        this.times = times;
    }

    public List<Long> getTimes() {
        return times;
    }

    public void setBasinsMap(HashMap<String, FFMPBasinData> basinsMap) {
        this.basinsMap = basinsMap;
    }

    public HashMap<String, FFMPBasinData> getBasinsMap() {
        return basinsMap;
    }
    
    /**
     * Add a basin Data Cache object
     * @param cacheData
     */
    public void setBasinData(FFMPBasinData cacheData) {
        basinsMap.put(cacheData.getHucLevel(), cacheData);
    }
    
    /**
     * Gets the BasinData object
     * @param huc
     * @return
     */
    public FFMPBasinData getBasinData(String huc) {
        if (basinsMap.containsKey(huc)) {
            return basinsMap.get(huc);
        }
        return null;
    }
  
}
