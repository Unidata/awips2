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

import java.util.HashMap;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;


/**
 * FFMP source binning object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/22/10      6581       D. Hladky   Initial release
 * 01/27/13     1478        D. Hladky   Removed un needed XML annotations
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@DynamicSerialize
public class SourceBinList implements ISerializableObject {
    
    /** sourceName and dataKey **/
    @DynamicSerializeElement
    public String sourceId;
    
    @DynamicSerializeElement
    public HashMap<Long, SourceBin> sourceMap;
    
    public SourceBinList() {
        
    }
    
    public SourceBinList(String sourceId) {
        this.sourceId = sourceId;
        this.sourceMap = new HashMap<Long, SourceBin>();
    }

    public String getSourceId() {
        return sourceId;
    }

    public void setSourceId(String sourceId) {
        this.sourceId = sourceId;
    }

    public HashMap<Long, SourceBin> getSourceMap() {
        return sourceMap;
    }

    public void setSourceMap(HashMap<Long, SourceBin> sourceMap) {
        this.sourceMap = sourceMap;
    }
    
    public void addBin(Long pfaf, SourceBin map) {
        getSourceMap().put(pfaf, map);
    }
    
    public SourceBin getMap(Long pfaf) {
        return getSourceMap().get(pfaf);
    }

}
