package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SourceBinList implements ISerializableObject {
    
    /** sourceName and dataKey **/
    @DynamicSerializeElement
    @XmlElement
    public String sourceId;
    
    @DynamicSerializeElement
    @XmlElement
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
