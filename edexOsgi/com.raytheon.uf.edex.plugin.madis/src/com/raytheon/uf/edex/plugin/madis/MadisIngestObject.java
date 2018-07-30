package com.raytheon.uf.edex.plugin.madis;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Madis ingest object
 * <pre>
 *                     
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 5/18/13       753         dhladky    Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MadisIngestObject implements ISerializableObject {
    
    @DynamicSerializeElement
    public List<String> lines = new ArrayList<String>();
    
    @DynamicSerializeElement
    public String[] header;
    
    public MadisIngestObject() {
        
    }

    public MadisIngestObject(String[] header) {

        this.header = header;
    }

    public List<String> getLines() {
        return lines;
    }

    public void setLines(List<String> lines) {
        this.lines = lines;
    }

    public String[] getHeader() {
        return header;
    }

    // set the header 
    public void setHeader(String[] header) {
        this.header = header;
    }
    
    // build the list of lines
    public void addLine(String line) {

        lines.add(line);
    }

}
