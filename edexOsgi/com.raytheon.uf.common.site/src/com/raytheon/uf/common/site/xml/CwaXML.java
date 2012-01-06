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
package com.raytheon.uf.common.site.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 22, 2009            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CwaXML implements ISerializableObject {
    @XmlAttribute(name = "id")
    private String id;
    
    @XmlElements( { @XmlElement(name = "adjId", type = String.class) })
    private ArrayList<String> adjIdList;

    @XmlElements( { @XmlElement(name = "marId", type = String.class) })
    private ArrayList<String> marIdList;

    public CwaXML() {
        
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the adjIdList
     */
    public ArrayList<String> getAdjIdList() {
        return adjIdList;
    }

    /**
     * @param adjIdList the adjIdList to set
     */
    public void setAdjIdList(ArrayList<String> adjIdList) {
        this.adjIdList = adjIdList;
    }

    /**
     * @return the marIdList
     */
    public ArrayList<String> getMarIdList() {
        return marIdList;
    }

    /**
     * @param marIdList the marIdList to set
     */
    public void setMarIdList(ArrayList<String> marIdList) {
        this.marIdList = marIdList;
    }
}
