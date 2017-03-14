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
package com.raytheon.viz.grid.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2010            jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class FieldDisplayTypes implements ISerializableObject {

    @XmlAttribute
    private String key;

    @XmlAttribute
    private String displayTypes;

    @XmlAttribute
    private String volumePlaneType;

    public FieldDisplayTypes() {
    }

    public FieldDisplayTypes(FieldDisplayTypes other) {
        this.setKey(other.getKey());
        this.setDisplayTypes(other.getDisplayTypes());
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }

    public void setDisplayTypes(String displayTypes) {
        this.displayTypes = displayTypes;
    }

    public String getDisplayTypes() {
        return displayTypes;
    }

    public String getVolumePlaneType() {
        return volumePlaneType;
    }

    public void setVolumePlaneType(String volumePlaneType) {
        this.volumePlaneType = volumePlaneType;
    }

}
