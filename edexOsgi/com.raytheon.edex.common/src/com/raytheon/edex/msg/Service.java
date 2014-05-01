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
package com.raytheon.edex.msg;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Service to represent ArchiveSrvs
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Dec 18, 2007 561         dfitch      Initial Creation.
 *  
 * </pre>
 * 
 * @author dfitch
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Service implements ISerializableObject {

    @XmlAttribute
    @DynamicSerializeElement
    private String archiveDirectoryLocation = "";

    @XmlAttribute
    @DynamicSerializeElement
    private boolean teeModeOn = false;

    @XmlAttribute
    @DynamicSerializeElement
    private boolean jmxModeOn = false;

    @XmlAttribute
    @DynamicSerializeElement
    private String name = null;

    public String getArchiveDirectoryLocation() {
        return archiveDirectoryLocation;
    }

    public void setArchiveDirectoryLocation(String archiveDirectoryLocation) {
        this.archiveDirectoryLocation = archiveDirectoryLocation;
    }

    public boolean isTeeModeOn() {
        return teeModeOn;
    }

    public void setTeeModeOn(boolean teeModeOn) {
        this.teeModeOn = teeModeOn;
    }

    public boolean isJmxModeOn() {
        return jmxModeOn;
    }

    public void setJmxModeOn(boolean jmxModeOn) {
        this.jmxModeOn = jmxModeOn;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

}
