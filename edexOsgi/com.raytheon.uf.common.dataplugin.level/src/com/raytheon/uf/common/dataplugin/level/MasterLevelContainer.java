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
package com.raytheon.uf.common.dataplugin.level;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Container for holding multiple master levels during serialization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 04, 201            bsteffen    Initial creation.
 * Jan 23, 2014  2711     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "masterLevelContainer")
public class MasterLevelContainer {

    @XmlElement(name = "masterLevel")
    @DynamicSerializeElement
    private List<MasterLevel> masterlevels;

    public MasterLevelContainer() {
        masterlevels = new ArrayList<MasterLevel>();
    }

    public MasterLevelContainer(int initialSize) {
        masterlevels = new ArrayList<MasterLevel>(initialSize);
    }

    public List<MasterLevel> getMasterLevels() {
        return masterlevels;
    }

    public void setMasterLevels(List<MasterLevel> masterLevels) {
        this.masterlevels = masterLevels;
    }

    public void add(MasterLevel masterLevel) {
        masterlevels.add(masterLevel);
    }
}
