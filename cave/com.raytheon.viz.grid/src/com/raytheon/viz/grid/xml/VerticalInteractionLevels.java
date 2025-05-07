/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.grid.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Wrapper around a collection of {@link VerticalInteractionLevelGroup}s to
 * support XML serialization of them.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 06, 2024 2036517    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class VerticalInteractionLevels {

    @XmlElement(name = "group")
    private List<VerticalInteractionLevelGroup> groups = new ArrayList<>();

    public List<VerticalInteractionLevelGroup> getGroups() {
        return groups;
    }

    public void setGroups(List<VerticalInteractionLevelGroup> groups) {
        this.groups = groups;
    }
}
