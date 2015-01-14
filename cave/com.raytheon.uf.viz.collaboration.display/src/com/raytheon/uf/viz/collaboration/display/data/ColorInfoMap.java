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
package com.raytheon.uf.viz.collaboration.display.data;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Contains foreground and background chat colors for a list of users or sites
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov 13, 2014 3709        mapeters    Initial creation.
 * Nov 26, 2014 3709        mapeters    Renamed from UserColorInformation, added fgSet getter.
 * Dec 08, 2014 3709        mapeters    Removed fgSet and individual colors' getters/setters, 
 *                                      set foreground and background together.
 * Jan 13, 2015 3709        bclement    moved from collaboration.ui to collaboration.display
 *                                      moved ColorInfo class to UserColorInfo
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ColorInfoMap {

    @XmlElement
    private Map<String, UserColorInfo> colors;

    /**
     * 
     */
    public ColorInfoMap() {
    }

    /**
     * @param colors
     */
    public ColorInfoMap(Map<String, UserColorInfo> colors) {
        this.colors = colors;
    }

    /**
     * @return the colors
     */
    public Map<String, UserColorInfo> getColors() {
        return colors;
    }

    /**
     * @param colors
     *            the colors to set
     */
    public void setColors(Map<String, UserColorInfo> colors) {
        this.colors = colors;
    }

}
