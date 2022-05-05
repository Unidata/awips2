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
package com.raytheon.uf.common.dataplugin.warning.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Mar 24, 2011           jsanchez     Initial creation
 * Dec 09, 2015  18209    D. Friedman  Support cwaStretch.
 * Aug 15, 2017  6328     randerso     Renamed to PresetInfoBullet
 *
 * </pre>
 *
 * @author jsanchez
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PresetInfoBullet extends Bullet {
    /** The coordinates of the preset threat area */
    @XmlAttribute
    private String coords;

    @XmlAttribute
    private boolean cwaStretch;

    public String getCoords() {
        return coords;
    }

    public void setCoords(String coords) {
        this.coords = coords;
    }

    public boolean isCwaStretch() {
        return cwaStretch;
    }

    public void setCwaStretch(boolean cwaStretch) {
        this.cwaStretch = cwaStretch;
    }
}
