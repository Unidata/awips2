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

import javax.xml.bind.annotation.XmlAttribute;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2011            jsanchez     Initial creation
 * Dec  9, 2015 ASM #18209 D. Friedman  Support cwaStretch.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class DamInfoBullet extends Bullet {
    /** The coordinates of the dam threat area */
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
