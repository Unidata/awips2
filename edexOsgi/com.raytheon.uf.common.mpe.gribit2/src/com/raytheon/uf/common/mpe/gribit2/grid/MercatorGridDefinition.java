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
package com.raytheon.uf.common.mpe.gribit2.grid;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Mercator Grid Definition. Based on: /rary.ohd.pproc.gribit/TEXT/w3fi71.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "mercatorGrid")
public class MercatorGridDefinition extends AbstractLatLonCommon12 {

    /**
     * LATITUDE AT WHICH PROJECTION CYLINDER INTERSECTS EARTH
     */
    @XmlElement(required = true)
    private int latProjCylinderIntersect;

    /**
     * SCANNING MODE FLAGS (CODE TABLE 8)
     */
    @XmlElement(required = true)
    private int scanningModeFlag;

    public int getLatProjCylinderIntersect() {
        return latProjCylinderIntersect;
    }

    public void setLatProjCylinderIntersect(int latProjCylinderIntersect) {
        this.latProjCylinderIntersect = latProjCylinderIntersect;
    }

    public int getScanningModeFlag() {
        return scanningModeFlag;
    }

    public void setScanningModeFlag(int scanningModeFlag) {
        this.scanningModeFlag = scanningModeFlag;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("MercatorGridDefinition [");
        sb.append(super.toString());
        sb.append(", latProjCylinderIntersect=").append(
                latProjCylinderIntersect);
        sb.append(", scanningModeFlag=").append(scanningModeFlag);
        sb.append("]");
        return sb.toString();
    }
}