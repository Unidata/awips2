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

/**
 * Twelve (12) attributes associated with a Latitude/Longitude grid that are
 * common across multiple grid definitions. Only two attributes are specified in
 * this class; however, this class builds on {@link AbstractLatLonCommon10}.
 * Based on: /rary.ohd.pproc.gribit/TEXT/w3fi71.f
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
public abstract class AbstractLatLonCommon12 extends AbstractLatLonCommon10 {

    /**
     * LATITUDE INCREMENT
     */
    @XmlElement(required = true)
    private int incrementLat;

    /**
     * LONGITUDE INCREMENT
     */
    @XmlElement(required = true)
    private int incrementLon;

    public int getIncrementLat() {
        return incrementLat;
    }

    public void setIncrementLat(int incrementLat) {
        this.incrementLat = incrementLat;
    }

    public int getIncrementLon() {
        return incrementLon;
    }

    public void setIncrementLon(int incrementLon) {
        this.incrementLon = incrementLon;
    }

    @Override
    public String toString() {
        /*
         * The class name and opening, closing brackets are excluded in this
         * toString method because this method should ideally only be used
         * directly by a subclass.
         */
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append(", incrementLat=").append(incrementLat);
        sb.append(", incrementLon=").append(incrementLon);
        return sb.toString();
    }
}