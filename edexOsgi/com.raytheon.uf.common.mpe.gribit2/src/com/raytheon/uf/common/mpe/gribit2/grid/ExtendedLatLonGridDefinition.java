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
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang3.StringUtils;

/**
 * Extended version of the Latitude/Longitude Grid Definition that allows for
 * the specification of the number of points in each of the 73 rows. Based on:
 * /rary.ohd.pproc.gribit/TEXT/w3fi71.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "extendedLatLonGrid")
public class ExtendedLatLonGridDefinition extends AbstractLatLonGridDefinition {

    /**
     * IGDS(91) FOR GRIDS 37-44, NUMBER OF POINTS IN EACH OF 73 ROWS.
     */
    @XmlElementWrapper(name = "numberPoints", required = true)
    @XmlElement(name = "point", required = true)
    private int[] numberPoints;

    public int[] getNumberPoints() {
        return numberPoints;
    }

    public void setNumberPoints(int[] numberPoints) {
        this.numberPoints = numberPoints;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("ExtendedLatLonGridDefinition [");
        sb.append(super.toString());
        sb.append(", numberPoints={")
                .append(StringUtils.join(numberPoints, ',')).append("}");
        sb.append("]");
        return sb.toString();
    }
}