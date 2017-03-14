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
package com.raytheon.uf.edex.plugin.mpe.buildhourly;

import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.edex.plugin.mpe.MpeConstants;

/**
 * Precipitation total data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2016 5571       skorolev    Initial creation
 * Jun 24, 2016 5699       bkowal      Updated to use {@link MpeConstants}.
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class EdexPrecipTotal extends PrecipTotal {

    private String lid = null;

    private double value = MpeConstants.MISSING_VALUE;

    private int summedFlag = 0;

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public double getValue() {
        return value;
    }

    public void setValue(double value) {
        this.value = value;
    }

    public int getSummedFlag() {
        return summedFlag;
    }

    public void setSummedFlag(int summedFlag) {
        this.summedFlag = summedFlag;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(pe);
        sb.append(" ").append(ts);
        sb.append(" ").append(percentFilled);
        sb.append(" ").append(secondsCovered);
        sb.append(" ").append(total);

        return sb.toString();
    }

}
