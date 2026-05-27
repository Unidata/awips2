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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic;

import java.util.Calendar;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * POJO used to generically keep track of important *BiasDyn fields required for
 * the Mean Field Bias Calculation.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2016  5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class BiasMemSpanValue {

    private short memspanInd;

    private double numPairs;

    private double sumgag;

    private double sumrad;

    private double bias;

    private Calendar obsTime = TimeUtil.newGmtCalendar();

    public BiasMemSpanValue() {
    }

    public short getMemspanInd() {
        return memspanInd;
    }

    public void setMemspanInd(short memspanInd) {
        this.memspanInd = memspanInd;
    }

    public double getNumPairs() {
        return numPairs;
    }

    public void setNumPairs(double numPairs) {
        this.numPairs = numPairs;
    }

    public double getSumgag() {
        return sumgag;
    }

    public void setSumgag(double sumgag) {
        this.sumgag = sumgag;
    }

    public double getSumrad() {
        return sumrad;
    }

    public void setSumrad(double sumrad) {
        this.sumrad = sumrad;
    }

    public double getBias() {
        return bias;
    }

    public void setBias(double bias) {
        this.bias = bias;
    }

    public Calendar getObsTime() {
        return obsTime;
    }

    public void setObsTime(Calendar obsTime) {
        this.obsTime = obsTime;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("BiasMemSpanValue [");
        sb.append("memspanInd=").append(memspanInd);
        sb.append(", numPairs=").append(numPairs);
        sb.append(", sumgag=").append(sumgag);
        sb.append(", sumrad=").append(sumrad);
        sb.append(", bias=").append(bias);
        sb.append("]");
        return sb.toString();
    }
}