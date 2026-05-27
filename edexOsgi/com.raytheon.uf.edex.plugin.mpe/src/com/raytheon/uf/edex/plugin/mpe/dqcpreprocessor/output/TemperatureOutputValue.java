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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

/**
 * Container to track the current output value and the difference from the
 * synoptic hour (in minutes) for a specific synoptic hour.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class TemperatureOutputValue {

    private Double value = null;

    /*
     * In minutes.
     */
    private long diffFromSynoptic;

    /**
     * @return the value
     */
    public Double getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(Double value) {
        this.value = value;
    }

    /**
     * @return the diffFromSynoptic
     */
    public long getDiffFromSynoptic() {
        return diffFromSynoptic;
    }

    /**
     * @param diffFromSynoptic
     *            the diffFromSynoptic to set
     */
    public void setDiffFromSynoptic(long diffFromSynoptic) {
        this.diffFromSynoptic = diffFromSynoptic;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("TemperatureOutputValue [");
        sb.append("value=").append(value);
        sb.append(", diffFromSynoptic=").append(diffFromSynoptic);
        sb.append("]");
        return sb.toString();
    }
}