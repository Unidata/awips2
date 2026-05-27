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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

/**
 * Container for the data value and associated quality code of a single HourlyPC
 * value.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2018  7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HourlyPCValue {

    private final Short value;

    private final char qc;

    public HourlyPCValue(Short value, char qc) {
        this.value = value;
        this.qc = qc;
    }

    /**
     * @return the value
     */
    public Short getValue() {
        return value;
    }

    /**
     * @return the qc
     */
    public char getQc() {
        return qc;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("HourlyPCValue [");
        sb.append("value=").append(value);
        sb.append(", qc=").append(qc);
        sb.append("]");
        return sb.toString();
    }
}