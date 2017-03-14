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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

/**
 * POJO representation of a bias table data row. Bias Table Rows are included in
 * a Bias Table file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2016 5576       bkowal      Initial creation
 * May 20, 2016 5576       bkowal      Remove default constructor.
 * Jun 13, 2016 5576       bkowal      Added {@link #toString()}, {@link #equals(Object)},
 *                                     and {@link #hashCode()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasTableRow {

    public static final int NUM_BYTES = 20;

    private int memorySpan;

    private int numPairs;

    private int sumGag;

    private int sumRad;

    private int bias;

    public BiasTableRow(int memorySpan, int numPairs, int sumGag, int sumRad,
            int bias) {
        this.memorySpan = memorySpan;
        this.numPairs = numPairs;
        this.sumGag = sumGag;
        this.sumRad = sumRad;
        this.bias = bias;
    }

    public int getMemorySpan() {
        return memorySpan;
    }

    public void setMemorySpan(int memorySpan) {
        this.memorySpan = memorySpan;
    }

    public int getNumPairs() {
        return numPairs;
    }

    public void setNumPairs(int numPairs) {
        this.numPairs = numPairs;
    }

    public int getSumGag() {
        return sumGag;
    }

    public void setSumGag(int sumGag) {
        this.sumGag = sumGag;
    }

    public int getSumRad() {
        return sumRad;
    }

    public void setSumRad(int sumRad) {
        this.sumRad = sumRad;
    }

    public int getBias() {
        return bias;
    }

    public void setBias(int bias) {
        this.bias = bias;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("BiasTableRow [");
        sb.append("memorySpan=").append(memorySpan);
        sb.append(", numPairs=").append(numPairs);
        sb.append(", sumGag=").append(sumGag);
        sb.append(", sumRad=").append(sumRad);
        sb.append(", bias=").append(bias);
        sb.append("]");
        return sb.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + bias;
        result = prime * result + memorySpan;
        result = prime * result + numPairs;
        result = prime * result + sumGag;
        result = prime * result + sumRad;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        BiasTableRow other = (BiasTableRow) obj;
        if (bias != other.bias) {
            return false;
        }
        if (memorySpan != other.memorySpan) {
            return false;
        }
        if (numPairs != other.numPairs) {
            return false;
        }
        if (sumGag != other.sumGag) {
            return false;
        }
        if (sumRad != other.sumRad) {
            return false;
        }
        return true;
    }
}