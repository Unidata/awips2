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
 * POJO representation of the bias table sub-header.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2016 5576       bkowal      Initial creation
 * May 20, 2016 5576       bkowal      Pre-initialize constant values.
 * Jun 13, 2016 5576       bkowal      Added {@link #toString()}, {@link #equals(Object)},
 *                                     and {@link #hashCode()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasTableSubHeader {

    public static final int STRING_LENGTH = 4;

    public static final int NUM_BYTES = 8 + (STRING_LENGTH * 2);

    private short divider = BiasmesgenConstants.BIAS_BLOCK_DIVIDER;

    private short blockId = BiasmesgenConstants.BIAS_BLOCK_ID;

    private short version = BiasmesgenConstants.BIAS_VERSION;

    private short blockLength = BiasmesgenConstants.BIAS_BLOCK_LENGTH;

    private String biasSource;

    private String radarId;

    public short getDivider() {
        return divider;
    }

    public void setDivider(short divider) {
        this.divider = divider;
    }

    public short getBlockId() {
        return blockId;
    }

    public void setBlockId(short blockId) {
        this.blockId = blockId;
    }

    public short getVersion() {
        return version;
    }

    public void setVersion(short version) {
        this.version = version;
    }

    public short getBlockLength() {
        return blockLength;
    }

    public void setBlockLength(short blockLength) {
        this.blockLength = blockLength;
    }

    public String getBiasSource() {
        return biasSource;
    }

    public void setBiasSource(String biasSource) {
        this.biasSource = biasSource;
    }

    public String getRadarId() {
        return radarId;
    }

    public void setRadarId(String radarId) {
        this.radarId = radarId;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("BiasTableSubHeader [");
        sb.append("divider=").append(divider);
        sb.append(", blockId=").append(blockId);
        sb.append(", version=").append(version);
        sb.append(", blockLength=").append(blockLength);
        sb.append(", biasSource=").append(biasSource);
        sb.append(", radarId=").append(radarId);
        sb.append("]");
        return sb.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((biasSource == null) ? 0 : biasSource.hashCode());
        result = prime * result + blockId;
        result = prime * result + blockLength;
        result = prime * result + divider;
        result = prime * result + ((radarId == null) ? 0 : radarId.hashCode());
        result = prime * result + version;
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
        BiasTableSubHeader other = (BiasTableSubHeader) obj;
        if (biasSource == null) {
            if (other.biasSource != null) {
                return false;
            }
        } else if (!biasSource.equals(other.biasSource)) {
            return false;
        }
        if (blockId != other.blockId) {
            return false;
        }
        if (blockLength != other.blockLength) {
            return false;
        }
        if (divider != other.divider) {
            return false;
        }
        if (radarId == null) {
            if (other.radarId != null) {
                return false;
            }
        } else if (!radarId.equals(other.radarId)) {
            return false;
        }
        if (version != other.version) {
            return false;
        }
        return true;
    }
}