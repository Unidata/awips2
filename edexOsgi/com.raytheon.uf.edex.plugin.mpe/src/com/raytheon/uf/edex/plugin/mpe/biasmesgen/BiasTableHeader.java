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
 * POJO representation of the bias table header.
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
public class BiasTableHeader {

    public static final int NUM_BYTES = 18;

    private short messageCode = BiasmesgenConstants.BIAS_MESSAGE_CODE;

    /**
     * The BiasTable version of the Julian Date (not a true Julian date which is
     * actually a decimal number)
     */
    private short messageDate;

    private int messageTime;

    private int messageLength = BiasmesgenConstants.BIAS_BLOCK_LENGTH
            + BiasmesgenConstants.BIAS_MIN_MSG_LENGTH;

    private short sourceId;

    private short destinationId;

    private short numberBlocks = BiasmesgenConstants.BIAS_TABLE_NUM_BLOCKS;

    public BiasTableHeader() {
    }

    public short getMessageCode() {
        return messageCode;
    }

    public void setMessageCode(short messageCode) {
        this.messageCode = messageCode;
    }

    public short getMessageDate() {
        return messageDate;
    }

    public void setMessageDate(short messageDate) {
        this.messageDate = messageDate;
    }

    public int getMessageTime() {
        return messageTime;
    }

    public void setMessageTime(int messageTime) {
        this.messageTime = messageTime;
    }

    public int getMessageLength() {
        return messageLength;
    }

    public void setMessageLength(int messageLength) {
        this.messageLength = messageLength;
    }

    public short getSourceId() {
        return sourceId;
    }

    public void setSourceId(short sourceId) {
        this.sourceId = sourceId;
    }

    public short getDestinationId() {
        return destinationId;
    }

    public void setDestinationId(short destinationId) {
        this.destinationId = destinationId;
    }

    public short getNumberBlocks() {
        return numberBlocks;
    }

    public void setNumberBlocks(short numberBlocks) {
        this.numberBlocks = numberBlocks;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("BiasTableHeader [");
        sb.append("messageCode=").append(messageCode);
        sb.append(", messageDate=").append(messageDate);
        sb.append(", messageTime=").append(messageTime);
        sb.append(", messageLength=").append(messageLength);
        sb.append(", sourceId=").append(sourceId);
        sb.append(", destinationId=").append(destinationId);
        sb.append(", numberBlocks=").append(numberBlocks);
        sb.append("]");
        return sb.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + destinationId;
        result = prime * result + messageCode;
        result = prime * result + messageDate;
        result = prime * result + messageLength;
        result = prime * result + messageTime;
        result = prime * result + numberBlocks;
        result = prime * result + sourceId;
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
        BiasTableHeader other = (BiasTableHeader) obj;
        if (destinationId != other.destinationId) {
            return false;
        }
        if (messageCode != other.messageCode) {
            return false;
        }
        if (messageDate != other.messageDate) {
            return false;
        }
        if (messageLength != other.messageLength) {
            return false;
        }
        if (messageTime != other.messageTime) {
            return false;
        }
        if (numberBlocks != other.numberBlocks) {
            return false;
        }
        if (sourceId != other.sourceId) {
            return false;
        }
        return true;
    }
}