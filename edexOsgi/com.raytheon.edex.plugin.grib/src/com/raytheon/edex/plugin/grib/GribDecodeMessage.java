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
package com.raytheon.edex.plugin.grib;

import java.io.Serializable;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Message used to describe a portion of the grib file that can be decoded
 * individually.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 03, 2013  2402     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @see GribSplitter
 */
@DynamicSerialize
public class GribDecodeMessage implements Serializable {

    private static final long serialVersionUID = -8088823527599617780L;

    @DynamicSerializeElement
    private String fileName;

    @DynamicSerializeElement
    private long startPosition;

    @DynamicSerializeElement
    private long messageLength;

    @DynamicSerializeElement
    private byte gribEdition;

    @DynamicSerializeElement
    private long gridPointCount;

    public GribDecodeMessage() {

    }

    public GribDecodeMessage(String str) {
        String[] parts = str.split(":", 5);
        startPosition = Long.valueOf(parts[0]);
        messageLength = Long.valueOf(parts[1]);
        gribEdition = Byte.valueOf(parts[2]);
        gridPointCount = Byte.valueOf(parts[3]);
        fileName = parts[4];

    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public long getStartPosition() {
        return startPosition;
    }

    public void setStartPosition(long startPosition) {
        this.startPosition = startPosition;
    }

    public long getMessageLength() {
        return messageLength;
    }

    public void setMessageLength(long messageLength) {
        this.messageLength = messageLength;
    }

    public byte getGribEdition() {
        return gribEdition;
    }

    public void setGribEdition(byte gribEdition) {
        this.gribEdition = gribEdition;
    }

    public long getGridPointCount() {
        return gridPointCount;
    }

    public void setGridPointCount(long gridPointCount) {
        this.gridPointCount = gridPointCount;
    }

    public String toString() {
        return startPosition + ":" + messageLength + ":" + gribEdition + ":"
                + gridPointCount + ":" + fileName;
    }
}
