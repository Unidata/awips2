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
package com.raytheon.viz.lightning.cache;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.viz.lightning.RepeatingBinOffset;

/**
 * Time and record data used to create a LightningFrame
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 9, 2014  3333       bclement     moved from LightningResource
 * Mar 05, 2015 4233       bsteffen     include source in cache key.
 * Sep 25, 2015 4605       bsteffen     repeat binning
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class LightningFrameMetadata {

    private final String source;

    private final RepeatingBinOffset offset;

    private final DataTime frameTime;

    private final List<BinLightningRecord> newRecords = new ArrayList<BinLightningRecord>();

    private final List<BinLightningRecord> processed = new ArrayList<BinLightningRecord>();

    public LightningFrameMetadata(String source, DataTime frameTime,
            RepeatingBinOffset offset) {
        this.source = source;
        this.frameTime = frameTime;
        this.offset = offset;
    }

    public boolean contains(DataTime time) {
        return offset.getNormalizedTimes(time).contains(frameTime);
    }

    public String getSource() {
        return source;
    }

    /**
     * @return the offset
     */
    public RepeatingBinOffset getOffset() {
        return offset;
    }

    /**
     * @return the frameTime
     */
    public DataTime getFrameTime() {
        return frameTime;
    }

    /**
     * @return the newRecords
     */
    public List<BinLightningRecord> getNewRecords() {
        return newRecords;
    }

    public boolean hasRecords() {
        return !newRecords.isEmpty() || !processed.isEmpty();
    }

    /**
     * @return the processed
     */
    public List<BinLightningRecord> getProcessed() {
        return processed;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((frameTime == null) ? 0 : frameTime.hashCode());
        result = prime * result + ((offset == null) ? 0 : offset.hashCode());
        result = prime * result + ((source == null) ? 0 : source.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        LightningFrameMetadata other = (LightningFrameMetadata) obj;
        if (frameTime == null) {
            if (other.frameTime != null)
                return false;
        } else if (!frameTime.equals(other.frameTime))
            return false;
        if (offset == null) {
            if (other.offset != null)
                return false;
        } else if (!offset.equals(other.offset))
            return false;
        if (source == null) {
            if (other.source != null)
                return false;
        } else if (!source.equals(other.source))
            return false;
        return true;
    }
}
