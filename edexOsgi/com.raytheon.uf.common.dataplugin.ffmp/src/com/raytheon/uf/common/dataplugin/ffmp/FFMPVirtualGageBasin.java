package com.raytheon.uf.common.dataplugin.ffmp;
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

import java.util.Collections;
import java.util.Date;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Holds VGB FFMP basin PC and PP data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02may10      3937       dhladky     Setup
 * 01/27/13     1478       dhladky     Removed un-needed XML annotations
 * Aug 08, 2015 4722       dhladky     Dynamic serialize imp not needed.
 * Aug 22, 2018 6720       njensen     Cleanup
 * 
 * </pre>
 * 
 * @author dhladky
 */
@DynamicSerialize
public class FFMPVirtualGageBasin extends FFMPBasin {

    public FFMPVirtualGageBasin() {

    }

    @DynamicSerializeElement
    public String lid;

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public FFMPVirtualGageBasin(String lid, Long pfaf, boolean aggregated) {
        setLid(lid);
        setPfaf(pfaf);
        setAggregated(aggregated);
        values = new TreeMap<>(Collections.reverseOrder());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("LID: ").append(lid).append("\n");
        sb.append("PFAF ID: ").append(pfaf).append("\n");
        sb.append("Aggregated: ").append(aggregated).append("\n");
        for (Entry<Date, Float> entry : values.entrySet()) {
            sb.append("Value : ").append(entry.getValue()).append("\n");
        }
        return sb.toString();
    }

}
