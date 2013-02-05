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

import java.util.Comparator;
import java.util.Date;
import java.util.TreeMap;

import com.raytheon.uf.common.serialization.ISerializableObject;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@DynamicSerialize
public class FFMPVirtualGageBasin extends FFMPBasin implements
        ISerializableObject {

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

    /**
     * useful constructor
     */
    public FFMPVirtualGageBasin(String lid, Long pfaf, boolean aggregated) {
        setLid(lid);
        setPfaf(pfaf);
        setAggregated(aggregated);
        values = new TreeMap<Date, Float>(new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                // Null checks?
                return (o2.before(o1) ? -1 : (o1.equals(o2) ? 0 : 1));
            }

        });

    }

    public String toString() {

        StringBuffer buff = new StringBuffer();
        buff.append("LID: " + lid + "\n");
        buff.append("PFAF ID: " + pfaf + "\n");
        buff.append("Aggregated : " + aggregated + "\n");
        for (Date date : values.keySet()) {
            buff.append("Value : " + values.get(date) + "\n");
        }
        return buff.toString();
    }

}
