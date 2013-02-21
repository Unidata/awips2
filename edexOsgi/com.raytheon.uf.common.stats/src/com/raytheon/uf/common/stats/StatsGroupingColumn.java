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
package com.raytheon.uf.common.stats;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.google.common.collect.Lists;

/**
 * Contains a list of groupings for statistics.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2013 1487       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement(name = "stat")
@XmlAccessorType(XmlAccessType.NONE)
public class StatsGroupingColumn {

    @XmlElement
    private List<StatsGrouping> group = Lists.newArrayList();

    /**
     * @return the group
     */
    public List<StatsGrouping> getGroup() {
        return group;
    }

    /**
     * @param group
     *            the group to set
     */
    public void setGroup(List<StatsGrouping> group) {
        this.group = group;
    }

    /**
     * Create a {@link StatsGroupingColumn} to hold the specified
     * {@link StatsGrouping} instances.
     * 
     * @param statsGroupings
     *            the groupings
     * @return the column
     */
    public static StatsGroupingColumn withGroupings(
            StatsGrouping... statsGroupings) {
        StatsGroupingColumn column = new StatsGroupingColumn();

        for (StatsGrouping grouping : statsGroupings) {
            column.group.add(grouping);
        }

        return column;
    }
}
