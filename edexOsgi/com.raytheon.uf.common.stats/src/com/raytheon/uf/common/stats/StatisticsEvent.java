package com.raytheon.uf.common.stats;

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

import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.event.Event;

/**
 * Event used for statistics
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2012  #1340     dhladky     Initial creation
 * Feb 10, 2013  #1584     mpduff      Add equals and hashCode.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class StatisticsEvent extends Event {

    private static final long serialVersionUID = 1L;

    public Set<String> getFields() {
        return getFieldUnitMap().keySet();
    }

    protected abstract Map<String, String> getFieldUnitMap();

    public abstract void finalizeEvent();

    public String getStorageUnit(String field) {
        return getFieldUnitMap().get(field);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        HashCodeBuilder hcBuilder = new HashCodeBuilder();
        hcBuilder.append(this.date);
        hcBuilder.append(this.id);

        return hcBuilder.toHashCode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof StatisticsEvent) {
            StatisticsEvent other = (StatisticsEvent) obj;
            EqualsBuilder builder = new EqualsBuilder();
            builder.append(this.date, other.date);
            builder.append(this.id, this.id);

            return builder.isEquals();
        }

        return super.equals(obj);
    }

}
