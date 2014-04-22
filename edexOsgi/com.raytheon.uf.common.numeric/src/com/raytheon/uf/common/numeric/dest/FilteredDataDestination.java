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
package com.raytheon.uf.common.numeric.dest;

import java.util.Arrays;

import com.raytheon.uf.common.numeric.filter.DataFilter;

/**
 * A Destination which filters the input values before applying then to another
 * destination.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 06, 2014  2791     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class FilteredDataDestination implements DataDestination {

    protected final DataDestination wrappedDestination;

    protected final DataFilter[] filters;

    protected FilteredDataDestination(DataDestination wrappedDestination,
            DataFilter... filters) {
        this.wrappedDestination = wrappedDestination;
        this.filters = filters;
    }

    @Override
    public void setDataValue(double dataValue, int x, int y) {
        for (DataFilter filter : filters) {
            dataValue = filter.filter(dataValue);
        }
        wrappedDestination.setDataValue(dataValue, x, y);
    }

    public DataDestination getWrappedDestination() {
        return wrappedDestination;
    }

    public DataFilter[] getFilters() {
        return filters;
    }

    public static FilteredDataDestination addFilters(
            DataDestination wrappedDestination, DataFilter... filters) {
        if (FilteredDataDestination.class.equals(wrappedDestination.getClass())) {
            FilteredDataDestination oldDest = (FilteredDataDestination) wrappedDestination;
            DataFilter[] oldFilters = oldDest.getFilters();
            DataFilter[] newFilters = Arrays.copyOf(oldFilters, filters.length
                    + oldFilters.length);
            System.arraycopy(oldFilters, 0, newFilters, filters.length,
                    oldFilters.length);
            return new FilteredDataDestination(oldDest.getWrappedDestination(),
                    newFilters);
        } else {
            return new FilteredDataDestination(wrappedDestination, filters);
        }
    }

}
