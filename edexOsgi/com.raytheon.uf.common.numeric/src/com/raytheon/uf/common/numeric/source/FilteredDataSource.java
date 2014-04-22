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
package com.raytheon.uf.common.numeric.source;

import java.util.Arrays;

import com.raytheon.uf.common.numeric.filter.DataFilter;

/**
 * A Source which filters the values retrieved from another source source.
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

public class FilteredDataSource implements DataSource {

    protected final DataSource wrappedSource;

    protected final DataFilter[] filters;

    protected FilteredDataSource(DataSource wrappedSource,
            DataFilter... filters) {
        this.wrappedSource = wrappedSource;
        this.filters = filters;
    }

    @Override
    public double getDataValue(int x, int y) {
        double dataValue = wrappedSource.getDataValue(x, y);
        for (DataFilter filter : filters) {
            dataValue = filter.filter(dataValue);
        }
        return dataValue;
    }

    protected DataFilter[] getFilters() {
        return filters;
    }

    protected DataSource getWrappedSource() {
        return wrappedSource;
    }

    public static FilteredDataSource addFilters(DataSource wrappedSource,
            DataFilter... filters){
        if (FilteredDataSource.class.equals(wrappedSource.getClass())) {
            FilteredDataSource oldSource = (FilteredDataSource) wrappedSource;
            DataFilter[] oldFilters = oldSource.getFilters();
            DataFilter[] newFilters = Arrays.copyOf(oldFilters, filters.length
                    + oldFilters.length);
            System.arraycopy(oldFilters, 0, newFilters, filters.length,
                    oldFilters.length);
            return new FilteredDataSource(oldSource.getWrappedSource(),
                    newFilters);
        }else{
            return new FilteredDataSource(wrappedSource, filters);
        }
    }

}
