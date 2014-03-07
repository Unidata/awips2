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
package com.raytheon.uf.common.numeric.filter;

import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.dest.FilteredDataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.FilteredDataSource;

/**
 * A Filter which converts any occurance of a given value to Double.NaN. This is
 * most useful for a source of integer types that use a specific value to
 * represent invalid points.
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
public class FillValueFilter implements DataFilter {

    private final double fillValue;

    public FillValueFilter(double fillValue) {
        this.fillValue = fillValue;
    }

    @Override
    public double filter(double value) {
        if (value == fillValue) {
            return Double.NaN;
        }
        return fillValue;
    }

    public static FilteredDataSource apply(DataSource source, double fillValue) {
        return FilteredDataSource.addFilters(source, new FillValueFilter(
                fillValue));
    }

    public static DataDestination apply(DataDestination dest, double fillValue) {
        return FilteredDataDestination.addFilters(dest, new FillValueFilter(
                fillValue));
    }
}
