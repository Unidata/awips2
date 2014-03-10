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

import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.FilteredDataSource;

/**
 * 
 * Converts a source providing regular short or byte data to a source providing
 * unsigned versions of the same data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 06, 2014  2791     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class UnsignedFilter implements DataFilter {

    private final int mask;
    
    private UnsignedFilter(int mask) {
        this.mask = mask;
    }

    @Override
    public double filter(double value) {
        return ((int) value) & mask;
    }

    public static DataSource apply(ByteBufferWrapper byteData) {
        return FilteredDataSource
                .addFilters(byteData, new UnsignedFilter(0xFF));
    }

    public static DataSource apply(ShortBufferWrapper shortData) {
        return FilteredDataSource.addFilters(shortData, new UnsignedFilter(
                0xFFFF));
    }

}