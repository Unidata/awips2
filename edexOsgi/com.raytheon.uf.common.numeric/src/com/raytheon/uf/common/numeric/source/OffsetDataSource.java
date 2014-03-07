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

/**
 * DataSource which wraps another DataSource but offsets the coordinates by
 * constant values. This can be useful for generating a DataSource that
 * represents a subgrid of a full grid where the subgrid should be zero indexed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 04, 2014  2672     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class OffsetDataSource implements DataSource {

    protected final DataSource wrappedSource;

    protected final int xOffset;

    protected final int yOffset;

    public OffsetDataSource(DataSource wrappedSource, int xOffset, int yOffset) {
        this.wrappedSource = wrappedSource;
        this.xOffset = xOffset;
        this.yOffset = yOffset;
    }

    @Override
    public double getDataValue(int x, int y) {
        return wrappedSource.getDataValue(xOffset + x, yOffset + y);
    }

}
