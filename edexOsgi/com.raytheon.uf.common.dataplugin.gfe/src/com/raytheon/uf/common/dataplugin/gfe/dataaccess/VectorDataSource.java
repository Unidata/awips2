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
package com.raytheon.uf.common.dataplugin.gfe.dataaccess;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * 
 * Specialized {@link DataSource} that can be used by {@link GFEGridFactory} for
 * vector parameters. This source will only return data for the magnitude of the
 * vector but it exposes both the magnitude and direction separately so that the
 * factory can extract the direction direction to be return it as an independent
 * grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 23, 2016  5637     bsteffen  Initial creation
 * Nov 07, 2016  5991     bsteffen  Fix direction.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class VectorDataSource implements DataSource {

    private final DataSource magSource;

    private final DataSource dirSource;

    public VectorDataSource(VectorGridSlice slice) {
        GridLocation loc = slice.getGridInfo().getGridLoc();
        Grid2DFloat mag = slice.getScalarGrid();
        magSource = new FloatBufferWrapper(mag.getFloats(), loc.getNx(),
                loc.getNy());
        Grid2DFloat dir = slice.getDirGrid();
        dirSource = new FloatBufferWrapper(dir.getFloats(), loc.getNx(),
                loc.getNy());
    }

    public DataSource getMagSource() {
        return magSource;
    }

    public DataSource getDirSource() {
        return dirSource;
    }

    @Override
    public double getDataValue(int x, int y) {
        return magSource.getDataValue(x, y);
    }

}