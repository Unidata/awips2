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
package com.raytheon.viz.core.contours.rsc.displays;

import java.nio.FloatBuffer;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * 
 * Performs same functions as the original GriddedVectorDisplay using wireframe
 * shapes instead of svg for much faster performance. This is still slightly
 * experimental but seems to work well. It should also have the drawing code
 * extracted to a class similar to PointWindDisplay so wireframe shape barbs and
 * arrows can be used elsewhere.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 22, 2010           bsteffen    Initial creation
 * Feb 07, 2011  7948     bkowal      added a public method to get the
 *                                    direction.
 * Feb 27, 2014  2791     bsteffen    Switch from IDataRecord to DataSource
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GriddedStreamlineDisplay extends GriddedContourDisplay {

    private DataSource vSource;

    public GriddedStreamlineDisplay(IMapDescriptor descriptor,
            GridGeometry2D gridGeometry, FloatBuffer ufb, FloatBuffer vfb) {
        super(descriptor, gridGeometry, ufb);
        this.vSource = new GeographicDataSource(vfb, gridGeometry);
    }

    public GriddedStreamlineDisplay(IMapDescriptor descriptor,
            GridGeometry2D gridGeometry, DataSource ufb, DataSource vfb) {
        super(descriptor, gridGeometry, ufb);
        this.vSource = vfb;
    }

    @Override
    public DataSource[] getData() throws VizException {
        return new DataSource[] { source, vSource };
    }

}
