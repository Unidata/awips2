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

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.core.contours.ContourRenderable;

/**
 * Displays contours from any data source
 * 
 * Currently implemented using the D2D contouring capability
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 11, 2008  1233     chammack    Initial creation
 * Feb 27, 2014  2791     bsteffen    Switch from IDataRecord to DataSource
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GriddedContourDisplay extends ContourRenderable {

    private ContourPreferences preferences;

    protected GridGeometry2D gridGeometry;

    protected DataSource source;

    public GriddedContourDisplay(IMapDescriptor descriptor,
            final GridGeometry2D gridGeometry, final FloatBuffer fb) {
        this(descriptor, gridGeometry, new GeographicDataSource(fb,
                gridGeometry));
    }

    public GriddedContourDisplay(IMapDescriptor descriptor,
            final GridGeometry2D gridGeometry, final DataSource source) {
        super(descriptor);
        this.gridGeometry = gridGeometry;
        this.source = source;
    }

    @Override
    public DataSource[] getData() throws VizException {
        return new DataSource[] { source };
    }

    @Override
    public GeneralGridGeometry getGridGeometry() throws VizException {
        return gridGeometry;
    }

    @Override
    public ContourPreferences getPreferences() throws VizException {
        return preferences;
    }

    /**
     * @param preferences
     *            the preferences to set
     */
    public void setPreferences(ContourPreferences preferences) {
        this.preferences = preferences;
    }

}
