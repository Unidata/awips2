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

import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2010            bsteffen     Initial creation
 * Feb 07, 2011 7948       bkowal       added a public method to get
 *                                      the direction.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GriddedStreamlineDisplay extends GriddedContourDisplay {

    private FloatBuffer vfb;

    public GriddedStreamlineDisplay(IMapDescriptor descriptor,
            GridGeometry2D gridGeometry, FloatBuffer ufb, FloatBuffer vfb) {
        super(descriptor, gridGeometry, ufb);
        this.vfb = vfb;
    }

    @Override
    public IDataRecord[] getData() throws VizException {
        FloatDataRecord ufdr = new FloatDataRecord("uData", "", fb.array(), 2,
                new long[] { gridGeometry.getGridRange2D().width,
                        gridGeometry.getGridRange2D().height });
        FloatDataRecord vfdr = new FloatDataRecord("vData", "", vfb.array(), 2,
                new long[] { gridGeometry.getGridRange2D().width,
                        gridGeometry.getGridRange2D().height });
        return new IDataRecord[] { ufdr, vfdr };
    }

}
