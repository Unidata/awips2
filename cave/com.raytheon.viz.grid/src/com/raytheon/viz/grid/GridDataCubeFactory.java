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
package com.raytheon.viz.grid;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.AbstractDataCubeFactory;
import com.raytheon.uf.viz.core.datastructure.CubeUtil;
import com.raytheon.uf.viz.core.datastructure.DataCube;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.style.level.SingleLevel;

/**
 * Factory for creating data cubes from GribRecords
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GridDataCubeFactory extends AbstractDataCubeFactory {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.datastructure.AbstractDataCubeFactory#construct
     * (com.raytheon.edex.db.objects.PluginDataObject[])
     */
    @Override
    public DataCube construct(PluginDataObject[] pdos, DataCube.Mode mode)
            throws VizDataCubeException {
        GribRecord[] grids = new GribRecord[pdos.length];
        String parameter = null;
        ISpatialObject spatial = null;
        Unit<?> units = null;
        for (int i = 0; i < grids.length; i++) {
            grids[i] = (GribRecord) pdos[i];
            if (i == 0) {
                parameter = grids[i].getModelInfo().getParameterName();
                spatial = (grids[i].getSpatialObject());
                units = grids[i].getModelInfo().getParameterUnitObject();
            } else {
                if (!grids[i].getModelInfo().getParameterName().equals(
                        parameter)) {
                    throw new VizDataCubeException(
                            "Only one parameter allowed per data cube.");
                }
            }
        }

        DataCube cube = new DataCube("grib", mode);
        cube.setSpatialArea(spatial);
        cube.setParameter(parameter);
        cube.setUnits(units);

        for (GribRecord rec : grids) {
            SingleLevel level = GridLevelTranslator.construct(rec
                    .getModelInfo().getLevelName());
            level.setValue(rec.getModelInfo().getLevelOneValue());
            DataTime time = rec.getDataTime();
            if (mode == DataCube.Mode.ON_DEMAND) {
                cube.addData(time, level, rec);
            } else if (mode == DataCube.Mode.EAGER) {
                IDataRecord dr = null;
                try {
                    dr = CubeUtil.retrieveData(rec, "grib");
                } catch (VizException e) {
                    throw new VizDataCubeException(
                            "Error retrieving data to form cube.", e);
                }
                if (dr != null) {
                    float[] data = (float[]) dr.getDataObject();
                    rec.setMessageData(data);
                    cube.addData(time, level, rec);
                }
            }
        }
        cube.initGeometry();
        return cube;
    }
}
