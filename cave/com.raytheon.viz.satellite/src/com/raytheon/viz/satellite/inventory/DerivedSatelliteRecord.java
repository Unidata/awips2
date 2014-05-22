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
package com.raytheon.viz.satellite.inventory;

import java.awt.Rectangle;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.measure.unit.UnitFormat;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

/**
 * Satellite record which performs derived parameter calculations to get data
 * instead of requesting directly from hdf5.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2014  2947     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DerivedSatelliteRecord extends SatelliteRecord {

    private static final long serialVersionUID = 1L;

    private final AbstractRequestableData requestableData;

    public DerivedSatelliteRecord(AbstractRequestableData requestableData)
            throws DataCubeException {
        this.requestableData = requestableData;
        setDataTime(requestableData.getDataTime());
        setPhysicalElement(requestableData.getParameter());
        setSectorID(requestableData.getSource());
        setUnits(UnitFormat.getUCUMInstance().format(requestableData.getUnit()));

        Set<SatelliteRecord> base = findBaseRecords(requestableData);
        Set<String> creatingEntities = new HashSet<String>();
        for (SatelliteRecord record : base) {
            creatingEntities.add(record.getCreatingEntity());
        }
        if (creatingEntities.size() == 1) {
            setCreatingEntity(creatingEntities.iterator().next());
        } else {
            throw new DataCubeException("Too many creating entities: "
                    + creatingEntities);
        }
        if (requestableData.getSpace() instanceof SatMapCoverage) {
            setCoverage((SatMapCoverage) requestableData.getSpace());
        } else if (requestableData.getSpace() instanceof ComparableSatMapCoverage) {
            setCoverage(((ComparableSatMapCoverage) requestableData.getSpace())
                    .getCoverage());
        }
        Rectangle[] levels = GridDownscaler
                .getDownscaleSizes(getGridGeometry());
        setInterpolationLevels(levels.length - 1);

    }

    /**
     * Perform the necessary calculations to get data that matches the provided
     * request and dataset and store the result in the messageData field.
     * 
     * @param req
     *            a {@link Request} specifying the amount of data to return
     * @param dataset
     *            indicates the interpolation level to deriv, uses the same
     *            naming convention as raw satellite data.
     * @throws DataCubeException
     */
    public void deriveMessageData(Request req, String dataset)
            throws DataCubeException {
        Rectangle[] levels = GridDownscaler
                .getDownscaleSizes(getGridGeometry());
        int level;
        for (level = 0; level < levels.length; ++level) {
            if (DataStoreFactory.createDataSetName(null,
                    SatelliteRecord.SAT_DATASET_NAME, level).equals(
                    dataset)) {
                break;
            }
        }
        if (level >= levels.length) {
            throw new DataCubeException("Unrecognized dataset: " + dataset);
        }
        GridGeometry2D gridGeometry = new GridGeometry2D(new GridEnvelope2D(
                levels[level]), getGridGeometry().getEnvelope());
        if (req.getType() == Request.Type.SLAB) {
            int[] min = req.getMinIndexForSlab();
            int[] max = req.getMaxIndexForSlab();
            GridEnvelope2D range = new GridEnvelope2D(min[0], min[1], max[0]
                    - min[0], max[1] - min[1]);
            Envelope env;
            try {
                env = gridGeometry.gridToWorld(range);
            } catch (TransformException e) {
                throw new DataCubeException(e);
            }
            gridGeometry = new GridGeometry2D(range, env);
        } else if (req.getType() != Request.Type.ALL) {
            throw new DataCubeException(
                    "Unsupported request type for derived satellite data: "
                            + req.getType());
        }
        Object dataValue = requestableData.getDataValue(gridGeometry);
        setMessageData(dataValue);
    }

    private static Set<SatelliteRecord> findBaseRecords(
            AbstractRequestableData data) {
        if (data instanceof SatelliteRequestableData) {
            SatelliteRequestableData srd = (SatelliteRequestableData) data;
            return Collections.singleton(srd.getRecord());
        } else {
            Set<SatelliteRecord> entities = new HashSet<SatelliteRecord>();
            for (AbstractRequestableData dep : data.getDependencies()) {
                entities.addAll(findBaseRecords(dep));
            }
            return entities;
        }
    }

    public String getName() {
        return requestableData.getParameterName();
    }

}
