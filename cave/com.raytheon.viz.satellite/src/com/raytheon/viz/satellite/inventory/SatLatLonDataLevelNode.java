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

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.data.LatLonRequestableData;
import com.raytheon.uf.common.derivparam.tree.LatLonDataLevelNode;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

/**
 * Implementation of {@link LatLonDataLevelNode} for satellite. Handles tiling
 * by interpreting the argument to the RequestableData as a
 * {@link GridGeometry2D}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 17, 2017  6345     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class SatLatLonDataLevelNode extends LatLonDataLevelNode {

    public SatLatLonDataLevelNode(String source, LatOrLon parameter,
            Level level) {
        super(source, parameter, level);
    }

    @Override
    public Collection<? extends IGridGeometryProvider> getAvailableSpaces(
            Map<String, RequestConstraint> originalConstraints)
            throws DataCubeException {
        return Collections.singleton(TimeAndSpace.SPACE_AGNOSTIC);
    }

    @Override
    protected AbstractRequestableData createData() {
        return new SatLatLonRequestableData(parameter);
    }

    private static class SatLatLonRequestableData
            extends LatLonRequestableData {
        public SatLatLonRequestableData(LatOrLon parameter) {
            super(parameter);
        }

        @Override
        public GridGeometry2D getGridGeometry(Object arg) {
            if (arg instanceof GridGeometry2D) {
                return (GridGeometry2D) arg;
            } else {
                return super.getGridGeometry(arg);
            }
        }

    }

}
