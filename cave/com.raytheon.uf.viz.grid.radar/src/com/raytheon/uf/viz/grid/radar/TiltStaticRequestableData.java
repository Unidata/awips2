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
package com.raytheon.uf.viz.grid.radar;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.grid.derivparam.data.SliceUtil;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

import si.uom.SI;

/**
 * Requestable Data that generates tilt elevation.
 *
 * The requestable data itself is static/time-agnostic, but it can produce
 * time-dependent results if a {@link TiltRequest} is passed in as an arg. The
 * true elevation angle is specifically time-dependent, since it can change
 * based on the current Volume Coverage Pattern (VCP) of the radar station.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 15, 2010           rjpeter   Initial creation
 * Aug 15, 2017  6332     bsteffen  Move to viz.grid.radar plugin
 * Jul 17, 2020  17574    smoorthy  Have tilt requests use trueElevationAngle
 *                                  for height calculation
 * Jul 14, 2021  8576     randerso  Changed RadarAdapter to support multiple
 *                                  local radars as defined in radarsInUse.txt
 * Sep 15, 2021  8652     njensen   Find icao through util method
 * Oct 14, 2024  2037939  mapeters  Rename from TiltRequestableData, radar-kxxx
 *                                  models now use separate temporal data object
 *
 * </pre>
 *
 * @author rjpeter
 */
public class TiltStaticRequestableData extends AbstractRequestableData {

    public TiltStaticRequestableData(String modelName, Level tiltAngle,
            GridCoverage coverage) {
        this.source = modelName;
        this.unit = SI.METRE;
        this.parameter = "TILT";
        this.parameterName = "TILT";
        this.level = tiltAngle;
        this.space = coverage;
    }

    @Override
    public FloatDataRecord getDataValue(Object arg) throws DataCubeException {

        GridCoverage coverage = (GridCoverage) getSpace();
        FloatDataRecord fdr = null;
        if (arg instanceof TiltRequest) {
            Coordinate tiltLoc = ((TiltRequest) arg).getTiltLocation();
            double trueElevationAngle = ((TiltRequest) arg)
                    .getTrueElevationAngle();
            fdr = TiltUtils.getInstance().getHeightGrid(tiltLoc, coverage,
                    trueElevationAngle);
        } else {
            // Default to home radar station
            fdr = TiltUtils.getInstance().getHeightGrid((String) null, coverage,
                    level.getLevelonevalue());
        }
        if (fdr != null && arg instanceof Request) {
            return SliceUtil.slice(fdr, (Request) arg);
        } else {
            return fdr;
        }
    }

}