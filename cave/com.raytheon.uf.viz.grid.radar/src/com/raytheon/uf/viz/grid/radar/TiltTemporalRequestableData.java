/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.grid.radar;

import com.raytheon.uf.common.dataplugin.grid.derivparam.data.SliceUtil;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

import si.uom.SI;

/**
 * A requestable data object for the height of a radar tilt at a particular
 * time. The data will be centered on a specific radar station and increase
 * moving away from that station.
 *
 * This is a temporal data object, as it converts the primary elevation angle of
 * this node to the appropriate true elevation angle for the radar's Volume
 * Coverage Pattern (VCP) at that particular point in time. The tilt heights are
 * then calculated for the true elevation angle.
 *
 * See {@link RadarElevationAngleMapping} for more info.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2024 2037939    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class TiltTemporalRequestableData extends AbstractRequestableData {

    protected final String icao;

    protected final TimeAndSpace tas;

    protected final double trueElevationAngle;

    public TiltTemporalRequestableData(String modelName,
            Level primaryAngleLevel, TimeAndSpace tas) {
        this.source = modelName;
        this.icao = RadarAsGridUtil.getIcaoFromModelName(modelName);
        this.unit = SI.METRE;
        this.parameter = RadarUtil.TILT;
        this.parameterName = RadarUtil.TILT;
        this.level = primaryAngleLevel;
        tas = processTimeAndSpace(tas, primaryAngleLevel, icao);
        this.tas = tas;
        this.dataTime = tas.getTime();
        this.space = tas.getSpace();
        this.trueElevationAngle = RadarElevationAngleMapping.getInstance(icao)
                .getTrueElevationAngle(level.getLevelonevalue(),
                        getTrueElevationAngleTime(tas));
    }

    @Override
    public FloatDataRecord getDataValue(Object arg) throws DataCubeException {
        GridCoverage coverage = (GridCoverage) getSpace();
        FloatDataRecord fdr = TiltUtils.getInstance().getHeightGrid(icao,
                coverage, trueElevationAngle);
        if (fdr != null && arg instanceof Request) {
            return SliceUtil.slice(fdr, (Request) arg);
        } else {
            return fdr;
        }
    }

    @Override
    public TimeAndSpace getTimeAndSpace() {
        return tas;
    }

    private static TimeAndSpace processTimeAndSpace(TimeAndSpace tas,
            Level level, String icao) {
        IGridGeometryProvider space = tas.getSpace();
        // SPACE_AGNOSTIC may be requested, so ensure space is valid
        if (!(space instanceof GridCoverage)) {
            space = RadarAdapter.getInstance().getCoverage(icao);
        }
        /*
         * Convert time/space to be for this specific data - specifically if
         * it's a RadarVirtualDerivedTimeAndSpace, compare our tilt against its
         * current scan tilt to determine if we are virtual or not
         */
        if (tas instanceof RadarVirtualTimeAndSpace) {
            RadarVirtualTimeAndSpace virtTas = (RadarVirtualTimeAndSpace) tas;
            if (!(tas instanceof RadarVirtualDerivedTimeAndSpace) || level
                    .getLevelonevalue() > ((RadarVirtualDerivedTimeAndSpace) tas)
                            .getCurrScanTilt()) {
                return new RadarVirtualTimeAndSpace(virtTas.getTime(), space,
                        virtTas.getPrevScanTime());
            }
        }
        return new TimeAndSpace(tas.getTime(), space);
    }

    private static DataTime getTrueElevationAngleTime(TimeAndSpace tas) {
        if (tas instanceof RadarVirtualTimeAndSpace) {
            return ((RadarVirtualTimeAndSpace) tas).getPrevScanTime();
        }
        return tas.getTime();
    }

    @Override
    public String toString() {
        return "TiltTemporalRequestableData [icao=" + icao + ", tas=" + tas
                + ", level=" + level + ", trueElevationAngle="
                + trueElevationAngle + "]";
    }
}
