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

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * A LevelNode for the height of a radar tilt. The data will be centered on a
 * specific radar station and increase moving away from that station.
 *
 * This is a temporal node, as it produces unique requestable data objects for
 * each volume scan time, and those data objects each convert the primary
 * elevation angle of this node to the appropriate true elevation angle for the
 * radar's Volume Coverage Pattern (VCP) at that particular point in time. The
 * tilt heights are then calculated for the true elevation angle.
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
public class TiltTemporalGridDataLevelNode extends AbstractBaseDataNode {

    protected final String modelName;

    protected final String icao;

    public TiltTemporalGridDataLevelNode(TiltTemporalGridDataLevelNode that) {
        super(that);
        this.modelName = that.modelName;
        this.icao = that.icao;
    }

    public TiltTemporalGridDataLevelNode(String modelName,
            Level primaryAngleLevel) {
        this.modelName = modelName;
        this.icao = RadarAsGridUtil.getIcaoFromModelName(modelName);
        setLevel(primaryAngleLevel);
    }

    @Override
    public DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints) {
        return null;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws DataCubeException {
        Set<DataTime> times = RadarUpdater.getInstance().getStationTimes(icao);
        GridCoverage coverage = RadarAdapter.getInstance().getCoverage(icao);
        return times.stream().map(time -> new TimeAndSpace(time, coverage))
                .collect(Collectors.toUnmodifiableSet());
    }

    @Override
    public DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> originalConstraints,
            Set<TimeAndSpace> availability) {
        return null;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> originalConstraints,
            Set<TimeAndSpace> availability, Object response)
            throws DataCubeException {
        Set<AbstractRequestableData> data = new HashSet<>();
        for (TimeAndSpace tas : availability) {
            data.add(buildData(tas));
        }
        return data;
    }

    protected AbstractRequestableData buildData(TimeAndSpace tas) {
        return new TiltTemporalRequestableData(modelName, level, tas);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Objects.hash(icao, modelName);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        TiltTemporalGridDataLevelNode other = (TiltTemporalGridDataLevelNode) obj;
        return Objects.equals(icao, other.icao)
                && Objects.equals(modelName, other.modelName);
    }

    @Override
    public String toString() {
        return "TiltTemporalGridDataLevelNode [icao=" + icao + ", level="
                + level + "]";
    }
}
