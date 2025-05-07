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

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * A requestable data record which wraps a {@link RadarRequestableData} to
 * support the radar virtual volume concept. This mostly defers to the wrapped
 * data record, except that it represents the data as being for the given
 * virtual time and uses the virtual volume version of the radar parameter.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2024 2037092    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarVirtualVolumeRequestableData extends RadarRequestableData {

    private final DataTime virtualTime;

    private final RadarRequestableData realRequestableData;

    /**
     * Constructor.
     *
     * @param virtualTime
     *            time that this data record should claim that it's for. If
     *            null, the real data time is used, and this wrapper just uses
     *            the virtual volume version of the parameter.
     * @param requestableData
     *            the real data to wrap
     * @throws VizException
     */
    public RadarVirtualVolumeRequestableData(DataTime virtualTime,
            RadarRequestableData requestableData) throws VizException {
        super(requestableData.radarSource,
                buildGridRecord(virtualTime, requestableData));
        this.virtualTime = virtualTime;
        this.realRequestableData = requestableData;
    }

    protected static GridRecord buildGridRecord(DataTime virtualTime,
            RadarRequestableData realRequestableData) throws VizException {
        String virtualParamAbbrev = RadarAsGridUtil.getVirtualVolumeParamAbbrev(
                realRequestableData.getParameter());
        DataTime time = virtualTime == null ? realRequestableData.getDataTime()
                : virtualTime;
        return RadarRequestableData.buildGridRecord(
                realRequestableData.radarSource, virtualParamAbbrev, time);
    }

    @Override
    public boolean needsRequest(Request request) {
        return realRequestableData.needsRequest(request);
    }

    @Override
    public IDataRecord[] getDataValue(Object arg) throws DataCubeException {
        return realRequestableData.getDataValue(arg);
    }

    @Override
    public void cacheDataValue(Request request, IDataRecord[] records) {
        realRequestableData.cacheDataValue(request, records);
    }

    @Override
    public DataTime getDataTime() {
        if (virtualTime == null) {
            return realRequestableData.getDataTime();
        }
        return virtualTime;
    }

    @Override
    public TimeAndSpace getTimeAndSpace() {
        if (virtualTime == null) {
            return realRequestableData.getTimeAndSpace();
        }
        return new RadarVirtualTimeAndSpace(virtualTime,
                realRequestableData.getSpace(),
                realRequestableData.getDataTime());
    }
}
