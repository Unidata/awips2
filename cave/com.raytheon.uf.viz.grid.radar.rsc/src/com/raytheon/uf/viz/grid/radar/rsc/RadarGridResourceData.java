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
package com.raytheon.uf.viz.grid.radar.rsc;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.grid.radar.RadarVirtualDerivedTimeAndSpace;
import com.raytheon.uf.viz.grid.radar.rsc.util.RadarSRMResourceUtils;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeUtil;
import com.raytheon.viz.grid.record.RequestableDataRecord;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * Resource data for radar-as-grid data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@XmlAccessorType(XmlAccessType.NONE)
public class RadarGridResourceData extends GridResourceData {

    @Override
    protected AbstractGridResource<GridResourceData> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        AbstractGridResource<GridResourceData> rsc = super.constructResource(
                loadProperties, objects);

        String paramAbbrev = getMetadataMap()
                .get(GridConstants.PARAMETER_ABBREVIATION).getConstraintValue();
        if (RadarAsGridUtil.isVirtualVolume(paramAbbrev)) {
            RadarVirtualVolumeUtil.registerVirtualVolumeListeners(rsc,
                    () -> getPdos(rsc));
        }

        if (RadarRecordUtil.SRM
                .equals(RadarAsGridUtil.getStandardParamAbbrev(paramAbbrev))) {
            RadarSRMResourceUtils.registerSRMListeners(rsc);
        }

        return rsc;
    }

    @Override
    public String getExtraLegendText(GridRecord record) {
        if (record instanceof RequestableDataRecord) {
            TimeAndSpace tas = ((RequestableDataRecord) record)
                    .getTimeAndSpace();
            if (tas instanceof RadarVirtualDerivedTimeAndSpace) {
                RadarVirtualDerivedTimeAndSpace virtTas = (RadarVirtualDerivedTimeAndSpace) tas;
                return RadarVirtualVolumeUtil.buildLegendText(
                        virtTas.getCurrScanTilt(), virtTas.getPrevScanTime());
            }
        }
        return "";
    }

    protected Collection<PluginDataObject> getPdos(
            AbstractGridResource<GridResourceData> rsc) {
        return Arrays.stream(rsc.getDataTimes()).map(rsc::getPluginDataObjects)
                .filter(Objects::nonNull).flatMap(List::stream)
                .collect(Collectors.toList());
    }
}
