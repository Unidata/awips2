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

import java.util.List;
import java.util.stream.Collectors;

import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.data.CubeRequestableData;
import com.raytheon.uf.common.inventory.tree.CubeLevel;
import com.raytheon.uf.viz.grid.radar.util.RadarVirtualVolumeStatus;

/**
 * Extension of {@link CubeRequestableData} that is intended specifically for
 * cubes of data from a radar source.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2024 2037624    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarCubeRequestableData extends CubeRequestableData {

    public RadarCubeRequestableData(AbstractRequestableData paramRecord) {
        super(paramRecord);
    }

    @Override
    public void addParam(AbstractRequestableData paramRecord) {
        super.addParam(paramRecord);
    }

    @Override
    public TimeAndSpace getTimeAndSpace() {
        /*
         * If this data uses virtual volumes, build a time/space value that
         * indicates the previous scan time that is blended into the current
         * scan, along with the current scan tilt (all higher tilts are from the
         * previous scan).
         */
        List<AbstractRequestableData> paramData = getDataMap().values().stream()
                .map(CubeLevel::getParam).collect(Collectors.toList());
        RadarVirtualVolumeStatus status = RadarVirtualVolumeStatus
                .build(paramData, dataTime);
        if (status != null) {
            return new RadarVirtualDerivedTimeAndSpace(dataTime, space,
                    status.getPrevScanTime(), status.getCurrScanTilt());
        }
        return super.getTimeAndSpace();
    }
}
