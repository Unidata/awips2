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

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.RadarHelper;

/**
 * A requestable data record for radar Storm Relative Velocity Map (SRM) data
 * that derives SRM from radial velocity and then converts it to the expected
 * grid projection and units.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2023 9021       mapeters    Initial creation
 * Dec 20, 2023 2036519    mapeters    Move config listening to RadarRequestableDataFactory
 *
 * </pre>
 *
 * @author mapeters
 */
public class RadarSRMRequestableData extends RadarRequestableData {

    /**
     * Constructor.
     *
     * @param source
     * @param parameterAbbrev
     * @throws VizException
     */
    public RadarSRMRequestableData(RadarRecord source, String parameterAbbrev)
            throws VizException {
        super(source, parameterAbbrev);
    }

    @Override
    protected byte[] getRadialData() throws DataCubeException {
        try {
            RadarHelper.loadSRMVelocity(radarSource);
            return radarSource.srmData;
        } catch (VizException e) {
            throw new DataCubeException(
                    "Error generating SRM data for " + radarSource, e);
        }
    }

    protected void clearCache() {
        synchronized (cache) {
            cache.clear();
        }
    }
}
