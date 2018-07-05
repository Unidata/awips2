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
package com.raytheon.edex.plugin.radar.handler;

import com.raytheon.edex.plugin.radar.dao.RadarStationDao;
import com.raytheon.edex.plugin.radar.util.RadarSpatialUtil;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataplugin.radar.request.GetRadarSpatialRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.StringUtil;

/**
 * Returns the RadarStation for the given request.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 21, 2009 #3922      rjpeter     Initial creation
 * May 11, 2017 #6266      nabowle     Add support for icao.
 *
 * </pre>
 *
 * @author rjpeter
 */
public class GetRadarSpatialRequestHandler implements
        IRequestHandler<GetRadarSpatialRequest> {
    @Override
    public RadarStation handleRequest(GetRadarSpatialRequest request)
            throws Exception {
        String icao = request.getIcao();
        if (StringUtil.isEmptyString(icao)) {
            return RadarSpatialUtil.getClosetRadarStation(request.getLat(),
                    request.getLon());
        } else {
            return new RadarStationDao().queryByRdaId(icao.toUpperCase());
        }

    }

}
