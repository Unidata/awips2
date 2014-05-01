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
package com.raytheon.uf.common.dataplugin.profiler.dao;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.dataplugin.profiler.ProfilerLevel;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerObs;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Transforms profiler {@link PointDataContainer}s into {@link ProfilerObs}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 28, 2009           jkorman     Initial creation
 * Dec 03, 2013  2537     bsteffen    Switch logger to ufstatus.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ProfilerDataTransform {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ProfilerDataTransform.class);

    public static final String[] HDR_PARAMS = new String[] { "stationId",
            "profilerId", "profilerName", "validTime", "latitude", "longitude",
            "elevation", "timeObs", "dataURI" };

    public static final String HDR_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < HDR_PARAMS.length - 1; ++i) {
            sb.append(HDR_PARAMS[i]).append(",");
        }
        sb.append(HDR_PARAMS[HDR_PARAMS.length - 1]);
        HDR_PARAMS_LIST = sb.toString();
    }

    private static final String[] OTHER_PARAMS = new String[] { "windSpeedSfc",
            "windDirSfc", "pressure", "temperature", "rainRate", "relHumidity",
            "submode", "numProfLvls", "height", "uComponent", "vComponent",
            "HorizSpStdDev", "wComponent", "VertSpStdDev", "peakPower",
            "levelMode", "uvQualityCode", "consensusNum" };

    public static final String[] MAN_PARAMS = Arrays.copyOf(HDR_PARAMS,
            HDR_PARAMS.length + OTHER_PARAMS.length);

    public static final String MAN_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append(HDR_PARAMS_LIST).append(",");

        for (int i = 0; i < OTHER_PARAMS.length - 1; ++i) {
            sb.append(OTHER_PARAMS[i]).append(",");
            MAN_PARAMS[HDR_PARAMS.length + i] = OTHER_PARAMS[i];
        }
        sb.append(OTHER_PARAMS[OTHER_PARAMS.length - 1]);
        MAN_PARAMS[MAN_PARAMS.length - 1] = OTHER_PARAMS[OTHER_PARAMS.length - 1];

        MAN_PARAMS_LIST = sb.toString();
    }

    private static ProfilerObs getProfilerObsHdr(PointDataView pdv) {
        ProfilerObs obs = null;
        if (pdv != null) {

            String uri = pdv.getString("dataURI");
            logger.debug("URI = " + uri);
            obs = new ProfilerObs(uri);

            long vt = pdv.getNumber("validTime").longValue();
            Calendar timeObs = TimeUtil.newGmtCalendar();
            timeObs.setTimeInMillis(vt);
            obs.setTimeObs(timeObs);

            SurfaceObsLocation location = new SurfaceObsLocation();

            int elev = pdv.getNumber("elevation").intValue();
            location.setElevation(elev);
            double lat = pdv.getNumber("latitude").doubleValue();
            double lon = pdv.getNumber("longitude").doubleValue();
            location.assignLocation(lat, lon);
            location.setStationId(pdv.getString("stationId"));
            obs.setLocation(location);
            obs.setProfilerId(pdv.getString("profilerId"));
            obs.setProfilerName(pdv.getString("profilerName"));
        }
        return obs;
    }

    private static ProfilerObs getProfilerData(PointDataView pdv,
            ProfilerObs profile) {
        if (profile != null) {
            profile.setSfcWindDir(pdv.getNumber("windDirSfc").doubleValue());
            profile.setSfcWindSpeed(pdv.getNumber("windSpeedSfc").doubleValue());

            Number[] h = pdv.getNumberAllLevels("height");
            Number[] uWind = pdv.getNumberAllLevels("uComponent");
            Number[] vWind = pdv.getNumberAllLevels("vComponent");
            Number[] qualCode = pdv.getNumberAllLevels("uvQualityCode");

            ArrayList<ProfilerLevel> levels = new ArrayList<ProfilerLevel>();

            for (int i = 0; i < h.length; i++) {
                if ((h[i].intValue() > -400) && (qualCode[i].intValue() == 0)) {
                    ProfilerLevel level = new ProfilerLevel();
                    level.setLevelHeight(h[i].intValue());
                    level.setUcWind(uWind[i].doubleValue());
                    level.setVcWind(vWind[i].doubleValue());

                    levels.add(level);
                }
            }
            Collections.sort(levels);
            profile.setLevels(levels);
        }
        return profile;
    }

    /**
     * 
     * @param container
     * @return
     */
    public static ProfilerObs[] toProfilerRecords(PointDataContainer container) {
        List<ProfilerObs> records = new ArrayList<ProfilerObs>();

        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);

            ProfilerObs obs = getProfilerObsHdr(pdv);
            obs = getProfilerData(pdv, obs);
            if (obs != null) {
                records.add(obs);
            }
        }

        return records.toArray(new ProfilerObs[records.size()]);
    }

}
