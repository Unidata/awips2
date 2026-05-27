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
package com.raytheon.uf.edex.plugin.acarssounding;

import static com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingConstants.EMPTY_SOUNDING;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.acars.dao.ACARSDao;
import com.raytheon.uf.edex.plugin.acarssounding.dao.ACARSSoundingDao;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSAircraftInfo;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSSoundingTools;
import com.raytheon.uf.edex.plugin.acarssounding.tools.AirportsBean;
import com.raytheon.uf.edex.plugin.acarssounding.tools.IntermediateData;
import com.raytheon.uf.edex.plugin.acarssounding.tools.SoundingBuilder;

/**
 * Uses the ACARS records referenced in the aircraft tailnumber files to create
 * new ACARSSounding records or add layers to existing ACARSSounding records.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009       1939 jkorman     Initial creation
 * Aug 18, 2014 3530       bclement    removed TimeTools usage
 * Dec 10, 2015 5166       kbisanz     Update logging to use SLF4J
 * Jul 26, 2016 5757       nabowle     Move processing out of localization structure.
 * Aug 08, 2016 5757       nabowle     Code cleanup.
 *
 * </pre>
 *
 * @author jkorman
 */

public class ACARSSounding {

    private Logger logger = LoggerFactory.getLogger(getClass());

    private String pluginName = ACARSSoundingTools.ACARS_SNDG_PLUGIN_NAME;

    private File dataDir = null;

    private SoundingBuilder builder;

    private ACARSDao acarsDAO = null;

    private ACARSSoundingDao soundingDAO = null;

    private boolean failSafe = false;

    private int failSafeError = 0;

    /**
     *
     * @param name
     */
    public ACARSSounding(String name, AirportsBean airports) {
        this.pluginName = name;

        try {
            setupFiles();
            if (dataDir == null) {
                setFailSafeError(3);
            }
        } catch (Exception e) {
            setFailSafeError(3);
        }
        try {
            acarsDAO = new ACARSDao();
        } catch (Exception e) {
            setFailSafeError(2);
        }
        try {
            soundingDAO = new ACARSSoundingDao();
        } catch (Exception e) {
            setFailSafeError(4);
        }
        try {
            builder = new SoundingBuilder(airports);
        } catch (Exception e) {
            setFailSafeError(1);
        }
    }

    /**
     *
     * @param sounding
     * @return
     */
    public PluginDataObject[] processSounding(ACARSAircraftInfo acftInfo) {
        ACARSSoundingRecord[] soundings = null;
        try {
            if (!failSafe) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Processing tailnumber "
                            + acftInfo.getTailNumber());
                }

                // Get a list of soundings that overlap the data
                List<ACARSSoundingRecord> tSoundings = checkSoundings(acftInfo);
                // Did we get any potential soundings back?
                int numSoundings = (tSoundings != null) ? tSoundings.size() : 0;
                if (logger.isDebugEnabled()) {
                    logger.debug(acftInfo.getTailNumber() + " Read ["
                            + numSoundings + "] soundings");
                }
                // Go create soundings
                soundings = builder.createSoundings(loadObs(acftInfo),
                        tSoundings);
            } else {
                soundings = EMPTY_SOUNDING;
            }
        } catch (Exception e) {
            logger.error("Error processing acars sounding ", e);
            soundings = EMPTY_SOUNDING;
        }
        if (soundings == null) {
            soundings = EMPTY_SOUNDING;
        }
        return soundings;
    }

    /**
     * Get the IntermediateData for a specified aircraft.
     *
     * @param acftInfo
     * @return
     */
    private IntermediateData loadObs(ACARSAircraftInfo acftInfo) {
        IntermediateData data = null;
        if (acftInfo != null) {
            List<String> uris = new ArrayList<>();
            File f = new File(dataDir, acftInfo.getTailNumber() + ".acft");
            if (f.exists()) {
                data = new IntermediateData(acftInfo);
                try (BufferedReader bf = new BufferedReader(new FileReader(f))) {
                    String s;
                    while ((s = bf.readLine()) != null) {
                        uris.add(s);
                    }
                    // All done reading
                } catch (IOException ioe) {
                    logger.error("Error reading tailnumber obs list", ioe);
                }
                // Done reading data. Get the current start and end times
                // for this aircraft.
                if (logger.isDebugEnabled()) {
                    logger.debug("Reading " + uris.size()
                            + " observations for tailnumber "
                            + acftInfo.getTailNumber());
                }

                // Do we have enough data to consider this data?
                if ((uris.size() >= ACARSSoundingTools.MIN_OBS_FOR_SOUNDING)) {
                    String s = uris.get(0);
                    Long startTime = Long.parseLong(s.substring(0, 20).trim());
                    Calendar start = TimeUtil
                            .newGmtCalendar(new Date(startTime));

                    s = uris.get(uris.size() - 1);
                    Long stopTime = Long.parseLong(s.substring(0, 20).trim());
                    Calendar end = TimeUtil.newGmtCalendar(new Date(stopTime));

                    List<ACARSRecord> obs = acarsDAO.getReports(
                            acftInfo.getTailNumber(), start, end);

                    data.setAcarsData(obs, uris);
                } else {
                    logger.info("Not enough observations to create sounding for aircraft "
                            + acftInfo.getTailNumber());
                }
            } else {
                // Could not find file to process! This shouldn't happen.
                // If files end up missing then we may have a threading problem.
                logger.error("Could not find file for aircraft "
                        + acftInfo.getTailNumber());
            }
        }
        return data;
    }

    /**
     * @param acftInfo
     * @return
     */
    private List<ACARSSoundingRecord> checkSoundings(ACARSAircraftInfo acftInfo) {

        String msg = "attempting " + acftInfo.getTailNumber() + " ";
        String tailNumber = acftInfo.getTailNumber();

        Calendar c = TimeUtil.newGmtCalendar(new Date(acftInfo.getStartTime()));
        msg += String.format(ACARSSoundingTools.STD_TM_FMT, c);

        c.setTimeInMillis(acftInfo.getStopTime());
        msg += "->";
        msg += String.format(ACARSSoundingTools.STD_TM_FMT, c);

        logger.info(msg);

        List<ACARSSoundingRecord> sndgs = soundingDAO.queryByTimeLimits(
                tailNumber, acftInfo.getStartTime(), acftInfo.getStopTime());

        return sndgs;
    }

    /**
     * Get the plugin name.
     *
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * Get the current time. ESB uses this to get the time for product header
     * information.
     *
     * @return The current time as a Long.
     * @return
     */
    public Long getQueueTime() {
        return new Long(System.currentTimeMillis());
    }

    /**
     *
     * @return
     */
    public int getFailSafeError() {
        return failSafeError;
    }

    /**
     *
     * @param failSafeError
     */
    public void setFailSafeError(int failSafeError) {
        failSafe = (failSafeError > 0);
        this.failSafeError = failSafeError;
    }

    /**
     *
     */
    private void setupFiles() {
        dataDir = ACARSSoundingTools
                .getInitializedDirectory(ACARSSoundingTools.DATA_PATH);
    }
}
