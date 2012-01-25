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
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.edex.plugin.acarssounding.dao.ACARSSoundingDao;
import com.raytheon.uf.common.dataplugin.acarssounding.tools.AirportsBean;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.acars.dao.ACARSDao;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSAircraftInfo;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSSoundingTools;
import com.raytheon.uf.edex.plugin.acarssounding.tools.IntermediateData;
import com.raytheon.uf.edex.plugin.acarssounding.tools.SoundingBuilder;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009       1939 jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSSounding {

    private Log logger = LogFactory.getLog(getClass());

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

        logger.info("Creating SoundingBuilder");

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
        ACARSSoundingRecord [] soundings = null;
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
                // TODO : Change this to a debug later!
                logger.info(acftInfo.getTailNumber() + " Read [" + numSoundings + "] soundings");
                // Go create soundings
                soundings = builder.createSoundings(loadObs(acftInfo),tSoundings);
            } else {
                soundings = EMPTY_SOUNDING;
            }
        } catch (Exception e) {
            logger.error("Error processing acars sounding ", e);
            soundings = EMPTY_SOUNDING;
        }
        if(soundings == null) {
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
            ArrayList<String> uris = new ArrayList<String>();
            File f = new File(dataDir, acftInfo.getTailNumber() + ".acft");
            if (f.exists()) {
                data = new IntermediateData(acftInfo);
                BufferedReader bf = null;
                try {
                    bf = new BufferedReader(new FileReader(f));

                    String s = null;
                    while ((s = bf.readLine()) != null) {
                        uris.add(s);
                    }
                    // All done reading
                } catch (IOException ioe) {
                    logger.error("Error reading tailnumber obs list", ioe);
                    // TODO: log error.
                } finally {
                    if (bf != null) {
                        try {
                            bf.close();
                        } catch (IOException ioe) {
                            logger.error("Error closing tailnumber obs list",
                                    ioe);
                        }
                    }
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
                    Long startTime = Long.parseLong(s.substring(0,20).trim());
                    Calendar start = TimeTools.newCalendar(startTime);
                    
                    s = uris.get(uris.size() - 1);
                    Long stopTime = Long.parseLong(s.substring(0,20).trim());
                    Calendar end = TimeTools.newCalendar(stopTime);

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

        Calendar c = TimeTools.newCalendar(acftInfo.getStartTime());
        msg += String.format(ACARSSoundingTools.STD_TM_FMT,c);

        c.setTimeInMillis(acftInfo.getStopTime());
        msg += "->";
        msg += String.format(ACARSSoundingTools.STD_TM_FMT,c);
        
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
        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

        dataDir = pathMgr.getFile(ctx, ACARSSoundingTools.BASE_PATH
                + ACARSSoundingTools.DATA_PATH);

    }
}
