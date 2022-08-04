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

package com.raytheon.uf.common.tafqueue;

import java.io.File;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalINIConfiguration;
import org.apache.commons.lang3.math.NumberUtils;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This class is used to read in configuration for AvnFPS verification (VFT)
 * product.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2013 15375      zhao        Initial creation
 * May 07, 2014 3091       rferrel     fcstid now a string.
 * May 14, 2015 4274       skorolev    Added period validation.
 * Nov 15, 2017 6183       tgurney     Move xmit.cfg to common_static
 *
 * </pre>
 *
 * @author zhao
 *
 */
public class TafQueueVftConfigMgr {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafQueueVftConfigMgr.class);

    private static TafQueueVftConfigMgr instance = null;

    /** Default, to be replaced by wmo in config file. */
    private String wmoid = "NXUS98";

    /** Default (siteid in taf_queue table). */
    private String siteid = "OAX";

    /** Default (stationid in taf_queue table). */
    private String stationid = "KOAX";

    /** Default forecasterid for VFT. */
    private String fcstid = "000";

    /**
     * Number of hours; default period for VFT product creation, to be replaced
     * by period in config file.
     */
    private int period = 6;

    /**
     * BBB field for VFT product; non-configurable
     */
    private String bbb = "___";

    private static final String XMIT_FILE = "aviation" + IPathManager.SEPARATOR
            + "config" + IPathManager.SEPARATOR + "xmit.cfg";

    private HierarchicalINIConfiguration xmitConfig = null;

    public static synchronized TafQueueVftConfigMgr getInstance() {
        if (instance == null) {
            instance = new TafQueueVftConfigMgr();
        }
        return instance;
    }

    private TafQueueVftConfigMgr() {
        // read in configuration
        loadXmitConfigFile();
        if (xmitConfig != null) {
            readConfiguration();
        }
    }

    private void loadXmitConfigFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pm.getLocalizationFile(context, XMIT_FILE);
        HierarchicalINIConfiguration config = new HierarchicalINIConfiguration();
        config.setDelimiterParsingDisabled(true);
        try {
            config.load(lFile.getFile());
        } catch (ConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Tafqueue VFT Configuration Manager: loading xmit.cfg file failed.\n"
                            + e.getLocalizedMessage(),
                    e);
            return;
        }
        this.xmitConfig = config;
    }

    private void readConfiguration() {
        String periodStr = "";
        try {
            String wmo = xmitConfig.getString("verification.wmo");
            String[] wmosplits = wmo.split(" ");
            wmoid = wmosplits[0];
            stationid = wmosplits[1];
            siteid = stationid.substring(1);
            String fcstidStr = xmitConfig.getString("verification.fcstid");
            if (fcstidStr != null) {
                setFcstid(fcstidStr);
            }
            periodStr = xmitConfig.getString("verification.period");
            period = Integer.parseInt(periodStr);
            if (period <= 0) {
                setDefaultPeriod(periodStr);
            }
        } catch (NumberFormatException e) {
            setDefaultPeriod(periodStr);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Tafqueue VFT Configuration Manager: error occurred while reading configuration.\n"
                            + e.getLocalizedMessage(),
                    e);
            return;
        }
        statusHandler.handle(Priority.INFO,
                "Tafqueue VFT Configuration Manager: wmo = " + wmoid + " "
                        + stationid + "; forecasterid = " + fcstid
                        + "; period = " + period);
        return;
    }

    public String getWmoid() {
        return wmoid;
    }

    public void setWmoid(String wmoid) {
        this.wmoid = wmoid;
    }

    public String getSiteid() {
        return siteid;
    }

    public void setSiteid(String siteid) {
        this.siteid = siteid;
    }

    public String getStationid() {
        return stationid;
    }

    public void setStationid(String stationid) {
        this.stationid = stationid;
    }

    public String getFcstid() {
        return fcstid;
    }

    public void setFcstid(String fcstid) {
        if (NumberUtils.isNumber(fcstid)) {
            // Handle old style ids.
            int id = Integer.parseInt(fcstid);
            this.fcstid = String.format("%03d", id);
        } else {
            this.fcstid = fcstid;
        }
    }

    public int getPeriod() {
        return period;
    }

    public void setPeriod(int period) {
        this.period = period;
    }

    public String getBbb() {
        return bbb;
    }

    public void setBbb(String bbb) {
        this.bbb = bbb;
    }

    private void setDefaultPeriod(String badPeriodStr) {
        period = 6;
        statusHandler.handle(Priority.PROBLEM,
                "Tafqueue VFT Configuration Manager: xmit.cfg file has invalid period value = "
                        + badPeriodStr + ". Default value = 6 will be used.");
    }

}
