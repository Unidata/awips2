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
package com.raytheon.uf.common.monitor.config;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.xml.AreaThresholdXML;
import com.raytheon.uf.common.monitor.xml.AreaXML;
import com.raytheon.uf.common.monitor.xml.ThresholdsXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This is a "generic" threshold manager class that handles display and monitor
 * thresholds for FOG, SNOW, and SAFESEAS.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 15, 2009  3963     lvenable  Initial creation
 * Dec 04, 2012  1351     skorolev  Cleaned code
 * Sep 18, 2015  3873     skorolev  Added error message for corrupted or empty
 *                                  default threshold file.
 * Dec 26, 2015  5115     skorolev  Moved from
 *                                  com.raytheon.uf.viz.monitor.thresholds.
 *                                  Added getMonitorParameters().
 * May 07, 2019  7689     randerso  Code cleanup
 * May 21, 2019  7689     randerso  Refactor handling of FSSObs thresholds
 *
 * </pre>
 *
 * @author lvenable
 */
public class ThresholdMgr implements ILocalizationFileObserver {

    private static final String DEFAULT_VALUES_KEY = "DefaultValues";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThresholdMgr.class);

    /**
     * Threshold XML data.
     */
    private ThresholdsXML baseThresholds;

    private ThresholdsXML siteThresholds;

    /** List of listeners */
    private final Set<MonitorConfigListener> listeners = new CopyOnWriteArraySet<>();

    private AppName appName;

    private DataUsageKey usageKey;

    private LocalizationFile lf;

    /**
     * Constructor.
     *
     * @param appName
     * @param usageKey
     * @param fileName
     *            file name
     * @throws LocalizationException
     * @throws IOException
     * @throws SerializationException
     *
     */
    public ThresholdMgr(AppName appName, DataUsageKey usageKey, String fileName)
            throws SerializationException, IOException, LocalizationException {
        this.appName = appName;
        this.usageKey = usageKey;

        readThresholds(fileName);
    }

    /**
     * Called when this ThersholdMgr is no longer needed
     */
    public void dispose() {
        if (lf != null) {
            lf.removeFileUpdatedObserver(this);
        }

        this.listeners.clear();
    }

    /**
     * @return the threshold directory
     */
    public String getThresholdDir() {
        return getThresholdPath("");
    }

    private String getThresholdPath(String fileName) {
        return LocalizationUtil.join(appName.name().toLowerCase(), "threshold",
                usageKey.name().toLowerCase(), fileName);
    }

    private void readThresholds(String fileName)
            throws SerializationException, IOException, LocalizationException {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        ILocalizationFile baseLf = pm.getLocalizationFile(context,
                getThresholdPath(fileName));
        this.baseThresholds = ThresholdsXML.readThresholdXml(baseLf);

        context = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        lf = pm.getLocalizationFile(context, getThresholdPath(fileName));
        if (lf.exists()) {
            this.siteThresholds = ThresholdsXML.readThresholdXml(lf);
        } else {
            this.siteThresholds = new ThresholdsXML();
        }

        lf.addFileUpdatedObserver(this);
    }

    /**
     * Save the XML threshold data to the current XML file name.
     *
     * @param fileName
     */
    public void saveThresholds(String fileName) {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile lf = pm.getLocalizationFile(context,
                getThresholdPath(fileName));

        try {
            getThresholdXML().saveThresholdXml(lf);
        } catch (SerializationException | IOException
                | LocalizationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error saving the XML file: " + lf, e);
        }
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        if (message.getFileName().equals(lf.getPath())) {
            for (MonitorConfigListener listener : listeners) {
                listener.configChanged(new MonitorConfigEvent(this));
            }
        }
    }

    private AreaXML getAreaOrDefault(String areaId) {
        AreaXML area = siteThresholds.getArea(areaId);
        if (area == null) {
            area = baseThresholds.getArea(DEFAULT_VALUES_KEY);
        }

        return area;
    }

    private AreaXML getAreaOrCopy(String areaId) {
        if (DEFAULT_VALUES_KEY.equals(areaId)) {
            throw new IllegalArgumentException(
                    "Modifying default values is not allowed.");
        }

        AreaXML area = siteThresholds.getArea(areaId);
        if (area == null) {
            area = baseThresholds.getArea(DEFAULT_VALUES_KEY).copy();
            area.setAreaId(areaId);
            siteThresholds.addArea(area);
        }

        return area;
    }

    /**
     * Get the red value.
     *
     * @param areaId
     *            Area ID.
     * @param key
     *            Identifying key.
     * @return The red value.
     */
    public double getRedValue(String areaId, String key) {
        return getAreaOrDefault(areaId).getRedValue(key);
    }

    /**
     * Get the yellow value.
     *
     * @param areaId
     *            Area ID.
     * @param key
     *            Identifying key.
     * @return The yellow value.
     */
    public double getYellowValue(String areaId, String key) {
        return getAreaOrDefault(areaId).getYellowValue(key);
    }

    /**
     * Set the red value.
     *
     * @param areaId
     *            Area ID.
     * @param key
     *            Identifying key.
     * @param value
     *            The red value.
     */
    public void setRedValue(String areaId, String key, double value) {
        AreaXML area = getAreaOrCopy(areaId);
        area.setRedValue(key, value);
    }

    /**
     * Set the yellow value.
     *
     * @param areaId
     *            Area ID.
     * @param key
     *            Identifying key.
     * @param value
     *            The yellow value.
     */
    public void setYellowValue(String areaId, String key, double value) {
        AreaXML area = getAreaOrCopy(areaId);
        area.setYellowValue(key, value);
    }

    /**
     * @return the threshold XML data
     */
    public ThresholdsXML getThresholdXML() {
        return this.siteThresholds;
    }

    /**
     * Gets parameters from current ThresholdXML.
     *
     * @return params
     */
    public List<String> getMonitorParameters() {
        List<String> retVal = new ArrayList<>();
        List<AreaThresholdXML> xmls = baseThresholds.getArea(DEFAULT_VALUES_KEY)
                .getAreaThresholds();
        for (AreaThresholdXML xml : xmls) {
            retVal.add(xml.getKey());
        }
        return retVal;
    }

    /**
     * Adds Monitor Configuration Listener
     *
     * @param ml
     *            Monitor config listener
     */
    public void addListener(MonitorConfigListener ml) {
        listeners.add(ml);
    }

    /**
     * Removes Monitor Configuration Listener
     *
     * @param ml
     *            Monitor config listener
     */
    public void removeListener(MonitorConfigListener ml) {
        listeners.remove(ml);
    }

}
