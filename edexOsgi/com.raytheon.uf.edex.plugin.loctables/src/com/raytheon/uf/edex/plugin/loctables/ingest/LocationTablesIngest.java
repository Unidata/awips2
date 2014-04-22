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
package com.raytheon.uf.edex.plugin.loctables.ingest;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.edex.ndm.ingest.IDataSetIngester;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.edex.plugin.loctables.util.CommonObsSpatialBuilder;
import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.DefaultHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.MaritimeTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.MesonetTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.MetarTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.PirepTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.RAOBTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.SynopticLandTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationStoreStrategy;

/**
 * Location Tables NDM subscriber
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 08, 2010            jkorman     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class LocationTablesIngest implements INationalDatasetSubscriber {

    private static final TableHandler DEFAULT_HANDLER = new DefaultHandler();

    private HashMap<String, TableHandler> handlers = null;

    private IDataSetIngester ingester = null;

    private final Log logger = LogFactory.getLog(getClass());

    @SuppressWarnings("unused")
    private LocationTablesIngest() {
    }

    public LocationTablesIngest(String pluginName, IDataSetIngester ingester) {
        this.ingester = ingester;

        setupHandlers();

        setupLocalFiles();
    }

    /**
     * 
     */
    private void setupHandlers() {

        logger.info("Creating handlers");
        handlers = new HashMap<String, TableHandler>();

        handlers.put("pirepsTable.txt", new PirepTableHandler(
                new ObStationStoreStrategy()));
        handlers.put("maritimeStationInfo.txt", new MaritimeTableHandler(
                new ObStationStoreStrategy()));
        handlers.put("metarStationInfo.txt", new MetarTableHandler(
                new ObStationStoreStrategy()));
        handlers.put("synopticStationTable.txt", new SynopticLandTableHandler(
                new ObStationStoreStrategy()));
        handlers.put("raobStationInfo.txt", new RAOBTableHandler(
                new ObStationStoreStrategy()));
        handlers.put("mesonetStationInfo.txt", new MesonetTableHandler(
                new ObStationStoreStrategy()));

        handlers.put("CMANStationInfo.txt", new MaritimeTableHandler(
                new ObStationStoreStrategy()));
        handlers.put("common_obs_spatial.txt",
                new CommonObsSpatialBuilder(this));

        for (String fileName : handlers.keySet()) {
            ingester.registerListener(fileName, this);
        }
    }

    private void setupLocalFiles() {
        String currentSite = SiteUtil.getSite();

        List<MonitorConfigurationManager> monitors = new ArrayList<MonitorConfigurationManager>();
        monitors.add(FogMonitorConfigurationManager.getInstance());
        monitors.add(SSMonitorConfigurationManager.getInstance());
        monitors.add(SnowMonitorConfigurationManager.getInstance());
        for (MonitorConfigurationManager monitor : monitors) {
            try {
                monitor.readConfigXml(currentSite);
            } catch (Throwable t) {
                logger.error("Could not configure "
                        + monitor.getClass().getName() + " for site "
                        + currentSite, t);
            }
        }
    }

    /**
     * Accept INationalDatasetSubscriber notifications.
     */
    @Override
    public void notify(String fileName, File file) {
        processFile(file);
    }

    /**
     * 
     * @param file
     * @return
     */
    public void processFile(File file) {
        getHandler(file).processFile(file);
    }

    /**
     * 
     * @param file
     * @return
     */
    private TableHandler getHandler(File file) {
        TableHandler handler = null;
        if (file != null) {
            if (handlers != null) {
                handler = handlers.get(file.getName());
                if (handler == null) {
                    handler = DEFAULT_HANDLER;
                }
            } else {
                handler = DEFAULT_HANDLER;
            }
        } else {
            handler = DEFAULT_HANDLER;
        }
        return handler;
    }

    /**
     * 
     * @return
     */
    public Map<String, TableHandler> getHandlers() {
        return handlers;
    }
}
