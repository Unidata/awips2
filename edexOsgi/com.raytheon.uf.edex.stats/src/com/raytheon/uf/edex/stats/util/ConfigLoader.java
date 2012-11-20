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
package com.raytheon.uf.edex.stats.util;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.stats.xml.Statistics;

/**
 * Loads StatisticsConfig files from localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Updated error handling and validated config files.
 * Nov 07, 2012   1317     mpduff      Update config files.
 * 
 * </pre>
 * 
 * @author jsanchez
 * 
 */
public class ConfigLoader {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConfigLoader.class);

    private static final JAXBManager jaxbManager;
    static {
        try {
            jaxbManager = new JAXBManager(StatisticsConfig.class,
                    Statistics.class);
        } catch (JAXBException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final IPathManager pm = PathManagerFactory.getPathManager();

    private final List<StatisticsConfig> configurations = new ArrayList<StatisticsConfig>();

    private final String STATS_DIR = "stats";

    /**
     * Returns a list of all StatisticsConfig files.
     * 
     * @return
     */
    public List<StatisticsConfig> getConfigurations() {
        return configurations;
    }

    /**
     * Loads the StatisticsConfig files in the STATS_DIR directory.
     */
    public void load() throws Exception {
        LocalizationContext[] searchContext = pm
                .getLocalSearchHierarchy(LocalizationType.EDEX_STATIC);
        LocalizationFile[] localizationFiles = null;
        for (LocalizationContext ctx : searchContext) {
            localizationFiles = pm.listFiles(ctx, STATS_DIR, null, false, true);
            if (localizationFiles != null && localizationFiles.length > 0) {
                break;
            }
        }

        if (localizationFiles != null && localizationFiles.length > 0) {
            configurations.clear();
            for (LocalizationFile localizationFile : localizationFiles) {
                if (localizationFile.getFile() != null
                        && localizationFile.getFile().exists()) {
                    StatisticsConfig config = (StatisticsConfig) jaxbManager
                            .jaxbUnmarshalFromXmlFile(localizationFile
                                    .getFile());
                    config = validateAggregates(config);
                    configurations.add(config);
                }
            }
        }
    }

    /**
     * Removes the aggregate if its not a numerical parameter.
     * 
     * @param config
     */
    private StatisticsConfig validateAggregates(StatisticsConfig config)
            throws ClassNotFoundException {
        List<StatisticsAggregate> aggregates = new ArrayList<StatisticsAggregate>();
        for (StatisticsEvent event : config.getEvents()) {
            Class<?> clazz = Class.forName(event.getType());
            aggregates = new ArrayList<StatisticsAggregate>();
            for (StatisticsAggregate aggregate : event.getAggregateList()) {
                String aggregateField = aggregate.getField();
                try {
                    Field field = clazz.getDeclaredField(aggregateField);
                    if (!field.getType().isPrimitive()) {
                        statusHandler
                                .info("'"
                                        + aggregateField
                                        + "' not a primitive type. Aggregate being removed. ");
                    }
                    aggregates.add(aggregate);
                } catch (NoSuchFieldException e) {
                    statusHandler.info("'" + aggregateField
                            + "' not a valid field. Aggregate being removed. ");
                }
            }
            event.setAggregateList(aggregates);
        }

        return config;
    }
}
