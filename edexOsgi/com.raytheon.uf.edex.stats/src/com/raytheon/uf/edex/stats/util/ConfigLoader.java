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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.stats.xml.Aggregate;
import com.raytheon.uf.edex.stats.xml.Statistics;
import com.raytheon.uf.edex.stats.xml.StatsConfig;

/**
 * Loads statsConfig files from localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez     Updated error handling and validated config files.
 * 
 * </pre>
 * 
 * @author jsanchez
 * 
 */
public class ConfigLoader {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConfigLoader.class);

    private JAXBContext jax;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /** Marshaller object */
    private Marshaller marshaller;

    private IPathManager pm = PathManagerFactory.getPathManager();

    private List<StatsConfig> configurations;

    private final String STATS_DIR = "stats";

    /**
     * Constructor. Performs an initial statsCon
     */
    public ConfigLoader() throws JAXBException {
        jax = JAXBContext.newInstance(new Class[] { StatsConfig.class,
                Statistics.class });
        unmarshaller = jax.createUnmarshaller();
        this.marshaller = jax.createMarshaller();
        this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        this.configurations = new ArrayList<StatsConfig>();
    }

    /**
     * Loads the statsConfig files in the STATS_DIR directory.
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
                    StatsConfig config = (StatsConfig) unmarshaller
                            .unmarshal(localizationFile.getFile());
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
    private StatsConfig validateAggregates(StatsConfig config)
            throws ClassNotFoundException {
        Class<?> clazz = Class.forName(config.getEventType());
        List<Aggregate> aggregates = new ArrayList<Aggregate>();

        for (Aggregate aggregate : config.getAggregates()) {
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

        config.setAggregates(aggregates.toArray(new Aggregate[aggregates.size()]));

        return config;
    }

    /**
     * Returns a list of all statsConfig files.
     * 
     * @return
     */
    public List<StatsConfig> getConfigurations() {
        return configurations;
    }

}
