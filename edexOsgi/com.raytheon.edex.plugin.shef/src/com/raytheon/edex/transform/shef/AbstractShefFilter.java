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
package com.raytheon.edex.transform.shef;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractObsFilter;
import com.raytheon.uf.edex.decodertools.core.filterimpl.PluginDataObjectFilter;
import com.raytheon.uf.edex.decodertools.core.filterimpl.RadiusFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.RectFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.StationIdFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.WMOHeaderFilterElement;

/**
 * Abstraction of a Shef Filter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2015 4783       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public abstract class AbstractShefFilter {

    private static final String ERROR_1_FMT = "Could not create {%s} context for file \"%s\"";

    private static final String ERROR_2_FMT = "File %s does not exist";

    protected static final String METAR_CFG = "metar.cfg";

    protected static final String DUMMY_FILTER_NAME = "Created Pass-All filter";

    public static final String FILTERS_DIR = "plugin-filters";

    protected final Log logger = LogFactory.getLog(getClass());

    private String filterConfigFile = null;

    protected AbstractShefFilter() {
        this.init();
    }

    protected AbstractShefFilter(String configFile, String localContext,
            final Class<? extends AbstractShefFilter> shefFilterClass) {
        this.init();
        this.filterConfigFile = configFile;
        this.readConfig(localContext, shefFilterClass);
    }

    private void readConfig(String localContext,
            final Class<? extends AbstractShefFilter> shefFilterClass) {
        try {
            /*
             * Retrieve and verify the existence of the configuration file.
             */
            IPathManager manager = PathManagerFactory.getPathManager();
            if (manager == null) {
                throw new Exception("Failed to construct a Path Manager.");
            }
            LocalizationContext context = manager.getContext(EDEX_STATIC,
                    LocalizationLevel.valueOf(localContext));
            if (context == null) {
                throw new Exception(String.format(ERROR_1_FMT, localContext,
                        this.filterConfigFile));
            }
            Path configPath = Paths.get(
                    manager.getFile(context, FILTERS_DIR).getAbsolutePath())
                    .resolve(filterConfigFile);
            if (Files.exists(configPath) == false) {
                throw new Exception(String.format(ERROR_2_FMT,
                        configPath.toString()));
            }

            /*
             * Read the configuration file.
             */
            JAXBManager jaxb = new JAXBManager(PluginDataObjectFilter.class,
                    shefFilterClass, RadiusFilterElement.class,
                    RectFilterElement.class, StationIdFilterElement.class,
                    WMOHeaderFilterElement.class);
            byte[] data = Files.readAllBytes(configPath);
            Object obj = jaxb.unmarshalFromXml(new String(data));
            if (shefFilterClass.isInstance(obj)
                    || obj instanceof PluginDataObjectFilter) {
                this.buildRun(obj, this.filterConfigFile);
            } else {
                throw new Exception("Read unexpected config data type: "
                        + obj.getClass().getName());
            }
        } catch (Exception e) {
            logger.error("Failed to process filter configuration.", e);
            this.createDummyFilter();
        }
    }

    protected PluginDataObject filterARun(PluginDataObject report,
            List<AbstractFilterElement> filterElements) {
        if (report != null) {
            PluginDataObject r = null;
            boolean keep = true;
            for (AbstractFilterElement element : filterElements) {
                r = element.filter(report);

                /*
                 * Only allow keep to be set to false. Once false it stays that
                 * way.
                 */
                if (AbstractObsFilter.INCLUDE_TYPE.equals(element
                        .getFilterType())) {
                    // Did the filter pass?
                    if (r == null) {
                        // If we fail an element, exit now.
                        keep = false;
                        break;
                    }
                } else if (AbstractObsFilter.EXCLUDE_TYPE.equals(element
                        .getFilterType())) {
                    if (r != null) {
                        // There was a match, so we want to remove this
                        // item.
                        keep = false;
                        // And there's no reason for further checks.
                        break;
                    }
                }
            }
            if (keep) {
                report = r;
            } else {
                report = null;
            }
        }
        return report;
    }

    protected abstract void init();

    protected abstract void buildRun(final Object obj, final String configFile);

    protected abstract void createDummyFilter();

    public abstract PluginDataObject[] filter(PluginDataObject[] reports);
}