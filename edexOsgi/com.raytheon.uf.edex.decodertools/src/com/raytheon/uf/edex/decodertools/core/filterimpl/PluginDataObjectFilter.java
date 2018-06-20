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
package com.raytheon.uf.edex.decodertools.core.filterimpl;

import java.io.InputStream;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Filter {@link PluginDataObject}s based off configurable parameters.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 16, 2009           jkorman     Initial creation
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Jun 11, 2014  2061     bsteffen    Remove IDecoderGettable
 * Jul 23, 2014  3410     bclement    location changed to floats
 * Sep 09, 2014  3548     mapeters    Improved constructor's error messages.
 * Sep 11, 2014  3548     mapeters    Replaced use of SerializationUtil
 *                                    with JAXBManager.
 * Sep 18, 2014  3627     mapeters    Removed unused getInputStream().
 * Dec 14, 2015  5166     kbisanz     Update logging to use SLF4J
 * Jul 11, 2016  5744     mapeters    Config files moved from edex_static to
 *                                    common_static
 * Nov 02, 2017  6132     mapeters    Support localization overrides for filter file
 *
 * </pre>
 *
 * @author jkorman
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PluginDataObjectFilter extends AbstractObsFilter {

    private static final String ERROR_FMT = "File %s does not exist";

    public static final String FILTERS_DIR = "plugin-filters";

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private String filterConfigFile = null;

    public PluginDataObjectFilter() {
    }

    public PluginDataObjectFilter(String configFile) {
        filterConfigFile = configFile;
        try {
            String configPath = FILTERS_DIR + IPathManager.SEPARATOR
                    + configFile;
            IPathManager manager = PathManagerFactory.getPathManager();
            ILocalizationFile srcFile = manager.getStaticLocalizationFile(
                    LocalizationType.COMMON_STATIC, configPath);
            if (srcFile != null && srcFile.exists()) {
                JAXBManager jaxb = new JAXBManager(PluginDataObjectFilter.class,
                        RadiusFilterElement.class, RectFilterElement.class,
                        StationIdFilterElement.class,
                        WMOHeaderFilterElement.class);
                try (InputStream is = srcFile.openInputStream()) {
                    PluginDataObjectFilter filter = jaxb
                            .unmarshalFromInputStream(
                                    PluginDataObjectFilter.class, is);

                    setFilterElements(filter.getFilterElements());
                    setFilterName(filter.getFilterName());
                }
            } else {
                logger.error(String.format(ERROR_FMT, configPath));
                createDummyFilter();
            }
        } catch (Exception e) {
            logger.error("Error creating filter from " + filterConfigFile, e);
            createDummyFilter();
        }
        logger.info("Filter name = " + getFilterName());
        for (AbstractFilterElement element : getFilterElements()) {
            logger.info("" + element);
        }
    }

    /**
     * Apply the list of filters against given input data.
     */
    @Override
    public PluginDataObject[] filter(PluginDataObject[] reports) {
        int reportCount = 0;
        if (reports != null) {

            for (int i = 0; i < reports.length; i++) {
                PluginDataObject r = null;
                boolean keep = true;
                for (AbstractFilterElement element : filterElements) {
                    r = element.filter(reports[i]);

                    /*
                     * Only allow keep to be set to true. Once true it stays
                     * that way.
                     */
                    if (AbstractObsFilter.INCLUDE_TYPE
                            .equals(element.getFilterType())) {
                        // Did the filter pass?
                        if (r == null) {
                            // If we fail an element, exit now.
                            keep = false;
                            break;
                        }
                    } else if (AbstractObsFilter.EXCLUDE_TYPE
                            .equals(element.getFilterType())) {
                        if (r != null) {
                            /*
                             * There was a match, so we want to remove this
                             * item.
                             */
                            keep = false;
                            // And there's no reason for further checks.
                            break;
                        }
                    }
                }
                if (keep) {
                    reportCount++;
                } else {
                    reports[i] = null;
                }
            }
        }
        if (reportCount == 0) {
            reports = new PluginDataObject[0];
        } else {
            PluginDataObject[] newReports = new PluginDataObject[reportCount];
            int i = 0;
            // Copy in the reports that passed filtering.
            for (PluginDataObject report : reports) {
                if (report != null) {
                    newReports[i] = report;
                    ++i;
                }
            }
            reports = newReports;
        }
        return reports;
    }

    private void createDummyFilter() {
        setFilterName("Created Pass-All filter");
        // Add a dummy element.
        AbstractFilterElement dummy = new AbstractFilterElement() {
            @Override
            public PluginDataObject filter(PluginDataObject report) {
                return report;
            }
        };
        dummy.setFilterType(AbstractObsFilter.INCLUDE_TYPE);
        addFilterElement(dummy);
    }
}
