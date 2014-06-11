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
package com.raytheon.edex.plugin.binlightning.filter;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;

/**
 * Geographic filtering utility for lightning data. Configured in localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2014 3226       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 * @see {@link GeoFilterParser}
 */
public class LightningGeoFilter {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(LightningGeoFilter.class);

    public static final String LOCALIZATION_FILTER_DIR = "binlightning/filters";

    private static final GeometryFactory GEOM_FACTORY = new GeometryFactory();

    /**
     * map of lightning source names to filter geometries
     */
    private static final Map<String, Collection<PreparedGeometry>> geometryMap = new ConcurrentHashMap<String, Collection<PreparedGeometry>>(
            4);

    private static volatile boolean initialized = false;

    /**
     * read filter configuration files from localization. Threadsafe and
     * idempotent
     */
    private static void initialize() {
        synchronized (geometryMap) {
            if (!initialized) {
                IPathManager pathMgr = PathManagerFactory.getPathManager();

                LocalizationFile[] files = pathMgr.listFiles(pathMgr
                        .getLocalSearchHierarchy(LocalizationType.EDEX_STATIC),
                        LOCALIZATION_FILTER_DIR, new String[] { ".xml" }, true,
                        true);

                for (LocalizationFile file : files) {
                    Collection<PreparedGeometry> filters = getFilterGeometries(file);
                    if (!filters.isEmpty()) {
                        String bareName = getFileNameWithoutExtension(file
                                .getName());
                        geometryMap.put(bareName.toLowerCase(), filters);
                    }
                }
                initialized = true;
            }
        }
    }

    /**
     * Parse filter geometries from filter config file
     * 
     * @param file
     * @return empty list on error
     */
    private static Collection<PreparedGeometry> getFilterGeometries(
            LocalizationFile file) {
        Collection<PreparedGeometry> rval;
        InputStream in = null;
        try {
            in = file.openInputStream();
            GeoFilterResult result = GeoFilterParser.parse(in);
            if (result.hasErrors()) {
                for (GeoFilterException e : result.getErrors()) {
                    log.error(e.getLocalizedMessage(), e);
                }
                log.warn("Filter parsing included errors, filters will be incomplete");
            }
            return result.getFilters();
        } catch (JAXBException e) {
            log.error("Unable to parse filter file: " + file.getName(), e);
            rval = Collections.emptyList();
        } catch (LocalizationException e) {
            log.error("Unable to open filter file: " + file.getName(), e);
            rval = Collections.emptyList();
        } catch (SerializationException e) {
            log.error("Unable to parse filter file: " + file.getName(), e);
            rval = Collections.emptyList();
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    log.error(
                            "Problem closing localization file: "
                                    + file.getName(), e);
                }
            }
        }
        return rval;
    }

    /**
     * Strip the bare file name from path. Removes path and file suffix (if
     * either exist)
     * 
     * @param path
     * @return
     */
    private static String getFileNameWithoutExtension(String path) {
        int slashIndex = path.lastIndexOf(IPathManager.SEPARATOR);
        int start = slashIndex < 0 ? 0 : slashIndex + 1;
        int dotIndex = path.lastIndexOf('.');
        if (dotIndex < 0) {
            return path.substring(start);
        } else {
            return path.substring(start, dotIndex);
        }
    }

    /**
     * Applies data source filter to strikes. Source is determined from sample
     * strike. If no filter exists for source, provided collection is returned.
     * 
     * @param strikes
     * @return
     */
    public static Collection<LightningStrikePoint> filterStrikes(
            Collection<LightningStrikePoint> strikes) {
        if (!initialized) {
            initialize();
        }
        Collection<LightningStrikePoint> rval = strikes;
        if (!strikes.isEmpty()) {
            LightningStrikePoint sample = strikes.iterator().next();
            String source = BinLightningRecord.getDataSource(sample);
            Collection<PreparedGeometry> filter = geometryMap.get(source
                    .toLowerCase());
            if (filter != null && !filter.isEmpty()) {
                rval = new ArrayList<LightningStrikePoint>(strikes.size());
                for (LightningStrikePoint strike : strikes) {
                    if (passesFilter(filter, strike)) {
                        rval.add(strike);
                    }
                }
            }
        }
        return rval;
    }

    /**
     * @param filter
     * @param strike
     * @return true if strike is located in any of the filter geometries
     */
    private static boolean passesFilter(Collection<PreparedGeometry> filter,
            LightningStrikePoint strike) {
        boolean passes = false;
        Point p = GEOM_FACTORY.createPoint(new Coordinate(
                strike.getLongitude(), strike.getLatitude()));
        for (PreparedGeometry g : filter) {
            /* covers instead of contains to include points at boundary */
            if (g.covers(p)) {
                passes = true;
                break;
            }
        }
        return passes;
    }

    /**
     * Create a BinLightningRecord after applying data source filter to list of
     * strikes
     * 
     * @see #filterStrikes(Collection)
     * @param strikes
     * @return
     */
    public static BinLightningRecord createFilteredRecord(
            Collection<LightningStrikePoint> strikes) {
        return new BinLightningRecord(filterStrikes(strikes));
    }

}
