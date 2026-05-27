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
package com.raytheon.uf.viz.grid.radar;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.grid.xml.ParameterList;
import com.raytheon.viz.grid.xml.ParameterMapping;

/**
 * Structure for retrieving radar product code to grib parameter abbrev mappings
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------
 * Mar 22, 2010  4473     rjpeter   Initial creation
 * Nov 07, 2361  2361     njensen   Use JAXBManager for XML
 * Aug 15, 2017  6332     bsteffen  Move to viz.grid.radar plugin
 * Jan 09, 2024  2036695  mapeters  Support multiple params using the same
 *                                  product code (for SRM/V)
 * Jun 17, 2024  2037092  mapeters  Move instance into LazyHolder
 *
 * </pre>
 *
 * @author rjpeter
 */
public class RadarProductCodeMapping {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarProductCodeMapping.class);

    private static final String LOC_PATH = LocalizationUtil
            .join("parameterMapping", "radar", "RadarProductCodes.xml");

    /**
     * Initialization-on-demand holder to prevent instantiation until
     * getInstance() is actually called. Specifically needed for unit tests to
     * be able to mock getInstance().
     */
    private static class LazyHolder {
        private static final RadarProductCodeMapping instance = new RadarProductCodeMapping();
    }

    private Map<String, List<Integer>> parameterMappings = new HashMap<>();

    private Map<Integer, Set<String>> pCodeMappings = new HashMap<>();

    public static RadarProductCodeMapping getInstance() {
        return LazyHolder.instance;
    }

    private RadarProductCodeMapping() {
        IPathManager pm = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> locFiles = pm
                .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                        LOC_PATH);

        // Incrementally load param -> product code map
        try {
            JAXBManager jaxb = new JAXBManager(ParameterList.class);
            LocalizationLevel[] levels = { LocalizationLevel.BASE,
                    LocalizationLevel.SITE, LocalizationLevel.USER };
            for (LocalizationLevel level : levels) {
                ILocalizationFile locFile = locFiles.get(level);
                if (locFile != null) {
                    loadParamToProductCodeMappings(locFile, jaxb);
                }
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Error occurred loading radar product code to grid parameter mappings from file: "
                            + LOC_PATH,
                    e);
        }
        // Make unmodifiable
        parameterMappings = parameterMappings.entrySet().stream()
                .collect(Collectors.toUnmodifiableMap(e -> e.getKey(),
                        e -> Collections.unmodifiableList(e.getValue())));

        // Populate reverse product code -> param map from the above map
        for (Entry<String, List<Integer>> entry : parameterMappings
                .entrySet()) {
            String paramAbbrev = entry.getKey();
            for (Integer productCode : entry.getValue()) {
                pCodeMappings
                        .computeIfAbsent(productCode, pc -> new HashSet<>())
                        .add(paramAbbrev);
            }
        }
        // Make unmodifiable
        pCodeMappings = pCodeMappings.entrySet().stream()
                .collect(Collectors.toUnmodifiableMap(e -> e.getKey(),
                        e -> Collections.unmodifiableSet(e.getValue())));
    }

    private void loadParamToProductCodeMappings(ILocalizationFile fileToLoad,
            JAXBManager jaxb)
            throws SerializationException, IOException, LocalizationException {
        ParameterList parameterList;
        try (InputStream is = fileToLoad.openInputStream()) {
            parameterList = jaxb.unmarshalFromInputStream(ParameterList.class,
                    is);
        }

        for (ParameterMapping parameter : parameterList.getParameters()) {
            parameterMappings.put(parameter.getAbbrev(),
                    parameter.getProductCodes());
        }
    }

    public List<Integer> getProductCodesForAbbrev(String abbrev) {
        return parameterMappings.get(abbrev);
    }

    public Set<String> getParameterAbbrevs() {
        return parameterMappings.keySet();
    }

    public Set<String> getParameterAbbrevsForProductCode(Integer productCode) {
        return pCodeMappings.get(productCode);
    }
}
