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
package com.raytheon.viz.grid.util;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.grid.Activator;
import com.raytheon.viz.grid.xml.ParameterList;
import com.raytheon.viz.grid.xml.ParameterMapping;

/**
 * Structure for retrieving radar product code to grib parameter abbrev mappings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2010 #4473      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class RadarProductCodeMapping {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RadarProductCodeMapping.class);
    private static RadarProductCodeMapping instance;

    private Map<String, List<Integer>> parameterMappings = new HashMap<String, List<Integer>>();

    private Map<Integer, String> pCodeMappings = new HashMap<Integer, String>();

    public static RadarProductCodeMapping getInstance() {
        if (instance == null) {
            instance = new RadarProductCodeMapping();
        }

        return instance;
    }

    private RadarProductCodeMapping() {
        IPathManager pm = PathManagerFactory.getPathManager();
        File baseFile = pm.getFile(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE),
                "/parameterMapping/radar/RadarProductCodes.xml");

        File siteFile = pm.getFile(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.SITE),
                "/parameterMapping/radar/RadarProductCodes.xml");

        File userFile = pm.getFile(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER),
                "/parameterMapping/radar/RadarProductCodes.xml");

        try {
            loadParameters(baseFile);

            if (siteFile.exists()) {
                loadParameters(siteFile);
            }
            if (userFile.exists()) {
                loadParameters(userFile);
            }
        } catch (Exception e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error occurred loading radar product code to grid parameter mappings",
                    e);
        }
    }

    private void loadParameters(File fileToLoad) throws SerializationException {
        ParameterList parameterList = (ParameterList) SerializationUtil
                .jaxbUnmarshalFromXmlFile(fileToLoad);
        for (ParameterMapping parameter : parameterList.getParameters()) {
            // print message for overwrite?
            parameterMappings.put(parameter.getAbbrev(),
                    parameter.getProductCodes());
            for (Integer productCode : parameter.getProductCodes()) {
                pCodeMappings.put(productCode, parameter.getAbbrev());
            }
        }
    }

    public List<Integer> getProductCodesForAbbrev(String abbrev) {
        return parameterMappings.get(abbrev);
    }

    public Set<String> getParameterAbbrevs() {
        return parameterMappings.keySet();
    }

    public String getParameterAbbrev(Integer productCode) {
        return pCodeMappings.get(productCode);
    }
}
