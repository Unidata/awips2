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
package com.raytheon.edex.plugin.gfe.paraminfo;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParsePosition;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.format.MeasurementParseException;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Unmarshaller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.grid.mapping.DatasetIdMapper;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

import tech.units.indriya.format.SimpleUnitFormat;

/**
 * Lookup class for getting metadata information about grid parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 24, 2010  6372     bphillip    Initial creation
 * Jan 25, 2012  14305    ryu         Read site parameterInfo files
 * Sep 12, 2012  1117     dgilling    Implement method to retrieve all parm
 *                                    names for a given model.
 * Feb 15, 2013  1598     bsteffen    Make GridParamInfoLookup filter on
 *                                    extension.
 * Mar 20, 2013  1774     randerso    Added getModelInfo, 
 *                                    added Dflt if no levels specified
 * Apr 30, 2013  1961     bsteffen    Add ability to disable grib tables.
 * Oct 14, 2013  2473     bsteffen    Remove lookup of deprecated grib files.
 * Jun 05, 2015  4495     njensen     Improved error message
 * Jul 13, 2015  4537     randerso    Removed unused function
 * Jan 27, 2016  5237     tgurney     Remove deprecated LocalizationFile
 *                                    method call
 * Apr 12, 2016  5564     bsteffen    Move localization files to common_static
 * Mar 25, 2019 20790     ryu         Fix NPE that prevented type instantiation.
 * Oct 25, 2021 22848     aghanava    Added null check and unit validation for 
 *                                    paramInfo localization files after 
 *                                    unmarshalling. Moved add Dflt levels to 
 *                                    a separate method. Updated logging to use 
 *                                    slf4j.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GridParamInfoLookup {    
    private static final Logger logger = LoggerFactory
            .getLogger(GridParamInfoLookup.class);

    /** The singleton instance */
    private static GridParamInfoLookup instance;

    /** Parameter information map */
    private Map<String, GridParamInfo> modelParamMap;

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public synchronized static GridParamInfoLookup getInstance() {
        if (instance == null) {
            instance = new GridParamInfoLookup();
        }
        return instance;
    }

    /**
     * Creates a new GribParamInfoLookup instance
     */
    private GridParamInfoLookup() {
        modelParamMap = new HashMap<>();
        init();
    }

    /**
     * Gets the model information based on the specified model
     * 
     * @param mappedModel
     *            The model name
     * @return The parameter information or null if none found
     */
    public GridParamInfo getGridParamInfo(String mappedModel) {
        String paramInfoName = null;
        try {
            paramInfoName = DatasetIdMapper.getInstance().lookupAliasOrNull(
                    mappedModel, "gfeParamInfo");
        } catch (MultipleMappingException e) {
        	logger.warn(e.getLocalizedMessage(), e);
            paramInfoName = e.getArbitraryMapping();
        }

        if (paramInfoName == null) {
            return null;
        }

        return modelParamMap.get(paramInfoName);
    }

    /**
     * Gets the parameter information based on the specified model and parameter
     * name
     * 
     * @param site
     *            The site which is requesting the information
     * @param model
     *            The model name
     * @param parameter
     *            The parameter name
     * @return The parameter information
     */
    public ParameterInfo getParameterInfo(String mappedModel, String parameter) {
        GridParamInfo modelInfo = getGridParamInfo(mappedModel);
        if (modelInfo == null) {
            return null;
        }

        ParameterInfo parameterInfo = modelInfo.getParameterInfo(parameter);

        return parameterInfo;
    }

    public Collection<String> getParmNames(String mappedModel) {
        GridParamInfo modelInfo = getGridParamInfo(mappedModel);
        if (modelInfo == null) {
            return Collections.emptyList();
        }

        List<ParameterInfo> paramInfoList = modelInfo.getGridParamInfo();
        Set<String> parmNames = new HashSet<>();
        for (ParameterInfo info : paramInfoList) {
            parmNames.add(info.getShort_name());
        }

        return parmNames;
    }

    /**
     * Checks grid parameters and adds the Dflt level if no other levels are defined
     * 
     * @param file
     *            The parameterInfo localization file for the model
     * @param gridParamInfo
     *            The grid parameters for the model
     */
    private void addDfltLevels(ILocalizationFile file, GridParamInfo gridParamInfo) {
        for (String parmName : gridParamInfo.getParmNames()) {
            ParameterInfo parameterInfo = gridParamInfo.getParameterInfo(parmName);

            if (parameterInfo != null && parameterInfo.getLevels().isEmpty()) {
                logger.warn("No levels defined for grid parameter '" 
                        + parameterInfo.getShort_name() + "' in file '" 
                        + file.toString() + "'. Adding Dflt level...");
                parameterInfo.getLevels().add("Dflt");
            }
        }
    }

    /**
     * Validates the parameter information for the grid parameters of a model 
     * 
     * @param file
     *            The parameterInfo localization file for the model
     * @param gridParamInfo
     *            The grid parameters for the model
     * @return true if parameter information is valid or false otherwise
     */
    private boolean isValidParameterInfo(ILocalizationFile file, 
            GridParamInfo gridParamInfo) {

        List<ParameterInfo> paramInfoList = gridParamInfo.getGridParamInfo();

        if(paramInfoList != null) {
            for (ParameterInfo parameterInfo : paramInfoList) {
                try {
                    SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII)
                            .parseProductUnit(parameterInfo.getUnits(),
                                    new ParsePosition(0));
                } catch (MeasurementParseException e) {
                    logger.warn("Unable to parse unit '" + parameterInfo.getUnits()
                            + "' for grid parameter '" + parameterInfo.getShort_name() 
                            + "' in '" + file.toString() + "'");
                }
            }

            return true;
        } else {
            logger.error("No gridParameterInfo entries found in " + file.toString());
            return false;
        }
    }

    /**
     * Initializes the grid parameter information
     */
    private void init() {
        Unmarshaller um = null;
        try {
            JAXBContext context = JAXBContext.newInstance(ParameterInfo.class,
                    GridParamInfo.class);
            um = context.createUnmarshaller();
        } catch (JAXBException e) {
            logger.error(e.getLocalizedMessage(), e);
            return;
        }
        IPathManager pm = PathManagerFactory.getPathManager();

        ILocalizationFile[] files = pm.listFiles(
                pm.getLocalSearchHierarchy(LocalizationType.COMMON_STATIC),
                "grid" + IPathManager.SEPARATOR + "parameterInfo",
                new String[] { ".xml" }, true, true);

        for (ILocalizationFile file : files) {
            try (InputStream is = file.openInputStream()) {
                GridParamInfo paramInfo = (GridParamInfo) um.unmarshal(is);
                Path path = Paths.get(file.getPath());
                String key = path.getFileName().toString().replace(".xml", "");

                if(isValidParameterInfo(file, paramInfo)) {
                    if (!modelParamMap.containsKey(key)) {
                        addDfltLevels(file, paramInfo);
                        modelParamMap.put(key, paramInfo);
                    }
                }
            } catch (JAXBException | IOException | LocalizationException e) {
                logger.error("Error unmarshalling grid parameter information from file "
                        + file.getPath(), e);
            }
        }
    }
}
