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
package com.raytheon.edex.plugin.grib.util;

import java.io.File;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grib.util.GridModel;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Lookup class for getting metadata information about grib parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2010 #6372      bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GribParamInfoLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribParamInfoLookup.class);

    /** The singleton instance */
    private static GribParamInfoLookup instance;

    /** Parameter information map */
    private Map<String, GribParamInfo> modelParamMap;

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public synchronized static GribParamInfoLookup getInstance() {
        if (instance == null) {
            instance = new GribParamInfoLookup();
        }
        return instance;
    }

    /**
     * Creates a new GribParamInfoLookup instance
     */
    private GribParamInfoLookup() {
        modelParamMap = new HashMap<String, GribParamInfo>();
        init();
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
    public synchronized ParameterInfo getParameterInfo(String mappedModel,
            String parameter) {

        GridModel gridModel = GribModelLookup.getInstance().getModelByName(
                mappedModel);

        if (gridModel == null) {
            return null;
        }

        GribParamInfo modelInfo = modelParamMap.get(gridModel.getParamInfo());
        if (modelInfo == null) {
            return null;
        }

        ParameterInfo parameterInfo = modelInfo.getParameterInfo(parameter);
        if (parameterInfo == null) {
            return null;
        }
        return parameterInfo;
    }

    public synchronized List<TimeRange> getParameterTimes(String mappedModel,
            Date refTime) {
        GridModel gridModel = GribModelLookup.getInstance().getModelByName(
                mappedModel);

        if (gridModel == null) {
            return Collections.emptyList();
        }

        GribParamInfo modelInfo = modelParamMap.get(gridModel.getParamInfo());
        if (modelInfo == null) {
            return Collections.emptyList();
        }
        return modelInfo.getAvailableTimes(refTime);
    }

    /**
     * Initializes the grib parameter information
     */
    private void init() {
        IPathManager pm = PathManagerFactory.getPathManager();
        File infoDir = pm.getFile(pm.getContext(LocalizationType.EDEX_STATIC,
                LocalizationLevel.BASE), "/grib/parameterInfo");
        File[] files = infoDir.listFiles();

        for (File file : files) {
            try {
                GribParamInfo paramInfo = (GribParamInfo) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(file);
                modelParamMap
                        .put(file.getName().replace(".xml", ""), paramInfo);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error unmarshalling grib parameter information", e);
            }
        }
    }
}
