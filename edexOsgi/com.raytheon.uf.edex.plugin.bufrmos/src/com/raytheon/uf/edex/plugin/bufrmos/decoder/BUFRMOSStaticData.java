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
package com.raytheon.uf.edex.plugin.bufrmos.decoder;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.plugin.bufrmos.BufrMosSeparator;

/**
 * BUFRMOS decoder static data loader class. Reads the mapping files for WMO
 * header to model name and BUFR descriptor to parameter name.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2002  861       jkorman      Initial creation
 * Jul 14, 2015  4543      dgilling     Refactor code.
 * Feb 09, 2016  5283      nabowle      Remove NGM MOS support.
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
public class BUFRMOSStaticData {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static BUFRMOSStaticData factoryInstance = null;

    private static final String WMOHDR_FMT = "%s%s%s%s%02d_%s";

    public static final Integer BUFRMOS_AVN = 1000;

    public static final Integer BUFRMOS_ETA = 1001;

    public static final Integer BUFRMOS_GFS = 1002;

    public static final Integer BUFRMOS_HPC = 1003;

    public static final Integer BUFRMOS_LAMP = 1004;

    public static final Integer BUFRMOS_MRF = 1005;

    public static final String MODEL_AVN = "AVN";

    public static final String MODEL_ETA = "ETA";

    public static final String MODEL_GFS = "GFS";

    public static final String MODEL_HPC = "HPC";

    public static final String MODEL_LAMP = "LAMP";

    public static final String MODEL_MRF = "MRF";

    private final Map<String, Map<String, BufrMOSElement>> elementMap;

    private final Map<String, Map<Integer, BufrMOSElement>> parameterMap;

    private final Properties fileNameTypes;

    /**
     * Create an instance of this class.
     */
    private BUFRMOSStaticData() {
        this.fileNameTypes = getWMOHeaderMappings();

        List<String> modelNames = Arrays.asList(MODEL_AVN, MODEL_ETA,
                MODEL_GFS, MODEL_GFS, MODEL_HPC, MODEL_LAMP, MODEL_MRF);

        this.parameterMap = new HashMap<>();
        for (String modelName : modelNames) {
            this.parameterMap.put(modelName,
                    new HashMap<Integer, BufrMOSElement>());
        }

        this.elementMap = new HashMap<>();
        for (String modelName : modelNames) {
            this.elementMap.put(modelName,
                    new HashMap<String, BufrMOSElement>());
        }

        getParameterMappings();
    }

    /**
     * Return the single instance of this class.
     *
     * @return The BUFRMOSStaticData instance.
     */
    public static synchronized BUFRMOSStaticData getInstance() {
        if (factoryInstance == null) {
            factoryInstance = new BUFRMOSStaticData();
        }
        return factoryInstance;
    }

    /**
     * Map the WMO header for some data to a model name.
     *
     * @return The model name associated with the WMO header within the
     *         specified separator.
     */
    public String getMOSType(BufrMosSeparator separator) {
        WMOHeader hdr = separator.getWmoHeader();

        String wmoHdr = String.format(WMOHDR_FMT, hdr.getT1(), hdr.getT2(),
                hdr.getA1(), hdr.getA2(), hdr.getIi(), hdr.getCccc());

        return fileNameTypes.getProperty(wmoHdr);
    }

    /**
     * Get the BufrMOSElement definition for a specified parameter.
     *
     * @param model
     *            The model being used i.e. AVN.
     * @param parameter
     *            A parameter from the model.
     * @return The BufrMOSElement associated with a given [model,parameter]. A
     *         null reference is returned if no association exists.
     */
    public BufrMOSElement getElementInfo(String model, String parameter) {
        BufrMOSElement element = null;
        Map<String, BufrMOSElement> map = elementMap.get(model);
        if (map != null) {
            element = map.get(parameter);
        }
        return element;
    }

    private Properties getWMOHeaderMappings() {
        Properties fileHeaderMappings = new Properties();
        try (InputStream input = getClass().getResourceAsStream(
                "/res/bufrtables/mosFilenames.properties")) {
            fileHeaderMappings.load(input);
        } catch (IOException e) {
            statusHandler
                    .error("Could not load bufrmos WMO header to model mappings file.",
                            e);
        }
        return fileHeaderMappings;
    }

    private void getParameterMappings() {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                getClass().getResourceAsStream(
                        "/res/bufrtables/MnemonicDescriptorMapping.txt")))) {
            String line = null;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty() && line.charAt(0) != '#') {
                    // ---------------------------------------
                    // Model|Descrip | Index|Mnemonic
                    // ---------------------------------------
                    // AVN|0 12 021| 12|maxTemp24Hour
                    // ---------------------------------------

                    String[] tokens = line.split("\\|");
                    if ((tokens != null) && (tokens.length == 4)) {
                        Integer index = Integer.parseInt(tokens[2].trim());
                        BufrMOSElement element = BufrMOSElement.createElement(
                                tokens[0].trim(), tokens[3].trim(), index,
                                tokens[1].trim());

                        String modelName = element.getModelName();
                        Map<Integer, BufrMOSElement> modelParamMapping = parameterMap
                                .get(modelName);
                        if (modelParamMapping != null) {
                            modelParamMapping.put(element.getDescriptor(),
                                    element);
                        }

                        Map<String, BufrMOSElement> modelElementMapping = elementMap
                                .get(modelName);
                        if (modelElementMapping != null) {
                            modelElementMapping.put(element.getElementName(),
                                    element);
                        }
                    }
                }
            }
        } catch (IOException e) {
            statusHandler.error(
                    "Could not load bufrmos parameter mappings file.", e);
        }
    }

    /**
     * Convert a model name into its ordinal value.
     *
     * @param model
     *            A model name as a String.
     * @return The associated ordinal value for the model. Returns a null if no
     *         association exists.
     */
    public static final Integer getModelType(String model) {
        Integer modelType = null;
        if (MODEL_AVN.equals(model)) {
            modelType = BUFRMOS_AVN;
        } else if (MODEL_ETA.equals(model)) {
            modelType = BUFRMOS_ETA;
        } else if (MODEL_GFS.equals(model)) {
            modelType = BUFRMOS_GFS;
        } else if (MODEL_HPC.equals(model)) {
            modelType = BUFRMOS_HPC;
        } else if (MODEL_LAMP.equals(model)) {
            modelType = BUFRMOS_LAMP;
        } else if (MODEL_MRF.equals(model)) {
            modelType = BUFRMOS_MRF;
        }
        return modelType;
    }

    /**
     * Convert a model ordinal value to its String name .
     *
     * @param modelType
     *            A model ordinal value.
     * @return The model name associated with a given ordinal value. Returns a
     *         null if no association exists.
     */
    public static final String getModelName(Integer modelType) {
        String modelName = null;
        if (BUFRMOS_AVN.equals(modelType)) {
            modelName = MODEL_AVN;
        } else if (BUFRMOS_ETA.equals(modelType)) {
            modelName = MODEL_ETA;
        } else if (BUFRMOS_GFS.equals(modelType)) {
            modelName = MODEL_GFS;
        } else if (BUFRMOS_HPC.equals(modelType)) {
            modelName = MODEL_HPC;
        } else if (MODEL_LAMP.equals(modelType)) {
            modelName = MODEL_LAMP;
        } else if (BUFRMOS_MRF.equals(modelType)) {
            modelName = MODEL_MRF;
        }
        return modelName;
    }

    public Map<Integer, BufrMOSElement> getModelParamterMappings(
            String modelName) {
        Map<Integer, BufrMOSElement> map = parameterMap.get(modelName);
        if (map == null) {
            map = Collections.emptyMap();
        }
        return Collections.unmodifiableMap(map);
    }
}
