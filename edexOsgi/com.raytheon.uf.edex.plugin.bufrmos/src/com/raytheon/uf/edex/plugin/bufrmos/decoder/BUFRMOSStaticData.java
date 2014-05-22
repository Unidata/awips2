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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.plugin.bufrmos.BufrMosSeparator;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20020221            861 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRMOSStaticData {

    private Log logger = LogFactory.getLog(getClass());

    private static final String WMOHDR_FMT = "%s%s%s%s%02d_%s";

    private static BUFRMOSStaticData factoryInstance = null;

    public static final Integer BUFRMOS_AVN = 1000;

    public static final Integer BUFRMOS_ETA = 1001;

    public static final Integer BUFRMOS_GFS = 1002;

    public static final Integer BUFRMOS_HPC = 1003;

    public static final Integer BUFRMOS_LAMP = 1004;

    public static final Integer BUFRMOS_MRF = 1005;

    public static final Integer BUFRMOS_NGM = 1006;

    public static final String MODEL_AVN = "AVN";

    public static final String MODEL_ETA = "ETA";

    public static final String MODEL_GFS = "GFS";

    public static final String MODEL_HPC = "HPC";

    public static final String MODEL_LAMP = "LAMP";

    public static final String MODEL_MRF = "MRF";

    public static final String MODEL_NGM = "NGM";

    private Map<String, List<BufrMOSElement>> elementMap = new HashMap<String, List<BufrMOSElement>>();
    {
        elementMap.put(MODEL_AVN, new ArrayList<BufrMOSElement>());
        elementMap.put(MODEL_ETA, new ArrayList<BufrMOSElement>());
        elementMap.put(MODEL_GFS, new ArrayList<BufrMOSElement>());
        elementMap.put(MODEL_HPC, new ArrayList<BufrMOSElement>());
        elementMap.put(MODEL_LAMP, new ArrayList<BufrMOSElement>());
        elementMap.put(MODEL_MRF, new ArrayList<BufrMOSElement>());
        elementMap.put(MODEL_NGM, new ArrayList<BufrMOSElement>());
    }

    private Map<String, Map<String, BufrMOSElement>> paramsMap = new HashMap<String, Map<String, BufrMOSElement>>();
    {
        paramsMap.put(MODEL_AVN, new HashMap<String, BufrMOSElement>());
        paramsMap.put(MODEL_ETA, new HashMap<String, BufrMOSElement>());
        paramsMap.put(MODEL_GFS, new HashMap<String, BufrMOSElement>());
        paramsMap.put(MODEL_HPC, new HashMap<String, BufrMOSElement>());
        paramsMap.put(MODEL_LAMP, new HashMap<String, BufrMOSElement>());
        paramsMap.put(MODEL_MRF, new HashMap<String, BufrMOSElement>());
        paramsMap.put(MODEL_NGM, new HashMap<String, BufrMOSElement>());
    }

    private Properties fileNameTypes;

    private boolean loaded = false;

    /**
     * Create an instance of this class.
     */
    private BUFRMOSStaticData() {
        populateMappings();
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
     * Has the static data been loaded?
     * 
     * @return Has the static data been loaded?
     */
    public boolean isLoaded() {
        return loaded;
    }

    /**
     * Map the WMO header for some data to a model name.
     * 
     * @return The model name associated with the WMO header within the
     *         specified separator.
     */
    public String getMOSType(BufrMosSeparator separator) {
        WMOHeader hdr = separator.getWmoHeader();

        String wmoHdr = String.format(WMOHDR_FMT, hdr.getT1(), hdr.getT2(), hdr
                .getA1(), hdr.getA2(), hdr.getIi(), hdr.getCccc());

        return fileNameTypes.getProperty(wmoHdr);
    }

    /**
     * Get an Iterator to the underlying element map for a specified model.
     * 
     * @param model
     *            A model name.
     * @return An iterator to the element map for a specific model. A null
     *         reference is returned if no map exists for the specified model.
     */
    public Iterator<BufrMOSElement> getElementIterator(String model) {

        Iterator<BufrMOSElement> it = null;

        List<BufrMOSElement> list = elementMap.get(model);

        if (list != null) {
            it = list.iterator();
        }
        return it;
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

        Map<String, BufrMOSElement> map = paramsMap.get(model);

        if (map != null) {
            element = map.get(parameter);
        }

        return element;
    }

    /**
     * Populate the WMO Header to model type mappings. Populate the [model type
     * - descriptor - dataitem name] mappings.
     */
    private void populateMappings() {
        InputStream strm = null;
        BufferedReader bf = null;

        fileNameTypes = new Properties();

        try {
            try {
                strm = this.getClass().getResourceAsStream(
                        "/res/bufrtables/mosFilenames.properties");

                if (strm != null) {
                    bf = new BufferedReader(new InputStreamReader(strm));
                    fileNameTypes.load(bf);
                    loaded = true;
                } else {
                    loaded = false;
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }

        // ---------------------------------------
        // Model|Descrip | Index|Mnemonic
        // ---------------------------------------
        // AVN|0 12 021| 12|maxTemp24Hour
        // ---------------------------------------
        try {
            strm = this.getClass().getResourceAsStream(
                    "/res/bufrtables/MnemonicDescriptorMapping.txt");

            if (strm != null) {
                bf = new BufferedReader(new InputStreamReader(strm));
                String line = null;
                while ((line = bf.readLine()) != null) {
                    if (line.length() > 0) {
                        if (line.charAt(0) != '#') {
                            String[] ss = line.split("\\|");
                            if ((ss != null) && (ss.length == 4)) {
                                Integer index = Integer.parseInt(ss[2].trim());

                                BufrMOSElement element = BufrMOSElement
                                        .createElement(ss[0].trim(), ss[3]
                                                .trim(), index, ss[1].trim());
                                if (element != null) {
                                    String modelKey = ss[0].trim();

                                    List<BufrMOSElement> list = elementMap
                                            .get(modelKey);

                                    if (list != null) {
                                        list.add(element);
                                    }
                                    Map<String, BufrMOSElement> hp = paramsMap
                                            .get(modelKey);
                                    if (hp != null) {
                                        hp.put(element.getElementName(),
                                                element);
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                logger
                        .error("Could not open descriptor mapping in BUFRMOSStaticData");
            }
        } catch (IOException ioe) {
            logger.error("Static data load failed", ioe);
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    logger.error("May have failed to close resource", ioe);
                }
            }
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
        } else if (MODEL_NGM.equals(model)) {
            modelType = BUFRMOS_NGM;
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
        } else if (BUFRMOS_NGM.equals(modelType)) {
            modelName = MODEL_NGM;
        }
        return modelName;
    }

}
