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
package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * Color manager for Hydro
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation.
 * 10/10/2013   2361       njensen     Use JAXBManager for XML
 * 07/06/2018   6885       mduff       cleanup.
 * 
 * </pre>
 * 
 */
public class HydroColorManager extends ColorManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroColorManager.class);

    private static final SingleTypeJAXBManager<DefaultColorScaleXML> JAXB = SingleTypeJAXBManager
            .createWithoutException(DefaultColorScaleXML.class);

    /** Instance of this class */
    private static final HydroColorManager instance = new HydroColorManager();

    /**
     * The application name.
     */
    public static final String APPLICATION_NAME = "hydroview";

    public static final int MISSING_INDEX = 0;

    public static final int MISSING_STAGE_INDEX = 1;

    public static final int NO_FLOOD_INDEX = 2;

    public static final int NEAR_FLOOD_INDEX = 3;

    public static final int FLOOD_INDEX = 4;

    /**
     * HydroviewDefaultcolorScale.xml
     */
    private volatile DefaultColorScaleXML defaultXml = null;

    private ILocalizationPathObserver fileObserver;

    private HydroColorManager() {
        applicationName = APPLICATION_NAME;
    }

    public static final HydroColorManager getInstance() {
        return instance;
    }

    /**
     * Read in the Default Color Scale file.
     * 
     * @return The DefaultColorScaleXML object
     */
    public NamedColorSetGroup getDefaultColorSetGroup() {
        NamedColorSetGroup namedColorSetGroup = new NamedColorSetGroup();

        readDataFile();

        List<ColorDataClassXML> dataClassList = defaultXml
                .getColorDataClassList();

        for (ColorDataClassXML cdc : dataClassList) {
            String dbColorUseName = cdc.getDbColorUseName();
            String colorUseName = cdc.getColorUseName();

            colorNameMap.put(dbColorUseName, colorUseName);

            List<ColorThresholdXML> threshList = cdc.getThresholdList();

            List<Double> thresholdValueArray = new ArrayList<>();
            List<String> thresholdColorArray = new ArrayList<>();

            for (int i = 0; i < threshList.size(); i++) {
                thresholdValueArray.add(threshList.get(i).getValue());
                thresholdColorArray.add(threshList.get(i).getColor());
            }

            String missingColorName = thresholdColorArray.get(0);
            String defaultColorName = thresholdColorArray.get(1);

            // Create arrays
            String[] colorArray = thresholdColorArray
                    .toArray(new String[thresholdColorArray.size()]);
            double[] colorValueArray = new double[thresholdValueArray.size()];
            for (int i = 0; i < thresholdValueArray.size(); i++) {
                colorValueArray[i] = thresholdValueArray.get(i);
            }

            NamedColorUseSet namedColorUseSet = new NamedColorUseSet(
                    dbColorUseName, colorUseName, colorValueArray, colorArray,
                    missingColorName, defaultColorName, 0);

            namedColorSetGroup.addNamedColorUseSet(namedColorUseSet);
        }

        return namedColorSetGroup;
    }

    private void readDataFile() {
        String filename = "hydro" + IPathManager.SEPARATOR
                + "HydroviewDefaultColorScale.xml";

        try {
            IPathManager pm = PathManagerFactory.getPathManager();

            String path = pm
                    .getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                            LocalizationLevel.BASE), filename)
                    .getAbsolutePath();

            defaultXml = JAXB.unmarshalFromXmlFile(path);

            if (fileObserver == null) {
                fileObserver = new ILocalizationPathObserver() {
                    @Override
                    public void fileChanged(ILocalizationFile file) {
                        readDataFile();
                    }
                };

                pm.addLocalizationPathObserver(filename, fileObserver);
            }
        } catch (Exception e) {
            statusHandler.error("Error reading " + filename, e);
        }
    }

    @Override
    public String getApplicationName() {
        return applicationName;
    }
}
