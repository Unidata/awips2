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

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

public class HydroColorManager extends ColorManager {
    /** Instance of this class */
    private static HydroColorManager instance = null;
    
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
    protected DefaultColorScaleXML defaultXml = null;

    private HydroColorManager() {
        applicationName = APPLICATION_NAME;
    }
    
    public static HydroColorManager getInstance() {
        if (instance == null) {
            instance = new HydroColorManager();
        }
        
        return instance;
    }

    /**
     * Read in the Default Color Scale file.
     * 
     * @return
     *      The DefaultColorScaleXML object
     */
    public NamedColorSetGroup getDefaultColorSetGroup() {
        String filename = "hydro/HydroviewDefaultColorScale.xml";
        DefaultColorScaleXML defaultXmltmp = null;
        NamedColorSetGroup namedColorSetGroup = new NamedColorSetGroup();
        
        try {
            IPathManager pm = PathManagerFactory.getPathManager();

            String path = pm.getFile(
                    pm.getContext(LocalizationType.COMMON_STATIC,
                            LocalizationLevel.BASE), filename)
                    .getAbsolutePath();

            defaultXmltmp = (DefaultColorScaleXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(path.toString());
            defaultXml = defaultXmltmp;
        } catch (Exception e) {
            // e.printStackTrace();
            System.err.println("No configuration file found");
        }
        
        ArrayList<ColorDataClassXML> dataClassList = defaultXmltmp.getColorDataClassList();
        
        for (ColorDataClassXML cdc: dataClassList) {
            String dbColorUseName = cdc.getDbColorUseName();
            String colorUseName = cdc.getColorUseName();
            insertColorUseStringIntoHashMap(colorUseName, dbColorUseName);
            
            colorNameMap.put(dbColorUseName, colorUseName);
            
            ArrayList<ColorThresholdXML> threshList = cdc.getThresholdList();
            
            ArrayList<Double> thresholdValueArray = new ArrayList<Double>();
            ArrayList<String> thresholdColorArray = new ArrayList<String>();
            
            for (int i = 0; i < threshList.size(); i++) {
                thresholdValueArray.add(threshList.get(i).getValue());
                thresholdColorArray.add(threshList.get(i).getColor()); 
            }
            
            String missingColorName = thresholdColorArray.get(0);
            String defaultColorName = thresholdColorArray.get(1);
            
            // Create arrays
            String[] colorArray = thresholdColorArray.toArray(new String[thresholdColorArray.size()]);
            double[] colorValueArray = new double[thresholdValueArray.size()];
            for (int i = 0; i < thresholdValueArray.size(); i++) {
                colorValueArray[i] = thresholdValueArray.get(i);
            }
            
            NamedColorUseSet namedColorUseSet = new NamedColorUseSet(dbColorUseName,
                    colorUseName, colorValueArray, colorArray,
                    missingColorName, defaultColorName, 0);
            
            namedColorSetGroup.addNamedColorUseSet(namedColorUseSet);
        }
        
        return namedColorSetGroup;
    }

    private void insertColorUseStringIntoHashMap(String colorUseString,
            String databaseColorUseString) {
        colorNameMap.put(colorUseString,
                databaseColorUseString);
        colorNameMap.put(databaseColorUseString,
                colorUseString);
    }    

    @Override
    public String getApplicationName() {
        return applicationName;
    }
}
