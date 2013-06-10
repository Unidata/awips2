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
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.viz.core.style.image.ImagePreferences;

/**
 * FFMPColor Utility
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/29/09      2152       D. Hladky   Initial release
 * 05/21/12		DR 14833    G. Zhang    Error handling for invalid cmap 
 * Apr 26, 2013 1954        bsteffen    Minor code cleanup throughout FFMP.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPColorUtils {

    private ColorMapParameters colormapparams = null;

    private FIELDS field = null;

    private boolean tableLoad = false;

    private double time = 0.0;

    private ArrayList<String> fileArray = new ArrayList<String>();

    private TreeMap<Double, String> hourColorMapMap = new TreeMap<Double, String>();    
    
    // DR 14833: replacing the one in the constructor
    private StyleRule sr = null;
    
    // DR 14833: used when no colormap found
    private static final String DEFAULT_COLORMAP  = "ffmp/qpe";
    
    // DR 14833: used when paramname not matching colormap name found
    private static final String DEFAULT_PARAMNAME = "qpe";

    /**
     * Set up FFMP Color maps
     * 
     * @param field
     * @param time
     * @param tableLoad
     */
    public FFMPColorUtils(FIELDS field, double time, boolean tableLoad) {

        this.field = field;
        this.time = time;
        this.tableLoad = tableLoad;
        this.colormapparams = null;

        // LocalizationFile[] files = ColorMapLoader.listColorMapFiles();
        // for (LocalizationFile file : files) {
        // String fn = file.getName();
        // if (fn.startsWith("colormaps/ffmp/qpe"))
        // {
        // System.out.println(file.getName());
        // String hour = fn.s
        // }
        //
        // }

//        StyleRule sr = null;// DR 14833 replaced by a instance field
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, getMatchCriteria());
        } catch (VizStyleException e) {
            e.printStackTrace();
        }
        String colormapfile = ((ImagePreferences) sr.getPreferences())
                .getDefaultColormap();

        IColorMap cxml = null;

        try {
            cxml = ColorMapLoader.loadColorMap(colormapfile);
        } catch (VizException e) {
            e.printStackTrace();
        }

        if(cxml == null) cxml = getDefaultColorMap(); // DR 14833: load the default map 
        ColorMap colorMap = new ColorMap(colormapfile, (ColorMap) cxml);
        colormapparams = new ColorMapParameters();
        colormapparams.setColorMap(colorMap);
        colormapparams.setDisplayUnit(((ImagePreferences) sr.getPreferences())
                .getDisplayUnits());
        colormapparams.setDataMapping(((ImagePreferences) sr.getPreferences())
                .getDataMapping());

        colormapparams.setColorMapMin(0);
        colormapparams.setColorMapMax(255);
    }

    public ColorMapParameters getColorMapParameters() {
        return colormapparams;
    }

    /**
     * Gets the ColorMap
     * 
     * @return
     */
    private ColorMap getColorMap() {
        return (ColorMap) colormapparams.getColorMap();
    }

    /**
     * convert color
     * 
     * @param color
     * @return
     */
    private static RGB convert(Color color) {

        if (color != null) {
            int blue = (int) (color.getBlue() * 255.0f);
            int green = (int) (color.getGreen() * 255.0f);
            int red = (int) (color.getRed() * 255.0f);
            return new RGB(red, green, blue);
        }

        return null;
    }

    /**
     * the check and assignment of colors
     * 
     * @param value
     * @return rgb value
     * @throws VizException
     */
    protected RGB colorByValue(double valueArg) throws VizException {

        int ret = 0;
        RGB rgb = null;

        if (Double.isNaN(valueArg)) {
            rgb = convert(getColorMap().getColors().get(ret));
            return rgb;
        }

        double value = (Math.round(valueArg * 100.0)) / 100.0;

        
        if (field == FIELDS.DIFF) {
            Color color = colormapparams.getColorByValue((float) value);
            rgb = convert(color);
            return rgb;

        } else if (value >= 0.005) {
                Color color = colormapparams.getColorByValue((float) value);
                rgb = convert(color);
                return rgb;
        }

        List<Color> colors = getColorMap().getColors();

        if (ret >= colors.size()) {
            ret = colors.size() - 1;
        }

        if (ret < 0) {
            ret = 0;
        }

        rgb = convert(colors.get(ret));
        return rgb;
    }

    /**
     * Get and load the style rule
     * 
     * @return
     */
    private ParamLevelMatchCriteria getMatchCriteria() {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        ArrayList<String> paramList = new ArrayList<String>();

        if (field == FFMPRecord.FIELDS.QPE || field == FIELDS.QPF
                || field == FIELDS.GUIDANCE) {
            // qpe cases
            if (tableLoad) {
                String qpeName = FIELDS.QPE.getFieldName()
                        + determineQpeToUse(time);

                paramList.add(qpeName);
            } else {
                if (field == FIELDS.GUIDANCE) {
                    paramList.add(FIELDS.GUIDANCE.getFieldName());
                } else {
                    paramList.add(FIELDS.QPE.getFieldName());
                }
            }
        } else if (field == FIELDS.RATIO) {
            // ratio case
            paramList.add(FIELDS.RATIO.getFieldName());
        } else if (field == FIELDS.DIFF) {
            // rate, ratio and diff cases
            paramList.add(FIELDS.DIFF.getFieldName());
        } else if (field == FFMPRecord.FIELDS.RATE) {
            // rate case
            paramList.add(FIELDS.RATE.getFieldName());
        }

        match.setParameterName(paramList);

        return match;
    }

    private String determineQpeToUse(double time) {
        getQpeColorMapFiles();
        parseFileNames();
        String qpeHourToUse = determineColorMap(time);

        return qpeHourToUse;
    }

    private void parseFileNames() {
        double hour = 0.0;
        for (String fn : fileArray) {
            hour = 0.0;

            if (fn.indexOf("ffmp/qpe") >= 0) {

                String name1 = fn.replaceAll("colormaps/ffmp/qpe", "");
                String name2 = name1.replaceAll(".cmap", "");

                if (name2.length() == 0) {
                    hourColorMapMap.put(0.0, fn);
                } else {
                    hour = Double.parseDouble(name2);
                    hourColorMapMap.put(hour, fn);
                }
            }
        }
    }

    private String determineColorMap(double durHour) {
        String qpeHourToUse = null;
        for (Double dblHour : hourColorMapMap.keySet()) {
            if (durHour < dblHour) {
                break;
            }

            if (dblHour != 0.0) {

                // create a tmp value that will store the integer
                // part of the time. Example: 6.25 --> 6
                int intHour = dblHour.intValue();

                /*
                 * If the difference between double hour and int hour is greater
                 * than 0, set the qpeHourToUse to the double value.
                 * 
                 * If the difference between double hour and int hour is zero,
                 * then set qpeHourToUse to the int hour value.
                 * 
                 * The reason for this is that a color map name would be
                 * "qpe6.cmap" not qpe6.0.cmap. However, if you have an hour
                 * with a decimal greater than zero then qpe4.5.cmap would be
                 * valid and the qpeHourToUse would be 4.5
                 */
                if ((dblHour - intHour) > 0) {
                    qpeHourToUse = String.valueOf(dblHour);
                } else {
                    qpeHourToUse = String.valueOf(intHour);
                }

            }
        }

        /*
         * If qpeHourToUse is null then set qpeHourToUse to "". qpeHourToUse
         * will be added to the qpe file name to determine the color map to use.
         */
        if (qpeHourToUse == null) {
            qpeHourToUse = "";
        }

        return qpeHourToUse;
    }

    private void getQpeColorMapFiles() {
        LocalizationFile[] files = ColorMapLoader.listColorMapFiles();
        for (LocalizationFile file : files) {
            String fn = file.getName();
            if (fn.indexOf("ffmp/qpe") > 0) {
                fileArray.add(fn);
            }
        }
    }   
    
    
    /**
     * DR 14833: Error handling for the following:
     * when a user modified the ffmpImageryStyleRules.xml file 
     * without adding the related qpeX.cmap and for a user made
     * error like: qpe6/qpe4.cmap then default qpe/qpe.cmap used.      
     * 
     */    
    public IColorMap getDefaultColorMap(){
    	IColorMap cxml = null;  
    	
    	/*see parseFileNames(): colormap_name is "0.0" or qpe+key+".cmap"
    	double hour = hourColorMapMap.firstKey();
    	String cmapHour = ( hour==0.0 ? "" : String.valueOf(hour) );    		
    	System.out.println("FFMPColorUtils.getDefaultColorMap() cmapHour: "+cmapHour );*/

		/* Loop through all StyleRules to get the default.     
		 * In StyleManager.loadRules(StyleType), all levels(not only USER) 
		 * StyleRule loaded. So it is guaranteed the default can be loaded.
		 */
    	
		com.raytheon.uf.viz.core.style.StyleRuleset srs = 
			StyleManager.getInstance().getStyleRuleSet(StyleManager.StyleType.IMAGERY);
		
		for(StyleRule srl : srs.getStyleRules()){
			String pn="", cm="";
			try{
				pn = ((ParamLevelMatchCriteria)srl.getMatchCriteria()).getParameterNames().get(0);
				cm = ((ImagePreferences)srl.getPreferences()).getDefaultColormap();
			}catch(Exception e){ continue;	}	
			
			if(DEFAULT_PARAMNAME.equalsIgnoreCase(pn) && DEFAULT_COLORMAP.equalsIgnoreCase(cm)){
				sr = srl;
				System.out.println("FFMPColorUtils.getDefaultColorMap(): StyleRule pn-cm value:  "+pn+"-"+cm);
				break;
			}	 
			
		}
		/*
		if(sr == null){
		    	//get the MatchCriteria
		        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
		        ArrayList<String> paramList = new ArrayList<String>();        
		        paramList.add( FIELDS.QPE.getFieldName()+cmapHour );
		        match.setParameterName(paramList); 
		        
		        //get the StyleRule
		        try {
		            sr=StyleManager.getInstance().getStyleRule(StyleManager.StyleType.IMAGERY, match);
		        } catch (VizStyleException e) {
		            e.printStackTrace();
		        }
		} 
		*/
        //get the colormapfile name
        String colormapfile = ((ImagePreferences) sr.getPreferences()).getDefaultColormap();

        //load the colormap
        try {
            cxml = ColorMapLoader.loadColorMap(colormapfile);
        } catch (VizException e) {
            e.printStackTrace();
        }
    	
    	return cxml;
    }
}
