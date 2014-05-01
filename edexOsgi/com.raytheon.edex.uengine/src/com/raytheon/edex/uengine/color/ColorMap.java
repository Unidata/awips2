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

package com.raytheon.edex.uengine.color;

import java.util.HashMap;
import java.util.Map;

/**
 * Class used by <code>ColorMapImage</code> task to get color map information.
 * 
 * @author pheaberl
 */
public class ColorMap {

    private String colorMapName = null;
    
    private int numColors = -1;
    
    private int numBits = -1;
    
    private float maxVal = 0f;
    
    private float minVal = 0f;
    
    private byte[] red = null;
    
    private byte[] blue = null;
    
    private byte[] green = null;

    // "enumeration" HashMap
    private static Map<String, ColorMap> colorMapEnum = new HashMap<String, ColorMap>();
    
    private ColorMap(String colorMapName, int numColors, int numBits) {
        super();
        this.colorMapName = colorMapName;
        this.numColors = numColors;
        this.numBits = numBits;
    }

    public byte[] getRed() {
        return red;
    }

    public byte[] getGreen() {
        return green;
    }

    public byte[] getBlue() {
        return blue;
    }

    public int getNumBits() {
        return numBits;
    }

    public int getNumColors() {
        return numColors;
    }

    public float getMaxVal() {
        return maxVal;
    }

    public float getMinVal() {
        return minVal;
    }
    
    //----------------------- ColorMaps
    public static final ColorMap BLACK_WHITE = new ColorMap("BW", 256, 8);
    static {
        BLACK_WHITE.red   = new byte[BLACK_WHITE.numColors];
        BLACK_WHITE.green = new byte[BLACK_WHITE.numColors];
        BLACK_WHITE.blue  = new byte[BLACK_WHITE.numColors];
        
        // Grayscale color map to use with satellite imagery
        for (int i = 0; i < BLACK_WHITE.numColors; i++)
        {
            BLACK_WHITE.red[i]   = (byte)i;
            BLACK_WHITE.green[i] = (byte)i;
            BLACK_WHITE.blue[i]  = (byte)i;
        }
        
        colorMapEnum.put(BLACK_WHITE.colorMapName, BLACK_WHITE);
    }
    public static final ColorMap STOP_LIGHT = new ColorMap("StopLight",3,8);
    static {
        /*
         * creates a "stop light" color map.
         * 0 => green
         * 1 => yellow
         * 2 => red
         */
        byte[] red   = {      0  , (byte)255, (byte)255};
        byte[] green = {(byte)255, (byte)255,       0  };
        byte[] blue  = {      0  ,       0  ,       0  };
        STOP_LIGHT.red = red;
        STOP_LIGHT.green = green;
        STOP_LIGHT.blue = blue;
        STOP_LIGHT.minVal = 0;
        STOP_LIGHT.maxVal = 2;
        colorMapEnum.put(STOP_LIGHT.colorMapName,STOP_LIGHT);
    }
    public static final ColorMap SIXTY_FOUR_RGB = new ColorMap("64RGB",256,8);
    static {
        SIXTY_FOUR_RGB.red   = new byte[SIXTY_FOUR_RGB.numColors];
        SIXTY_FOUR_RGB.green = new byte[SIXTY_FOUR_RGB.numColors];
        SIXTY_FOUR_RGB.blue  = new byte[SIXTY_FOUR_RGB.numColors];
        // funky map of 64 colors
        int[] range = {0,1,2,3};
        for(int i : range) {
            for (int j : range) {
                for(int k : range) {
                    int l = 16 * i + 4 * j + k;
                    int red = 64 * i;
                    int green = 64 * j;
                    int blue = 64 * k;
                    for (int m : range) {
                        int n = 4 * l + m;
                        SIXTY_FOUR_RGB.red[n] = (byte)red;
                        SIXTY_FOUR_RGB.green[n] = (byte)green;
                        SIXTY_FOUR_RGB.blue[n] = (byte)blue;
                    }
                }
            }
        }
        SIXTY_FOUR_RGB.maxVal = 255;
        SIXTY_FOUR_RGB.minVal = 0;
        colorMapEnum.put(SIXTY_FOUR_RGB.colorMapName, SIXTY_FOUR_RGB);

    }
    public static final ColorMap SIXTY_FOUR_BW = new ColorMap("64BW",256,8);
    static {
        SIXTY_FOUR_BW.red   = new byte[SIXTY_FOUR_BW.numColors];
        SIXTY_FOUR_BW.green = new byte[SIXTY_FOUR_BW.numColors];
        SIXTY_FOUR_BW.blue  = new byte[SIXTY_FOUR_BW.numColors];
        // funky map of 64 gray scales
        int[] range = {0,1,2,3};
        for(int i : range) {
            for (int j : range) {
                for(int k : range) {
                    int l = 64 * i + 16 * j + 4 * k;
                    for (int m : range) {
                        SIXTY_FOUR_BW.red[l + m] = (byte)l;
                        SIXTY_FOUR_BW.green[l + m] = (byte)l;
                        SIXTY_FOUR_BW.blue[l + m] = (byte)l;
                    }
                }
            }
        }
        SIXTY_FOUR_BW.maxVal = 255;
        SIXTY_FOUR_BW.minVal = 0;
        colorMapEnum.put(SIXTY_FOUR_BW.colorMapName, SIXTY_FOUR_BW);

    }
    public static final ColorMap FALSE_COLOR = new ColorMap("false",33,8);
    static {
        byte red[] = {0,(byte)255,0,(byte)228,(byte)205,(byte)170,(byte)135,(byte)130,(byte)130,(byte)130,(byte)130,(byte)139,(byte)152,(byte)161,(byte)176,(byte)189,(byte)202,(byte)214,(byte)220,(byte)227,(byte)240,(byte)176,(byte)128,80,32,0,0,0,0,0,0,0,0};
        FALSE_COLOR.red = red;
        
        byte green[] = {0,(byte)255,(byte)178,(byte)228,(byte)205,(byte)170,(byte)135,106,84,72,60,75,96,111,(byte)135,(byte)156,(byte)177,(byte)198,(byte)207,(byte)219,(byte)240,(byte)224,(byte)212,(byte)200,(byte)188,(byte)180,(byte)165,(byte)140,125,110,95,80,0};
        FALSE_COLOR.green = green;

        byte blue[] = {0,(byte)255,(byte)238,(byte)228,(byte)205,(byte)170,(byte)135,95,65,48,30,28,24,22,18,14,10,7,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
        FALSE_COLOR.blue = blue;
        
        FALSE_COLOR.maxVal = 32;
        FALSE_COLOR.minVal = 0;
        colorMapEnum.put(FALSE_COLOR.colorMapName, FALSE_COLOR);
    }
    
    public static final ColorMap GRIB_COLOR = new ColorMap("GribRGB", 64, 8);
    static {
    	byte[] red = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 0, 0, 0, 0, 7, 23, 39, 55, 71, 87, 103, 119, (byte)135, (byte)151,
				(byte)167, (byte)183, (byte)199, (byte)215, (byte)231,(byte) 247, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255,
				(byte)255, (byte)255, (byte)255, (byte)255,(byte) 255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)246, (byte)228,
				(byte)211, (byte)193, (byte)175, (byte)158, (byte)140 };
        GRIB_COLOR.red = red;
        
        byte[] green = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 27, 43, 59, 75, 91, 107,
				123, (byte)139, (byte)155, (byte)171, (byte)187, (byte)203, (byte)219, (byte)235, (byte)251, (byte)255, (byte)255, (byte)255,
				(byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255,
				(byte)255, (byte)247, (byte)231, (byte)215, (byte)199, (byte)183, (byte)167, (byte)151, (byte)135, 119, 103, 87, 71,
				55, 39, 23, 7, 0, 0, 0, 0, 0, 0, 0 };
        GRIB_COLOR.green = green;

        byte[] blue = { 0, (byte)143, (byte)159, (byte)175, (byte)191, (byte)207, (byte)223, (byte)239, (byte)255, (byte)255, (byte)255,
        		(byte)255, (byte)255,(byte) 255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255, (byte)255,(byte) 255, (byte)255, (byte)255,
        		(byte)255, (byte)255, (byte)247, (byte)231, (byte)215, (byte)199, (byte)183, (byte)167, (byte)151, (byte)135, 119, 103, 87,
				71, 55, 39, 23, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 0, 0, 0, 0, 0, 0 };
        GRIB_COLOR.blue = blue;
        
        GRIB_COLOR.maxVal = 330f;//134.33f;
        GRIB_COLOR.minVal = 165f;//-162.67f;
        
        colorMapEnum.put(GRIB_COLOR.colorMapName, GRIB_COLOR);
    }
    /**
     * Creates a blue to red ramp for temperature gradiation. This color map is based
     * on an article found on the web @ http://local.wasp.uwa.edu.au/~pbourke/colour/colourramp/
     */
    public static final ColorMap GRIB_TEMP = new ColorMap("GribTempRGB", 256, 8);
    static {
        byte[] red = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,66,70,74,78,82,86,90,94,98,102,106,110,114,118,122,126,(byte)130,(byte)134,(byte)138,(byte)142,(byte)146,(byte)150,(byte)154,(byte)158,(byte)162,(byte)166,(byte)170,(byte)174,(byte)178,(byte)182,(byte)186,(byte)190,(byte)194,(byte)198,(byte)202,(byte)206,(byte)210,(byte)214,(byte)218,(byte)222,(byte)226,(byte)230,(byte)234,(byte)238,(byte)242,(byte)246,(byte)250,(byte)254,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255};
        GRIB_TEMP.red   = red;
        byte[] green = {0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124,(byte)128,(byte)132,(byte)136,(byte)140,(byte)144,(byte)148,(byte)152,(byte)156,(byte)160,(byte)164,(byte)168,(byte)172,(byte)176,(byte)180,(byte)184,(byte)188,(byte)192,(byte)196,(byte)200,(byte)204,(byte)208,(byte)212,(byte)216,(byte)220,(byte)224,(byte)228,(byte)232,(byte)236,(byte)240,(byte)244,(byte)248,(byte)252,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)252,(byte)248,(byte)244,(byte)240,(byte)236,(byte)232,(byte)228,(byte)224,(byte)220,(byte)216,(byte)212,(byte)208,(byte)204,(byte)200,(byte)196,(byte)192,(byte)187,(byte)183,(byte)179,(byte)175,(byte)171,(byte)167,(byte)163,(byte)159,(byte)155,(byte)151,(byte)147,(byte)143,(byte)139,(byte)135,(byte)131,127,123,119,115,111,107,103,99,95,91,87,83,79,75,71,67,63,59,55,51,47,43,39,35,31,27,23,19,15,11,7,3,0};
        GRIB_TEMP.green = green;
        byte[] blue = {(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)255,(byte)254,(byte)250,(byte)246,(byte)242,(byte)238,(byte)234,(byte)230,(byte)226,(byte)222,(byte)218,(byte)214,(byte)210,(byte)206,(byte)202,(byte)198,(byte)194,(byte)189,(byte)185,(byte)181,(byte)177,(byte)173,(byte)169,(byte)165,(byte)161,(byte)157,(byte)153,(byte)149,(byte)145,(byte)141,(byte)137,(byte)133,(byte)129,125,121,117,113,109,105,101,97,93,89,85,81,77,73,69,65,61,57,53,49,45,41,37,33,29,25,21,17,13,9,5,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
        GRIB_TEMP.blue  = blue;
        GRIB_TEMP.minVal = 220f; // approximately -60F
        GRIB_TEMP.maxVal = 335f; // approximately 145F
        colorMapEnum.put(GRIB_TEMP.colorMapName, GRIB_TEMP);
    }
    public static final ColorMap IR_ENHANCED = new ColorMap("IREnhanced", 256, 8);
    static {
    	byte[] red = new byte[256];
    	byte[] green = new byte[256];
    	byte [] blue = new byte[256];
    	for(int i = 0; i <= 60; i++){
    		red[i] = green[i] = blue[i]= 0;
    	}
    	for(int i = 0; i <= 29; i++){
    		int position = 61 + i;
    		float currentColorDiff = (float)(67.0 * (i / 29.0));
    		int color = 3 + Math.round(currentColorDiff);
    		red[position] = green[position] = blue[position] = (byte)color;
    	}
    	for(int i = 91; i <= 100; i++){
    		red[i] = green[i] = blue[i] = (byte)i;
    	}
    	for(int i = 0; i <= 44; i++){
    		int position = 101 + i;
    		float currentColorDiff = (float)(147.0 * (i/44.0));
    		int color = 103 + Math.round(currentColorDiff);
    		red[position] = green[position] = blue[position] = (byte)color;
    	}
    	for(int i = 0; i <= 8; i++){
    		int position = 146 + i;
    		float currentBlueColorDiff = (float)(5.0 * (i/8.0));
    		float currentGreenColorDiff = (float)(222.0 * (i/8.0));
    		float currentRedColorDiff = (float)(52.0 * (i/8.0));
    		int blueColor = 250 + Math.round(currentBlueColorDiff);
    		int greenColor = 222 - Math.round(currentGreenColorDiff);
    		int redColor = 243 - Math.round(currentRedColorDiff);
    		green[position] = (byte)greenColor;
    		blue[position] = (byte)blueColor;
    		red[position] = (byte)redColor;
    	}
    	for(int i = 0; i <= 15; i++){
    		int position = 155 + i;
    		float currentColorDiff = (float)(191.0 * (i/15.0));
    		int color = 191 - Math.round(currentColorDiff);
    		blue[position] = (byte)255;
    		green[position] = 0;
    		red[position] = (byte)color;
    	}
    	for(int i = 0; i <= 19; i++){
    		int position = 171 + i;
    		float currentBlueColorDiff = (float)(242.0 * (i/19.0));
    		float currentGreenColorDiff = (float)(243.0 * (i/19.0));
    		int blueColor = 242 - Math.round(currentBlueColorDiff);
    		int greenColor = 12 + Math.round(currentGreenColorDiff);
    		green[position] = (byte)greenColor;
    		blue[position] = (byte)blueColor;
    		red[position] = 0;
    	}
    	for(int i = 0; i <= 9; i++){
    		int position = 191 + i;
    		float currentColorDiff = (float)(230.0 * (i/9.0));
    		int color = 25 + Math.round(currentColorDiff);
    		blue[position] = 0;
    		green[position] = (byte)255;
    		red[position] = (byte)color;
    	}
    	for(int i = 0; i <= 9; i++){
    		int position = 201 + i;
    		float currentColorDiff = (float)(229.0 * (i/9.0));
    		int color = 229 - Math.round(currentColorDiff);
    		blue[position] = 0;
    		green[position] = (byte)color;
    		red[position] = (byte)255;
    	}
    	for(int i = 0; i <= 9; i++){
    		int position = 211 + i;
    		float currentColorDiff = (float)(229.0 * (i/9.0));
    		int color = 229 - Math.round(currentColorDiff);
    		green[position] = blue[position] = 0;
    		red[position] = (byte)color;
    	}
    	for(int i = 0; i <= 24; i++){
    		int position = 221 + i;
    		float currentColorDiff = (float)(235.0 * (i/24.0));
    		int color = 20 + Math.round(currentColorDiff);
    		red[position] = green[position] = blue[position] = (byte)color;
    	}
    	for(int i = 246; i <= 255; i++){
    		red[i] = green[i] = blue[i] = (byte)255;
    	}
    	IR_ENHANCED.red = red;
    	IR_ENHANCED.blue = blue;
    	IR_ENHANCED.green = green;
    	IR_ENHANCED.maxVal = 255;
    	IR_ENHANCED.minVal = 0;
    	colorMapEnum.put(IR_ENHANCED.colorMapName, IR_ENHANCED);
    }
    
    //----------------------- map ColorMaps to String
    /**
     * Gets the color map by name.
     * 
     * @param colorMapName the color map name
     * @return the color map
     */
    public static final ColorMap getColorMap(String colorMapName) {
        return colorMapEnum.get(colorMapName);
    }
    /**
     * Checks to see if their is a color map matching a specified name.
     * @param colorMapName the color map name
     * @return true if the specified color map exists
     */
    public static final boolean isColorMap(String colorMapName) {
        return colorMapEnum.containsKey(colorMapName);
    }
}
