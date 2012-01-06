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
package com.raytheon.uf.viz.monitor.data;

import java.util.HashMap;


public class RangesUtil
{
    /**
     * Class instance.
     */
    private static RangesUtil classInstance;
    
    /**
     * Array of visibility values.
     */
    private String[] visStrArray;
    
    private int[] visSixteenthArray;
    
    private HashMap<Integer, String> visValToStringMap;
    private HashMap<String, Integer> visStringToValMap;
    
    private HashMap<Integer, String> visStringIndexMap;
    private HashMap<Integer, Integer> visValueIndexMap;
    
    private HashMap<Integer, Integer> visValueToIndexMap;
    
    /**
     * Flag indicating the R value will be higher than the Y value.
     */
    public final Boolean RValIsHigher = true;
    
    /**
     * Flag indicating the Y value will be higher than the R value.
     */
    public final Boolean YValIsHigher = false;
    
    /**
     * Flag indicating that neither R or Y is higher than the other. 
     */
    public final Boolean ryNotNeeded = false;
       
    /**
     * Range type enumeration.
     */
    public static enum RangeType
    {
        SS_ScaWind, SS_ScaGust, SS_ScaMax, SS_ScaWave, SS_GaleWind, SS_GaleGust, SS_GaleMax,
        SS_StormWind, SS_StormGust, SS_StormMax, SS_HfwwWind, SS_HfwwGust, SS_HfwwMax,
        SS_WindDir, SS_Wind, SS_Gust, SS_WindDirF, SS_WindDirT, SS_Temp, SS_DewPt, SS_SLP, SS_SST,
        SS_Wave, SS_Steep, SS_PSwellHgt, SS_PSwellPer, SS_PSwellDirF, SS_PSwellDirT, SS_SSwellHgt, 
        SS_SSwellPer, SS_SSwellDirF, SS_SSwellDirT,
        SNOW_Wind, SNOW_WindDirF, SNOW_WindDirT, SNOW_Temp, SNOW_DewPt, SNOW_SLP, SNOW_Precip,
        SNOW_Chill, SNOW_FrstBt, SNOW_Depth, SNOW_SnIncRHr, SNOW_SnIncRTot,
        FOG_Wind, FOG_WindDirF, FOG_WindDirT, FOG_Temp, FOG_DewPt, FOG_Depr, FOG_Ceil, FOG_Humid
    };
    
    /**
     * Map of ranges.
     */
    private HashMap<RangeType, RangeData> rangeMap;
    
    /**
     * Private constructor.
     */
    private RangesUtil()
    {
        initData();
    }

    /**
     * Get an instance of this class.
     * @return An instance of this class.
     */
    public static synchronized RangesUtil getInstance()
    {
        // If the RangeUtil has not been created
        // then create a new instance.
        if (classInstance == null)
        {
            classInstance = new RangesUtil();
        }
        return classInstance;
    }
    
    /**
     * Initialize the data.
     */
    private void initData()
    {
        /*
         * Setup the vis mapping.
         */
        setupVisMapping();
        
        rangeMap = new HashMap<RangeType, RangeData>();
        
        /*
         * Add the SAFESEAS ranges.
         */
        rangeMap.put(RangeType.SS_ScaWind, new RangeData(1 , 30));
        rangeMap.put(RangeType.SS_ScaGust, new RangeData(1 , 30));
        rangeMap.put(RangeType.SS_ScaMax, new RangeData(1 , 30));
        rangeMap.put(RangeType.SS_ScaWave, new RangeData(2 , 15));
        
        rangeMap.put(RangeType.SS_GaleWind, new RangeData(20 , 40));
        rangeMap.put(RangeType.SS_GaleGust, new RangeData(20 , 40));
        rangeMap.put(RangeType.SS_GaleMax, new RangeData(20 , 40));
        
        rangeMap.put(RangeType.SS_StormWind, new RangeData(35 , 50));
        rangeMap.put(RangeType.SS_StormGust, new RangeData(35 , 50));
        rangeMap.put(RangeType.SS_StormMax, new RangeData(35 , 50));
        
        rangeMap.put(RangeType.SS_HfwwWind, new RangeData(50 , 70));
        rangeMap.put(RangeType.SS_HfwwGust, new RangeData(50 , 70));
        rangeMap.put(RangeType.SS_HfwwMax, new RangeData(50 , 70));
        
        rangeMap.put(RangeType.SS_WindDir, new RangeData(0 , 359));
        rangeMap.put(RangeType.SS_Wind, new RangeData(1 , 55));
        rangeMap.put(RangeType.SS_Gust, new RangeData(1 , 55));
        rangeMap.put(RangeType.SS_WindDirF, new RangeData(0 , 355));
        rangeMap.put(RangeType.SS_WindDirT, new RangeData(0 , 355));
        rangeMap.put(RangeType.SS_Temp, new RangeData(-20 , 120));
        
        rangeMap.put(RangeType.SS_DewPt, new RangeData(-20 , 120));
        rangeMap.put(RangeType.SS_SLP, new RangeData(900 , 1050));
        rangeMap.put(RangeType.SS_SST, new RangeData(-5 , 100));
        rangeMap.put(RangeType.SS_Wave, new RangeData(2 , 15));
        rangeMap.put(RangeType.SS_Steep, new RangeData(0 , 100));
        rangeMap.put(RangeType.SS_PSwellHgt, new RangeData(1 , 55));
        rangeMap.put(RangeType.SS_PSwellPer, new RangeData(0 , 120));
        rangeMap.put(RangeType.SS_PSwellDirF, new RangeData(0 , 355));
        rangeMap.put(RangeType.SS_PSwellDirT, new RangeData(0 , 355));
        rangeMap.put(RangeType.SS_SSwellHgt, new RangeData(1 , 55));
        rangeMap.put(RangeType.SS_SSwellPer, new RangeData(0 , 120));
        rangeMap.put(RangeType.SS_SSwellDirF, new RangeData(0 , 355));
        rangeMap.put(RangeType.SS_SSwellDirT, new RangeData(0 , 355));
        
        /*
         * Add the SNOW ranges.
         */
        rangeMap.put(RangeType.SNOW_Wind, new RangeData(1 , 100));
        rangeMap.put(RangeType.SNOW_WindDirF, new RangeData(0 , 355));
        rangeMap.put(RangeType.SNOW_WindDirT, new RangeData(0 , 355));
        rangeMap.put(RangeType.SNOW_Temp, new RangeData(-50 , 100));
        rangeMap.put(RangeType.SNOW_DewPt, new RangeData(-50 , 100));
        rangeMap.put(RangeType.SNOW_SLP, new RangeData(900 , 1050));
        rangeMap.put(RangeType.SNOW_Precip, new RangeData(0 , 20));
        rangeMap.put(RangeType.SNOW_Chill, new RangeData(-50 , 50));
        rangeMap.put(RangeType.SNOW_FrstBt, new RangeData(0 , 30));
        rangeMap.put(RangeType.SNOW_Depth, new RangeData(1 , 60));
        rangeMap.put(RangeType.SNOW_SnIncRHr, new RangeData(1 , 12));
        rangeMap.put(RangeType.SNOW_SnIncRTot, new RangeData(1 , 60));
        
        /*
         * Add the FOG ranges.
         */
        rangeMap.put(RangeType.FOG_Wind, new RangeData(1 , 100));
        rangeMap.put(RangeType.FOG_WindDirF, new RangeData(0 , 355));
        rangeMap.put(RangeType.FOG_WindDirT, new RangeData(0 , 355));
        rangeMap.put(RangeType.FOG_Temp, new RangeData(-50 , 100));
        rangeMap.put(RangeType.FOG_DewPt, new RangeData(-50 , 100));
        rangeMap.put(RangeType.FOG_Depr, new RangeData(0 , 50));
        rangeMap.put(RangeType.FOG_Ceil, new RangeData(0 , 120));
        rangeMap.put(RangeType.FOG_Humid, new RangeData(0 , 100));
    }
    
    private void setupVisMapping()
    {       
        visValToStringMap = new HashMap<Integer, String>();
        visStringToValMap = new HashMap<String, Integer>();
        
        visStringIndexMap = new HashMap<Integer, String>();
        visValueIndexMap = new HashMap<Integer, Integer>();
        
        visValueToIndexMap = new HashMap<Integer, Integer>();
        
        visStrArray = new String[]{"1/16", "1/8", "3/16", "1/4", "5/16", "3/8", "1/2", "5/8", "3/4",
                "7/8", "1", "1 1/8", "1 1/4", "1 3/8", "1 1/2", "1 5/8", "1 3/4", "1 7/8", "2",
                "2 1/4", "2 1/2", "2 3/4", "3", "4", "5", "6", "7", "8", "9", "10"};
        
        visSixteenthArray = new int[]{1, 2, 3, 4, 5, 6, 8, 10, 12, 14, 16, 18, 20,
                22, 24, 26, 28, 30, 32, 36, 40, 44, 48, 64, 80, 96, 112, 128, 144, 160};
        
        for (int i = 0; i < visSixteenthArray.length; i++)
        {            
            visValToStringMap.put(visSixteenthArray[i], visStrArray[i]);
            visStringToValMap.put(visStrArray[i], visSixteenthArray[i]);
            
            visStringIndexMap.put(i, visStrArray[i]);
            visValueIndexMap.put(i, visSixteenthArray[i]);
            
            visValueToIndexMap.put(visSixteenthArray[i], i);
        }
    }
    
    public int getVisValue(String visString)
    {
        if (visStringToValMap.containsKey(visString) == true)
        {
            return visStringToValMap.get(visString);
        }
        
        return 1;
    }
    
    /**
     * Determine the visibility display string from the miles passes in.
     * @param miles Miles.
     * @return The visibility display string.
     */
    public String getVisStringFromMiles(double miles)
    {
        int mile16ths = (int)Math.round(miles * 16.0);
        
        //System.out.println("mile16ths = " + mile16ths);
        
        if (mile16ths > visSixteenthArray[visSixteenthArray.length - 1])
        {
            mile16ths = visSixteenthArray[visSixteenthArray.length - 1];
        }
        else if (mile16ths <= 1.0)
        {
            mile16ths = visSixteenthArray[0];
        }
        else
        {
            for (int i = 0; i < visSixteenthArray.length; i++)
            {
                if (mile16ths < visSixteenthArray[i])
                {
                    mile16ths = visSixteenthArray[i - 1];
                    break;
                }
            }
        }      
        
        //System.out.println("mile16ths converted = " + mile16ths);
        
        if (visValToStringMap.containsKey(mile16ths) == true)
        {
            return visValToStringMap.get(mile16ths);
        }
        
        return "MSG";
    }
    
    public String getVisString(int visValue)
    {
        if (visValToStringMap.containsKey(visValue) == true)
        {
            return visValToStringMap.get(visValue);
        }
        
        return "MSG";
    }
    
    public String getVisStringAtIndex(int index)
    {
        return visStringIndexMap.get(index);
    }
    
    public int getVisValueAtIndex(int index)
    {
        return visValueIndexMap.get(index);
    }
    
    public int getIndexOfValue(int val)
    {
        if (visValueToIndexMap.containsKey(val) == true)
        {
            return visValueToIndexMap.get(val);
        }
        
        return 0;
    }
    
    /**
     * Get a String array of all the possible visibility values.
     * @return String array of all the possible visibility values.
     */
    public String[] getVisibility()
    {
        return visStrArray;
    }
    
    /**
     * Get the range data for the specified range type.
     * @param type Range type.
     * @return Range data.
     */
    public RangeData getRangeData(RangeType type)
    {
        return rangeMap.get(type);
    }
}
