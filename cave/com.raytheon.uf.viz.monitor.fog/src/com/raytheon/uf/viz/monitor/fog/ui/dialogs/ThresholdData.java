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
package com.raytheon.uf.viz.monitor.fog.ui.dialogs;

public class ThresholdData
{
    private double maxCloudTempMin = -30.0;
    private double maxCloudTempMax = 15.0;
    private double maxCloudTempInc = 0.1;
    private double maxCloudTempDef = -1.5;
    
    private double iceSnowVsFogMin = -20.0;
    private double iceSnowVsFogMax = 10.0;
    private double iceSnowVsFogInc = 0.1;
    private double iceSnowVsFogDef = -1.3;
    
    private double coolFogVsWarmSurfMin = -10.0;
    private double coolFogVsWarmSurfMax = 40.0;
    private double coolFogVsWarmSurfInc = 0.1;
    private double coolFogVsWarmSurfDef = 19.9;
    
    private double daytimeSmoothMin = 0.0;
    private double daytimeSmoothMax = 100.0;
    private double daytimeSmoothInc = 0.1;
    private double daytimeSmoothDef = 80.1;
    
    private double adjacencyThreshMin = 0.0;
    private double adjacencyThreshMax = 100.0;    
    private double adjacencyThreshInc = 1.0;
    private double adjacencyThreshDef = 49.0;
    
    private double twilightAngleMin = 0.1;
    private double twilightAngleMax = 5.0;
    private double twilightAngleInc = 0.1;
    private double twilightAngleDef = 2.5;
    
    private double fractalDimMin = 1.0;
    private double fractalDimMax = 2.0;
    private double fractalDimInc = 0.1;
    private double fractalDimDef = 1.5;
    
    public static enum Threshold {MaxCloudTemp, IceSnowVsFog, CoolFogVsWarmSurf,
        DaytimeSmooth, AdjacencyThresh, TwilightAngle, FractalDim};
    
    public ThresholdData()
    {
        
    }

    public int getScaleMaxValue(Threshold thresh)
    {
        int maxVal = (int)Math.round(
                (getMaxVal(thresh) - getMinVal(thresh))/ getIncrement(thresh));
        
        return maxVal;
    }
    
    public int getInitialScaleValue(Threshold thresh)
    {
        double defaultVal = getDefaultValue(thresh);
        int val = calcScaleValueFromSpinner(thresh, defaultVal);
        return val;
    }
    
    public int calcScaleValueFromSpinner(Threshold thresh, double value)
    {
        int val = 0;
        double min = getMinVal(thresh);
        double inc = getIncrement(thresh);
        
        if (min <= 0.0)
        {
            val = (int)Math.round((value + Math.abs(min)) / inc);
        }
        else
        {
            val = (int)Math.round((value - Math.abs(min)) / inc);
        }
        
        return val;
    }
    
    public int calcSpnrValueFromScale(Threshold thresh, int value)
    {
        double min = getMinVal(thresh);
        int val = (int)Math.round((((double)value/10 + min))*10);
        
        return val;
    }
    
    public double getDefaultValue(Threshold thresh)
    {
        double rv = 0.0;
        
        switch (thresh)
        {
            case MaxCloudTemp:
                return maxCloudTempDef;
            case IceSnowVsFog:
                return iceSnowVsFogDef;
            case CoolFogVsWarmSurf:
                return coolFogVsWarmSurfDef;
            case DaytimeSmooth:
                return daytimeSmoothDef;
            case AdjacencyThresh:
                return adjacencyThreshDef;
            case TwilightAngle:
                return twilightAngleDef;
            case FractalDim:
                return fractalDimDef;
        }
        
        return rv;
    }
    
    public double getIncrement(Threshold thresh)
    {
        double rv = 0.0;
        
        switch (thresh)
        {
            case MaxCloudTemp:
                return maxCloudTempInc;
            case IceSnowVsFog:
                return iceSnowVsFogInc;
            case CoolFogVsWarmSurf:
                return coolFogVsWarmSurfInc;
            case DaytimeSmooth:
                return daytimeSmoothInc;
            case AdjacencyThresh:
                return adjacencyThreshInc;
            case TwilightAngle:
                return twilightAngleInc;
            case FractalDim:
                return fractalDimInc;
        }
        
        return rv;
    }
    
    public double getMinVal(Threshold thresh)
    {
        double rv = 0.0;
        
        switch (thresh)
        {
            case MaxCloudTemp:
                return maxCloudTempMin;
            case IceSnowVsFog:
                return iceSnowVsFogMin;
            case CoolFogVsWarmSurf:
                return coolFogVsWarmSurfMin;
            case DaytimeSmooth:
                return daytimeSmoothMin;
            case AdjacencyThresh:
                return adjacencyThreshMin;
            case TwilightAngle:
                return twilightAngleMin;
            case FractalDim:
                return fractalDimMin;
        }
        
        return rv;
    }
    
    public double getMaxVal(Threshold thresh)
    {
        double rv = 0.0;
        
        switch (thresh)
        {
            case MaxCloudTemp:
                return maxCloudTempMax;
            case IceSnowVsFog:
                return iceSnowVsFogMax;
            case CoolFogVsWarmSurf:
                return coolFogVsWarmSurfMax;
            case DaytimeSmooth:
                return daytimeSmoothMax;
            case AdjacencyThresh:
                return adjacencyThreshMax;
            case TwilightAngle:
                return twilightAngleMax;
            case FractalDim:
                return fractalDimMax;
        }
        
        return rv;
    }
}
