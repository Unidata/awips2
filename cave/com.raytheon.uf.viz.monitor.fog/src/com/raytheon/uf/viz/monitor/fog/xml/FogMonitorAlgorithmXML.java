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
package com.raytheon.uf.viz.monitor.fog.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "FogMonitorAlgorithm")
@XmlAccessorType(XmlAccessType.NONE)
public class FogMonitorAlgorithmXML implements ISerializableObject
{
    @XmlElement(name = "FogProductYLo")
    private double fogProductYLo;
    
    @XmlElement(name = "FogProductRLo")
    private double fogProductRLo;
    
    @XmlElement(name = "FogProductRHi")
    private double fogProductRHi;
    
    @XmlElement(name = "FogProductYHi")
    private double fogProductYHi;
    
    @XmlElement(name = "VisYLo")
    private double visYLo;
    
    @XmlElement(name = "VisRLo")
    private double visRLo;
    
    @XmlElement(name = "VisRHi")
    private double visRHi;
    
    @XmlElement(name = "VisYHi")
    private double visYHi;
    
    @XmlElement(name = "MaxCloudTemp")
    private double maxCloudTemp;
    
    @XmlElement(name = "IceSnowVsFog")
    private double iceSnowVsFog;
    
    @XmlElement(name = "IceSnowVsFogOn")
    private boolean iceSnowVsFogOn;
    
    @XmlElement(name = "CoolFogVsWarmSurface")
    private double coolFogVsWarmSurface;
    
    @XmlElement(name = "CoolFogVsWarmSurfaceOn")
    private boolean coolFogVsWarmSurfaceOn;
    
    @XmlElement(name = "DaytimeSmoothThresh")
    private double daytimeSmoothThresh;
    
    @XmlElement(name = "DaytimeSmoothThreshOn")
    private boolean daytimeSmoothThreshOn;
    
    @XmlElement(name = "AdjacencyThresh")
    private double adjacencyThresh;
    
    @XmlElement(name = "AdjacencyThreshOn")
    private boolean adjacencyThreshOn;
    
    @XmlElement(name = "TwilightAngle")
    private double twilightAngle;
    
    @XmlElement(name = "TwilightAngleOn")
    private boolean twilightAngleOn;
    
    @XmlElement(name = "FractalDimension")
    private double fractalDimension;
    
    @XmlElement(name = "FractalDimensionOn")
    private boolean fractalDimensionOn;

    public FogMonitorAlgorithmXML()
    {        
    }

    public double getFogProductYLo() {
        return fogProductYLo;
    }

    public void setFogProductYLo(double fogProductYLo) {
        this.fogProductYLo = fogProductYLo;
    }

    public double getFogProductRLo() {
        return fogProductRLo;
    }

    public void setFogProductRLo(double fogProductRLo) {
        this.fogProductRLo = fogProductRLo;
    }

    public double getFogProductRHi() {
        return fogProductRHi;
    }

    public void setFogProductRHi(double fogProductRHi) {
        this.fogProductRHi = fogProductRHi;
    }

    public double getFogProductYHi() {
        return fogProductYHi;
    }

    public void setFogProductYHi(double fogProductYHi) {
        this.fogProductYHi = fogProductYHi;
    }

    public double getVisYLo() {
        return visYLo;
    }

    public void setVisYLo(double visYLo) {
        this.visYLo = visYLo;
    }

    public double getVisRLo() {
        return visRLo;
    }

    public void setVisRLo(double visRLo) {
        this.visRLo = visRLo;
    }

    public double getVisRHi() {
        return visRHi;
    }

    public void setVisRHi(double visRHi) {
        this.visRHi = visRHi;
    }

    public double getVisYHi() {
        return visYHi;
    }

    public void setVisYHi(double visYHi) {
        this.visYHi = visYHi;
    }

    public double getMaxCloudTemp() {
        return maxCloudTemp;
    }

    public void setMaxCloudTemp(double maxCloudTemp) {
        this.maxCloudTemp = maxCloudTemp;
    }

    public double getIceSnowVsFog() {
        return iceSnowVsFog;
    }

    public void setIceSnowVsFog(double iceSnowVsFog) {
        this.iceSnowVsFog = iceSnowVsFog;
    }

    public boolean isIceSnowVsFogOn() {
        return iceSnowVsFogOn;
    }

    public void setIceSnowVsFogOn(boolean iceSnowVsFogOn) {
        this.iceSnowVsFogOn = iceSnowVsFogOn;
    }

    public double getCoolFogVsWarmSurface() {
        return coolFogVsWarmSurface;
    }

    public void setCoolFogVsWarmSurface(double coolFogVsWarmSurface) {
        this.coolFogVsWarmSurface = coolFogVsWarmSurface;
    }

    public boolean isCoolFogVsWarmSurfaceOn() {
        return coolFogVsWarmSurfaceOn;
    }

    public void setCoolFogVsWarmSurfaceOn(boolean coolFogVsWarmSurfaceOn) {
        this.coolFogVsWarmSurfaceOn = coolFogVsWarmSurfaceOn;
    }

    public double getDaytimeSmoothThresh() {
        return daytimeSmoothThresh;
    }

    public void setDaytimeSmoothThresh(double daytimeSmoothThresh) {
        this.daytimeSmoothThresh = daytimeSmoothThresh;
    }

    public boolean isDaytimeSmoothThreshOn() {
        return daytimeSmoothThreshOn;
    }

    public void setDaytimeSmoothThreshOn(boolean daytimeSmoothThreshOn) {
        this.daytimeSmoothThreshOn = daytimeSmoothThreshOn;
    }

    public double getAdjacencyThresh() {
        return adjacencyThresh;
    }

    public void setAdjacencyThresh(double adjacencyThresh) {
        this.adjacencyThresh = adjacencyThresh;
    }

    public boolean isAdjacencyThreshOn() {
        return adjacencyThreshOn;
    }

    public void setAdjacencyThreshOn(boolean adjacencyThreshOn) {
        this.adjacencyThreshOn = adjacencyThreshOn;
    }

    public double getTwilightAngle() {
        return twilightAngle;
    }

    public void setTwilightAngle(double twilightAngle) {
        this.twilightAngle = twilightAngle;
    }

    public boolean isTwilightAngleOn() {
        return twilightAngleOn;
    }

    public void setTwilightAngleOn(boolean twilightAngleOn) {
        this.twilightAngleOn = twilightAngleOn;
    }

    public double getFractalDimension() {
        return fractalDimension;
    }

    public void setFractalDimension(double fractalDimension) {
        this.fractalDimension = fractalDimension;
    }

    public boolean isFractalDimensionOn() {
        return fractalDimensionOn;
    }

    public void setFractalDimensionOn(boolean fractalDimensionOn) {
        this.fractalDimensionOn = fractalDimensionOn;
    }

    
}
