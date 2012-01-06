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
package com.raytheon.uf.viz.monitor.snow.threshold;

public class SnowMonitorMeteoData
{
    private String areaID;
    
    private double windSpeedR;
    private double windSpeedY;
    
    private double peakWindR;
    private double peakWindY;
    
    private double gustSpeedR;
    private double gustSpeedY;
    
    private double tempR;
    private double tempY;
        
    private double windChillR;
    private double windChillY;
    
    private double visR;
    private double visY;
    
    private double snowDepthR;
    private double snowDepthY;
    
    public SnowMonitorMeteoData()
    {        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
    }

    public double getWindSpeedR() {
        return windSpeedR;
    }

    public void setWindSpeedR(double windSpeedR) {
        this.windSpeedR = windSpeedR;
    }

    public double getWindSpeedY() {
        return windSpeedY;
    }

    public void setWindSpeedY(double windSpeedY) {
        this.windSpeedY = windSpeedY;
    }

    public double getPeakWindR() {
        return peakWindR;
    }

    public void setPeakWindR(double peakWindR) {
        this.peakWindR = peakWindR;
    }

    public double getPeakWindY() {
        return peakWindY;
    }

    public void setPeakWindY(double peakWindY) {
        this.peakWindY = peakWindY;
    }

    public double getGustSpeedR() {
        return gustSpeedR;
    }

    public void setGustSpeedR(double gustSpeedR) {
        this.gustSpeedR = gustSpeedR;
    }

    public double getGustSpeedY() {
        return gustSpeedY;
    }

    public void setGustSpeedY(double gustSpeedY) {
        this.gustSpeedY = gustSpeedY;
    }

    public double getTempR() {
        return tempR;
    }

    public void setTempR(double tempR) {
        this.tempR = tempR;
    }

    public double getTempY() {
        return tempY;
    }

    public void setTempY(double tempY) {
        this.tempY = tempY;
    }

    public double getWindChillR() {
        return windChillR;
    }

    public void setWindChillR(double windChillR) {
        this.windChillR = windChillR;
    }

    public double getWindChillY() {
        return windChillY;
    }

    public void setWindChillY(double windChillY) {
        this.windChillY = windChillY;
    }

    public double getVisR() {
        return visR;
    }

    public void setVisR(double visR) {
        this.visR = visR;
    }

    public double getVisY() {
        return visY;
    }

    public void setVisY(double visY) {
        this.visY = visY;
    }

    public double getSnowDepthR() {
        return snowDepthR;
    }

    public void setSnowDepthR(double snowDepthR) {
        this.snowDepthR = snowDepthR;
    }

    public double getSnowDepthY() {
        return snowDepthY;
    }

    public void setSnowDepthY(double snowDepthY) {
        this.snowDepthY = snowDepthY;
    }

    public void updateData(SnowMonitorMeteoData newData)
    {
        windSpeedR = newData.getWindSpeedR();
        windSpeedY = newData.getWindSpeedY();
        
        peakWindR = newData.getPeakWindR();
        peakWindY = newData.getPeakWindY();
        
        gustSpeedR = newData.getGustSpeedR();
        gustSpeedY = newData.getGustSpeedY();
        
        tempR = newData.getTempR();
        tempY = newData.getTempY();
            
        windChillR = newData.getWindChillR();
        windChillY = newData.getWindChillY();
        
        visR = newData.getVisR();
        visY = newData.getVisY();
        
        snowDepthR = newData.getSnowDepthR();
        snowDepthY = newData.getSnowDepthY();
    }
}
