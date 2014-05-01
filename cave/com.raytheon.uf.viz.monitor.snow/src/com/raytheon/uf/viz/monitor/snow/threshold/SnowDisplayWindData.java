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

public class SnowDisplayWindData 
{
    private String areaID;
    
    private double windWindSpeedR;
    private double windWindSpeedY;
    
    private double windPeakR;
    private double windPeakY;
    
    private double windGustR;
    private double windGustY;
    
    private double windDirFromR;
    private double windDirFromY;
    
    private double windDirToR;
    private double windDirToY;
    
    public SnowDisplayWindData()
    {        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
    }

    public double getWindWindSpeedR() {
        return windWindSpeedR;
    }

    public void setWindWindSpeedR(double windWindSpeedR) {
        this.windWindSpeedR = windWindSpeedR;
    }

    public double getWindWindSpeedY() {
        return windWindSpeedY;
    }

    public void setWindWindSpeedY(double windWindSpeedY) {
        this.windWindSpeedY = windWindSpeedY;
    }

    public double getWindPeakR() {
        return windPeakR;
    }

    public void setWindPeakR(double windPeakR) {
        this.windPeakR = windPeakR;
    }

    public double getWindPeakY() {
        return windPeakY;
    }

    public void setWindPeakY(double windPeakY) {
        this.windPeakY = windPeakY;
    }

    public double getWindGustR() {
        return windGustR;
    }

    public void setWindGustR(double windGustR) {
        this.windGustR = windGustR;
    }

    public double getWindGustY() {
        return windGustY;
    }

    public void setWindGustY(double windGustY) {
        this.windGustY = windGustY;
    }

    public double getWindDirFromR() {
        return windDirFromR;
    }

    public void setWindDirFromR(double windDirFromR) {
        this.windDirFromR = windDirFromR;
    }

    public double getWindDirFromY() {
        return windDirFromY;
    }

    public void setWindDirFromY(double windDirFromY) {
        this.windDirFromY = windDirFromY;
    }

    public double getWindDirToR() {
        return windDirToR;
    }

    public void setWindDirToR(double windDirToR) {
        this.windDirToR = windDirToR;
    }

    public double getWindDirToY() {
        return windDirToY;
    }

    public void setWindDirToY(double windDirToY) {
        this.windDirToY = windDirToY;
    }

    public void updateData(SnowDisplayWindData newData)
    {
        windWindSpeedR = newData.getWindWindSpeedR();
        windWindSpeedY = newData.getWindWindSpeedY();
        
        windPeakR = newData.getWindPeakR();
        windPeakY = newData.getWindPeakY();
        
        windGustR = newData.getWindGustR();
        windGustY = newData.getWindGustY();
        
        windDirFromR = newData.getWindDirFromR();
        windDirFromY = newData.getWindDirFromY();
        
        windDirToR = newData.getWindDirToR();
        windDirToY = newData.getWindDirToY();
    }
}
