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
package com.raytheon.uf.viz.monitor.safeseas.threshold;

public class SSDisplayWindData
{
    private String areaID;
    
    private double windSpeedR;
    private double windSpeedY;
    private double gustSpeedR;
    private double gustSpeedY;
    private double peakWindR;
    private double peakWindY;
    private double windDirFromR;
    private double windDirFromY;
    private double windDirToR;
    private double windDirToY;
    
    public SSDisplayWindData()
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
    
    public void updateData(SSDisplayWindData newData)
    {
        windSpeedR = newData.getWindSpeedR();
        windSpeedY = newData.getWindSpeedY();
        gustSpeedR = newData.getGustSpeedR();
        gustSpeedY = newData.getGustSpeedY();
        peakWindR = newData.getPeakWindR();
        peakWindY = newData.getPeakWindY();
        windDirFromR = newData.getWindDirFromR();
        windDirFromY = newData.getWindDirFromY();
        windDirToR = newData.getWindDirToR();
        windDirToY = newData.getWindDirToY();
    }
}
