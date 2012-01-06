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

public class SnowDisplayMeteoData
{
    private String areaID;
    
    private double tempR;
    private double tempY;
    
    private double dewpointR;
    private double dewpointY;
    
    private double visR;
    private double visY;
    
    private double slpR;
    private double slpY;
    
    private double hrPrecipR;
    private double hrPrecipY;
    
    private double windChillR;
    private double windChillY;
    
    private double frostBiteR;
    private double frostBiteY;
    
    private double snowDepthR;
    private double snowDepthY;
    
    private double snincrHrlyR;
    private double snincrHrlyY;
    
    private double snincrTotR;
    private double snincrTotY;
    
    public SnowDisplayMeteoData()
    {
        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
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

    public double getDewpointR() {
        return dewpointR;
    }

    public void setDewpointR(double dewpointR) {
        this.dewpointR = dewpointR;
    }

    public double getDewpointY() {
        return dewpointY;
    }

    public void setDewpointY(double dewpointY) {
        this.dewpointY = dewpointY;
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

    public double getSlpR() {
        return slpR;
    }

    public void setSlpR(double slpR) {
        this.slpR = slpR;
    }

    public double getSlpY() {
        return slpY;
    }

    public void setSlpY(double slpY) {
        this.slpY = slpY;
    }

    public double getHrPrecipR() {
        return hrPrecipR;
    }

    public void setHrPrecipR(double hrPrecipR) {
        this.hrPrecipR = hrPrecipR;
    }

    public double getHrPrecipY() {
        return hrPrecipY;
    }

    public void setHrPrecipY(double hrPrecipY) {
        this.hrPrecipY = hrPrecipY;
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

    public double getFrostBiteR() {
        return frostBiteR;
    }

    public void setFrostBiteR(double frostBiteR) {
        this.frostBiteR = frostBiteR;
    }

    public double getFrostBiteY() {
        return frostBiteY;
    }

    public void setFrostBiteY(double frostBiteY) {
        this.frostBiteY = frostBiteY;
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

    public double getSnincrHrlyR() {
        return snincrHrlyR;
    }

    public void setSnincrHrlyR(double snincrHrlyR) {
        this.snincrHrlyR = snincrHrlyR;
    }

    public double getSnincrHrlyY() {
        return snincrHrlyY;
    }

    public void setSnincrHrlyY(double snincrHrlyY) {
        this.snincrHrlyY = snincrHrlyY;
    }

    public double getSnincrTotR() {
        return snincrTotR;
    }

    public void setSnincrTotR(double snincrTotR) {
        this.snincrTotR = snincrTotR;
    }

    public double getSnincrTotY() {
        return snincrTotY;
    }

    public void setSnincrTotY(double snincrTotY) {
        this.snincrTotY = snincrTotY;
    }

    public void updateData(SnowDisplayMeteoData newData)
    {
        tempR = newData.getTempR();
        tempY = newData.getTempY();
        
        dewpointR = newData.getDewpointR();
        dewpointY = newData.getDewpointY();
        
        visR = newData.getVisR();
        visY = newData.getVisY();
        
        slpR = newData.getSlpR();
        slpY = newData.getSlpY();
        
        hrPrecipR = newData.getHrPrecipR();
        hrPrecipY = newData.getHrPrecipY();
        
        windChillR = newData.getWindChillR();
        windChillY = newData.getWindChillY();
        
        frostBiteR = newData.getFrostBiteR();
        frostBiteY = newData.getFrostBiteY();
        
        snowDepthR = newData.getSnowDepthR();
        snowDepthY = newData.getSnowDepthY();
        
        snincrHrlyR = newData.getSnincrHrlyR();
        snincrHrlyY = newData.getSnincrHrlyY();
        
        snincrTotR = newData.getSnincrTotR();
        snincrTotY = newData.getSnincrTotY();
    }
}
