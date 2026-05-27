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
package com.raytheon.uf.viz.monitor.fog.threshold;

public class FogDisplayMeteoData
{
    private String areaID;
    
    private double meteoVisR;
    private double meteoVisY;
    
    private double meteoCeilingR;
    private double meteoCeilingY;
    
    private double meteoTempR;
    private double meteoTempY;
    
    private double meteoDewpointR;
    private double meteoDewpointY;
    
    private double meteoTtdR;
    private double meteoTtdY;
    
    private double meteoRelHumR;
    private double meteoRelHumY;

    public FogDisplayMeteoData()
    {        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
    }

    public double getMeteoVisR() {
        return meteoVisR;
    }

    public void setMeteoVisR(double meteoVisR) {
        this.meteoVisR = meteoVisR;
    }

    public double getMeteoVisY() {
        return meteoVisY;
    }

    public void setMeteoVisY(double meteoVisY) {
        this.meteoVisY = meteoVisY;
    }

    public double getMeteoCeilingR() {
        return meteoCeilingR;
    }

    public void setMeteoCeilingR(double meteoCeilingR) {
        this.meteoCeilingR = meteoCeilingR;
    }

    public double getMeteoCeilingY() {
        return meteoCeilingY;
    }

    public void setMeteoCeilingY(double meteoCeilingY) {
        this.meteoCeilingY = meteoCeilingY;
    }

    public double getMeteoTempR() {
        return meteoTempR;
    }

    public void setMeteoTempR(double meteoTempR) {
        this.meteoTempR = meteoTempR;
    }

    public double getMeteoTempY() {
        return meteoTempY;
    }

    public void setMeteoTempY(double meteoTempY) {
        this.meteoTempY = meteoTempY;
    }

    public double getMeteoDewpointR() {
        return meteoDewpointR;
    }

    public void setMeteoDewpointR(double meteoDewpointR) {
        this.meteoDewpointR = meteoDewpointR;
    }

    public double getMeteoDewpointY() {
        return meteoDewpointY;
    }

    public void setMeteoDewpointY(double meteoDewpointY) {
        this.meteoDewpointY = meteoDewpointY;
    }

    public double getMeteoTtdR() {
        return meteoTtdR;
    }

    public void setMeteoTtdR(double meteoTtdR) {
        this.meteoTtdR = meteoTtdR;
    }

    public double getMeteoTtdY() {
        return meteoTtdY;
    }

    public void setMeteoTtdY(double meteoTtdY) {
        this.meteoTtdY = meteoTtdY;
    }

    public double getMeteoRelHumR() {
        return meteoRelHumR;
    }

    public void setMeteoRelHumR(double meteoRelHumR) {
        this.meteoRelHumR = meteoRelHumR;
    }

    public double getMeteoRelHumY() {
        return meteoRelHumY;
    }

    public void setMeteoRelHumY(double meteoRelHumY) {
        this.meteoRelHumY = meteoRelHumY;
    }
    
    /**
     * Update the data with the new data passed in.
     * @param newData New data.
     */
    public void updateData(FogDisplayMeteoData newData)
    {
        meteoVisR = newData.getMeteoVisR();
        meteoVisY = newData.getMeteoVisY();
        
        meteoCeilingR = newData.getMeteoCeilingR();
        meteoCeilingY = newData.getMeteoCeilingY();
        
        meteoTempR = newData.getMeteoTempR();
        meteoTempY = newData.getMeteoTempY();
        
        meteoDewpointR = newData.getMeteoDewpointR();
        meteoDewpointY = newData.getMeteoDewpointY();
        
        meteoTtdR = newData.getMeteoTtdR();
        meteoTtdY = newData.getMeteoTtdY();
        
        meteoRelHumR = newData.getMeteoRelHumR();
        meteoRelHumY = newData.getMeteoRelHumY();        
    }
}
