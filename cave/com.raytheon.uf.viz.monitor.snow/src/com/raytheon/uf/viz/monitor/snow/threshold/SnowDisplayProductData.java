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

public class SnowDisplayProductData
{
    private String areaID;
    
    private double blizWrnVisR;
    private double blizWrnVisY;
    private double blizWrnWindSpdR;
    private double blizWrnWindSpdY;
    private double blizWrnGustSpdR;
    private double blizWrnGustSpdY;
    private double blizWrnPeakWindR;
    private double blizWrnPeakWindY;
    
    private double frzPrecipTempR;
    private double frzPrecipTempY;
    private double frzPrecipHrlyPrcpR;
    private double frzPrecipHrlyPrcpY;
    
    private double hvySnowSnincrHrR;
    private double hvySnowSnincrHrY;
    private double hvySnowSnincrTotR;
    private double hvySnowSnincrTotY;
    private double hvySnowDepthR;
    private double hvySnowDepthY;

    public SnowDisplayProductData()
    {        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
    }

    public double getBlizWrnVisR() {
        return blizWrnVisR;
    }

    public void setBlizWrnVisR(double blizWrnVisR) {
        this.blizWrnVisR = blizWrnVisR;
    }

    public double getBlizWrnVisY() {
        return blizWrnVisY;
    }

    public void setBlizWrnVisY(double blizWrnVisY) {
        this.blizWrnVisY = blizWrnVisY;
    }

    public double getBlizWrnWindSpdR() {
        return blizWrnWindSpdR;
    }

    public void setBlizWrnWindSpdR(double blizWrnWindSpdR) {
        this.blizWrnWindSpdR = blizWrnWindSpdR;
    }

    public double getBlizWrnWindSpdY() {
        return blizWrnWindSpdY;
    }

    public void setBlizWrnWindSpdY(double blizWrnWindSpdY) {
        this.blizWrnWindSpdY = blizWrnWindSpdY;
    }

    public double getBlizWrnGustSpdR() {
        return blizWrnGustSpdR;
    }

    public void setBlizWrnGustSpdR(double blizWrnGustSpdR) {
        this.blizWrnGustSpdR = blizWrnGustSpdR;
    }

    public double getBlizWrnGustSpdY() {
        return blizWrnGustSpdY;
    }

    public void setBlizWrnGustSpdY(double blizWrnGustSpdY) {
        this.blizWrnGustSpdY = blizWrnGustSpdY;
    }

    public double getBlizWrnPeakWindR() {
        return blizWrnPeakWindR;
    }

    public void setBlizWrnPeakWindR(double blizWrnPeakWindR) {
        this.blizWrnPeakWindR = blizWrnPeakWindR;
    }

    public double getBlizWrnPeakWindY() {
        return blizWrnPeakWindY;
    }

    public void setBlizWrnPeakWindY(double blizWrnPeakWindY) {
        this.blizWrnPeakWindY = blizWrnPeakWindY;
    }

    public double getFrzPrecipTempR() {
        return frzPrecipTempR;
    }

    public void setFrzPrecipTempR(double frzPrecipTempR) {
        this.frzPrecipTempR = frzPrecipTempR;
    }

    public double getFrzPrecipTempY() {
        return frzPrecipTempY;
    }

    public void setFrzPrecipTempY(double frzPrecipTempY) {
        this.frzPrecipTempY = frzPrecipTempY;
    }

    public double getFrzPrecipHrlyPrcpR() {
        return frzPrecipHrlyPrcpR;
    }

    public void setFrzPrecipHrlyPrcpR(double frzPrecipHrlyPrcpR) {
        this.frzPrecipHrlyPrcpR = frzPrecipHrlyPrcpR;
    }

    public double getFrzPrecipHrlyPrcpY() {
        return frzPrecipHrlyPrcpY;
    }

    public void setFrzPrecipHrlyPrcpY(double frzPrecipHrlyPrcpY) {
        this.frzPrecipHrlyPrcpY = frzPrecipHrlyPrcpY;
    }

    public double getHvySnowSnincrHrR() {
        return hvySnowSnincrHrR;
    }

    public void setHvySnowSnincrHrR(double hvySnowSnincrHrR) {
        this.hvySnowSnincrHrR = hvySnowSnincrHrR;
    }

    public double getHvySnowSnincrHrY() {
        return hvySnowSnincrHrY;
    }

    public void setHvySnowSnincrHrY(double hvySnowSnincrHrY) {
        this.hvySnowSnincrHrY = hvySnowSnincrHrY;
    }

    public double getHvySnowSnincrTotR() {
        return hvySnowSnincrTotR;
    }

    public void setHvySnowSnincrTotR(double hvySnowSnincrTotR) {
        this.hvySnowSnincrTotR = hvySnowSnincrTotR;
    }

    public double getHvySnowSnincrTotY() {
        return hvySnowSnincrTotY;
    }

    public void setHvySnowSnincrTotY(double hvySnowSnincrTotY) {
        this.hvySnowSnincrTotY = hvySnowSnincrTotY;
    }

    public double getHvySnowDepthR() {
        return hvySnowDepthR;
    }

    public void setHvySnowDepthR(double hvySnowDepthR) {
        this.hvySnowDepthR = hvySnowDepthR;
    }

    public double getHvySnowDepthY() {
        return hvySnowDepthY;
    }

    public void setHvySnowDepthY(double hvySnowDepthY) {
        this.hvySnowDepthY = hvySnowDepthY;
    }
    
    public void updateData(SnowDisplayProductData newData)
    {
        blizWrnVisR = newData.getBlizWrnVisR();
        blizWrnVisY = newData.getBlizWrnVisY();
        blizWrnWindSpdR = newData.getBlizWrnWindSpdR();
        blizWrnWindSpdY = newData.getBlizWrnWindSpdY();
        blizWrnGustSpdR = newData.getBlizWrnGustSpdR();
        blizWrnGustSpdY = newData.getBlizWrnGustSpdY();
        blizWrnPeakWindR = newData.getBlizWrnPeakWindR();
        blizWrnPeakWindY = newData.getBlizWrnPeakWindY();
        
        frzPrecipTempR = newData.getFrzPrecipTempR();
        frzPrecipTempY = newData.getFrzPrecipTempY();
        frzPrecipHrlyPrcpR = newData.getFrzPrecipHrlyPrcpR();
        frzPrecipHrlyPrcpY = newData.getFrzPrecipHrlyPrcpY();
        
        hvySnowSnincrHrR = newData.getHvySnowSnincrHrR();
        hvySnowSnincrHrY = newData.getHvySnowSnincrHrY();
        hvySnowSnincrTotR = newData.getHvySnowSnincrTotR();
        hvySnowSnincrTotY = newData.getHvySnowSnincrTotY();
        hvySnowDepthR = newData.getHvySnowDepthR();
        hvySnowDepthY = newData.getHvySnowDepthY();
    }
}
