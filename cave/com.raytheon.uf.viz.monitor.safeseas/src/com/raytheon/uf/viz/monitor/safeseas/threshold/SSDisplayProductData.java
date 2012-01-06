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

public class SSDisplayProductData
{
    private String areaID;
    
    private double scaWindSpeedR;
    private double scaWindSpeedY;
    private double scaGustSpeedR;
    private double scaGustSpeedY;
    private double scaPeakWindR;
    private double scaPeakWindY;
    private double scaWaveHgtR;
    private double scaWaveHgtY;
    
    private double galeWindSpeedR;
    private double galeWindSpeedY;
    private double galeGustSpeedR;
    private double galeGustSpeedY;
    private double galePeakWindR;
    private double galePeakWindY;
    
    private double stormWrnWindSpeedR;
    private double stormWrnWindSpeedY;
    private double stormWrnGustSpeedR;
    private double stormWrnGustSpeedY;
    private double stormWrnPeakWindR;
    private double stormWrnPeakWindY;
    
    private double hfwwWindSpeedR;
    private double hfwwWindSpeedY;
    private double hfwwGustSpeedR;
    private double hfwwGustSpeedY;
    private double hfwwPeakWindR;
    private double hfwwPeakWindY;

    public SSDisplayProductData()
    {        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
    }

    public double getScaWindSpeedR() {
        return scaWindSpeedR;
    }

    public void setScaWindSpeedR(double scaWindSpeedR) {
        this.scaWindSpeedR = scaWindSpeedR;
    }

    public double getScaWindSpeedY() {
        return scaWindSpeedY;
    }

    public void setScaWindSpeedY(double scaWindSpeedY) {
        this.scaWindSpeedY = scaWindSpeedY;
    }

    public double getScaGustSpeedR() {
        return scaGustSpeedR;
    }

    public void setScaGustSpeedR(double scaGustSpeedR) {
        this.scaGustSpeedR = scaGustSpeedR;
    }

    public double getScaGustSpeedY() {
        return scaGustSpeedY;
    }

    public void setScaGustSpeedY(double scaGustSpeedY) {
        this.scaGustSpeedY = scaGustSpeedY;
    }

    public double getScaPeakWindR() {
        return scaPeakWindR;
    }

    public void setScaPeakWindR(double scaPeakWindR) {
        this.scaPeakWindR = scaPeakWindR;
    }

    public double getScaPeakWindY() {
        return scaPeakWindY;
    }

    public void setScaPeakWindY(double scaPeakWindY) {
        this.scaPeakWindY = scaPeakWindY;
    }

    public double getScaWaveHgtR() {
        return scaWaveHgtR;
    }

    public void setScaWaveHgtR(double scaWaveHgtR) {
        this.scaWaveHgtR = scaWaveHgtR;
    }

    public double getScaWaveHgtY() {
        return scaWaveHgtY;
    }

    public void setScaWaveHgtY(double scaWaveHgtY) {
        this.scaWaveHgtY = scaWaveHgtY;
    }

    public double getGaleWindSpeedR() {
        return galeWindSpeedR;
    }

    public void setGaleWindSpeedR(double galeWindSpeedR) {
        this.galeWindSpeedR = galeWindSpeedR;
    }

    public double getGaleWindSpeedY() {
        return galeWindSpeedY;
    }

    public void setGaleWindSpeedY(double galeWindSpeedY) {
        this.galeWindSpeedY = galeWindSpeedY;
    }

    public double getGaleGustSpeedR() {
        return galeGustSpeedR;
    }

    public void setGaleGustSpeedR(double galeGustSpeedR) {
        this.galeGustSpeedR = galeGustSpeedR;
    }

    public double getGaleGustSpeedY() {
        return galeGustSpeedY;
    }

    public void setGaleGustSpeedY(double galeGustSpeedY) {
        this.galeGustSpeedY = galeGustSpeedY;
    }

    public double getGalePeakWindR() {
        return galePeakWindR;
    }

    public void setGalePeakWindR(double galePeakWindR) {
        this.galePeakWindR = galePeakWindR;
    }

    public double getGalePeakWindY() {
        return galePeakWindY;
    }

    public void setGalePeakWindY(double galePeakWindY) {
        this.galePeakWindY = galePeakWindY;
    }

    public double getStormWrnWindSpeedR() {
        return stormWrnWindSpeedR;
    }

    public void setStormWrnWindSpeedR(double stormWrnWindSpeedR) {
        this.stormWrnWindSpeedR = stormWrnWindSpeedR;
    }

    public double getStormWrnWindSpeedY() {
        return stormWrnWindSpeedY;
    }

    public void setStormWrnWindSpeedY(double stormWrnWindSpeedY) {
        this.stormWrnWindSpeedY = stormWrnWindSpeedY;
    }

    public double getStormWrnGustSpeedR() {
        return stormWrnGustSpeedR;
    }

    public void setStormWrnGustSpeedR(double stormWrnGustSpeedR) {
        this.stormWrnGustSpeedR = stormWrnGustSpeedR;
    }

    public double getStormWrnGustSpeedY() {
        return stormWrnGustSpeedY;
    }

    public void setStormWrnGustSpeedY(double stormWrnGustSpeedY) {
        this.stormWrnGustSpeedY = stormWrnGustSpeedY;
    }

    public double getStormWrnPeakWindR() {
        return stormWrnPeakWindR;
    }

    public void setStormWrnPeakWindR(double stormWrnPeakWindR) {
        this.stormWrnPeakWindR = stormWrnPeakWindR;
    }

    public double getStormWrnPeakWindY() {
        return stormWrnPeakWindY;
    }

    public void setStormWrnPeakWindY(double stormWrnPeakWindY) {
        this.stormWrnPeakWindY = stormWrnPeakWindY;
    }

    public double getHfwwWindSpeedR() {
        return hfwwWindSpeedR;
    }

    public void setHfwwWindSpeedR(double hfwwWindSpeedR) {
        this.hfwwWindSpeedR = hfwwWindSpeedR;
    }

    public double getHfwwWindSpeedY() {
        return hfwwWindSpeedY;
    }

    public void setHfwwWindSpeedY(double hfwwWindSpeedY) {
        this.hfwwWindSpeedY = hfwwWindSpeedY;
    }

    public double getHfwwGustSpeedR() {
        return hfwwGustSpeedR;
    }

    public void setHfwwGustSpeedR(double hfwwGustSpeedR) {
        this.hfwwGustSpeedR = hfwwGustSpeedR;
    }

    public double getHfwwGustSpeedY() {
        return hfwwGustSpeedY;
    }

    public void setHfwwGustSpeedY(double hfwwGustSpeedY) {
        this.hfwwGustSpeedY = hfwwGustSpeedY;
    }

    public double getHfwwPeakWindR() {
        return hfwwPeakWindR;
    }

    public void setHfwwPeakWindR(double hfwwPeakWindR) {
        this.hfwwPeakWindR = hfwwPeakWindR;
    }

    public double getHfwwPeakWindY() {
        return hfwwPeakWindY;
    }

    public void setHfwwPeakWindY(double hfwwPeakWindY) {
        this.hfwwPeakWindY = hfwwPeakWindY;
    }
    
    public void updateData(SSDisplayProductData newData)
    {
        scaWindSpeedR = newData.getScaWindSpeedR();
        scaWindSpeedY = newData.getScaWindSpeedY();
        scaGustSpeedR = newData.getScaGustSpeedR();
        scaGustSpeedY = newData.getScaGustSpeedY();
        scaPeakWindR = newData.getScaPeakWindR();
        scaPeakWindY = newData.getScaPeakWindY();
        scaWaveHgtR = newData.getScaWaveHgtR();
        scaWaveHgtY = newData.getScaWaveHgtY();
        
        galeWindSpeedR = newData.getGaleWindSpeedR();
        galeWindSpeedY = newData.getGaleWindSpeedY();
        galeGustSpeedR = newData.getGaleGustSpeedR();
        galeGustSpeedY = newData.getGaleGustSpeedY();
        galePeakWindR = newData.getGalePeakWindR();
        galePeakWindY = newData.getGalePeakWindY();
        
        stormWrnWindSpeedR = newData.getStormWrnWindSpeedR();
        stormWrnWindSpeedY = newData.getStormWrnWindSpeedY();
        stormWrnGustSpeedR = newData.getStormWrnGustSpeedR();
        stormWrnGustSpeedY = newData.getStormWrnGustSpeedY();
        stormWrnPeakWindR = newData.getStormWrnPeakWindR();
        stormWrnPeakWindY = newData.getStormWrnPeakWindY();
        
        hfwwWindSpeedR = newData.getHfwwWindSpeedR();
        hfwwWindSpeedY = newData.getHfwwWindSpeedY();
        hfwwGustSpeedR = newData.getHfwwGustSpeedR();
        hfwwGustSpeedY = newData.getHfwwGustSpeedY();
        hfwwPeakWindR = newData.getHfwwPeakWindR();
        hfwwPeakWindY = newData.getHfwwPeakWindY();
    }
}
