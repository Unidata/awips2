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

public class SSDisplayMeteoData
{
    private String areaID;
    
    private double visR;
    private double visY;
    
    private double tempR;
    private double tempY;
    
    private double dewpointR;
    private double dewpointY;
    
    private double slpR;
    private double slpY;
    
    private double sstR;
    private double sstY;
    
    private double waveHgtR;
    private double waveHgtY;
    
    private double waveSteepR;
    private double waveSteepY;
    
    public SSDisplayMeteoData()
    {        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
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

    public double getSstR() {
        return sstR;
    }

    public void setSstR(double sstR) {
        this.sstR = sstR;
    }

    public double getSstY() {
        return sstY;
    }

    public void setSstY(double sstY) {
        this.sstY = sstY;
    }

    public double getWaveHgtR() {
        return waveHgtR;
    }

    public void setWaveHgtR(double waveHgtR) {
        this.waveHgtR = waveHgtR;
    }

    public double getWaveHgtY() {
        return waveHgtY;
    }

    public void setWaveHgtY(double waveHgtY) {
        this.waveHgtY = waveHgtY;
    }

    public double getWaveSteepR() {
        return waveSteepR;
    }

    public void setWaveSteepR(double waveSteepR) {
        this.waveSteepR = waveSteepR;
    }

    public double getWaveSteepY() {
        return waveSteepY;
    }

    public void setWaveSteepY(double waveSteepY) {
        this.waveSteepY = waveSteepY;
    }

    public void updateData(SSDisplayMeteoData newData)
    {
        visR = newData.getVisR();
        visY = newData.getVisY();
        
        tempR = newData.getTempR();
        tempY = newData.getTempY();
        
        dewpointR = newData.getDewpointR();
        dewpointY = newData.getDewpointY();
        
        slpR = newData.getSlpR();
        slpY = newData.getSlpY();
        
        sstR = newData.getSstR();
        sstY = newData.getSstY();
        
        waveHgtR = newData.getWaveHgtR();
        waveHgtY = newData.getWaveHgtY();
        
        waveSteepR = newData.getWaveSteepR();
        waveSteepY = newData.getWaveSteepY();
    }
}
