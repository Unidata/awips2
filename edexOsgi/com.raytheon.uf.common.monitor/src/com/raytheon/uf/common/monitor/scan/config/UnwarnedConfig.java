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
package com.raytheon.uf.common.monitor.scan.config;

/**
 * This class contains data for the unwarned alarm control.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2009 3039       lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class UnwarnedConfig
{
    private boolean unwarnedTor = false;
    private boolean torTvs = false;
    private boolean torMaxDbz = false;
    private boolean torMesoStRank = false;
    private boolean torMaxVil = false;    
    
    private boolean unwarnedSvr = false;
    private boolean svrTvs = false;
    private boolean svrMaxDbz = false;
    private boolean svrMesoStRank = false;
    private boolean svrMaxVil = false;
    private boolean hailSize = false;
    
    private int torMaxDbzVal = 0;
    private int torMesoStRankVal = 0;
    private int torMaxVilVal = 0;
    private int svrMaxDbzVal = 0;
    private int svrMesoStRankVal = 0;
    private int svrMaxVilVal = 0;
    private double hailSizeVal = 0.0;

    public UnwarnedConfig()
    {        
    }

    public boolean getUnwarnedTor()
    {
        return unwarnedTor;
    }

    public void setUnwarnedTor(boolean unwarnedTor)
    {
        this.unwarnedTor = unwarnedTor;
    }

    public boolean getTorTvs()
    {
        return torTvs;
    }

    public void setTorTvs(boolean torTvs) 
    {
        this.torTvs = torTvs;
    }

    public boolean getTorMaxDbz()
    {
        return torMaxDbz;
    }

    public void setTorMaxDbz(boolean torMaxDbz)
    {
        this.torMaxDbz = torMaxDbz;
    }

    public boolean getTorMesoStRank()
    {
        return torMesoStRank;
    }

    public void setTorMesoStRank(boolean torMesoStRank)
    {
        this.torMesoStRank = torMesoStRank;
    }

    public boolean getTorMaxVil()
    {
        return torMaxVil;
    }

    public void setTorMaxVil(boolean torMaxVil)
    {
        this.torMaxVil = torMaxVil;
    }

    public boolean getUnwarnedSvr()
    {
        return unwarnedSvr;
    }

    public void setUnwarnedSvr(boolean unwarnedSvr)
    {
        this.unwarnedSvr = unwarnedSvr;
    }

    public boolean getSvrTvs() 
    {
        return svrTvs;
    }

    public void setSvrTvs(boolean svrTvs)
    {
        this.svrTvs = svrTvs;
    }

    public boolean getSvrMaxDbz() 
    {
        return svrMaxDbz;
    }

    public void setSvrMaxDbz(boolean svrMaxDbz)
    {
        this.svrMaxDbz = svrMaxDbz;
    }

    public boolean getSvrMesoStRank() 
    {
        return svrMesoStRank;
    }

    public void setSvrMesoStRank(boolean svrMesoStRank)
    {
        this.svrMesoStRank = svrMesoStRank;
    }

    public boolean getSvrMaxVil() 
    {
        return svrMaxVil;
    }

    public void setSvrMaxVil(boolean svrMaxVil)
    {
        this.svrMaxVil = svrMaxVil;
    }

    public boolean getHailSize()
    {
        return hailSize;
    }

    public void setHailSize(boolean hailSize) 
    {
        this.hailSize = hailSize;
    }

    public int getTorMaxDbzVal()
    {
        return torMaxDbzVal;
    }

    public void setTorMaxDbzVal(int torMaxDbzVal)
    {
        this.torMaxDbzVal = torMaxDbzVal;
    }

    public int getTorMesoStRankVal() 
    {
        return torMesoStRankVal;
    }

    public void setTorMesoStRankVal(int torMesoStRankVal)
    {
        this.torMesoStRankVal = torMesoStRankVal;
    }

    public int getTorMaxVilVal()
    {
        return torMaxVilVal;
    }

    public void setTorMaxVilVal(int torMaxVilVal)
    {
        this.torMaxVilVal = torMaxVilVal;
    }

    public int getSvrMaxDbzVal()
    {
        return svrMaxDbzVal;
    }

    public void setSvrMaxDbzVal(int svrMaxDbzVal)
    {
        this.svrMaxDbzVal = svrMaxDbzVal;
    }

    public int getSvrMesoStRankVal() 
    {
        return svrMesoStRankVal;
    }

    public void setSvrMesoStRankVal(int svrMesoStRankVal)
    {
        this.svrMesoStRankVal = svrMesoStRankVal;
    }

    public int getSvrMaxVilVal()
    {
        return svrMaxVilVal;
    }

    public void setSvrMaxVilVal(int svrMaxVilVal)
    {
        this.svrMaxVilVal = svrMaxVilVal;
    }

    public double getHailSizeVal() 
    {
        return hailSizeVal;
    }

    public void setHailSizeVal(double hailSizeVal) 
    {
        this.hailSizeVal = hailSizeVal;
    }   
}
