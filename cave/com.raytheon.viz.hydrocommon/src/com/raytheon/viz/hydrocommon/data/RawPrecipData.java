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
package com.raytheon.viz.hydrocommon.data;

import java.util.ArrayList;

import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;

/**
 * Structure to hold PP and PC data for a particular lid.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2009  2257       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class RawPrecipData {
    private String lid = null;
    private ArrayList<Rawpp> rpp = new ArrayList<Rawpp>();
    private ArrayList<Rawpc> rpc = new ArrayList<Rawpc>();
    private ArrayList<ArrayList<Rawpc>> pcTsList = new ArrayList<ArrayList<Rawpc>>();
    private ArrayList<ArrayList<Rawpp>> ppTsList = new ArrayList<ArrayList<Rawpp>>();
    private ArrayList<String> pcTsLookup = new ArrayList<String>();
    private ArrayList<String> ppTsLookup = new ArrayList<String>();
    
    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }
    /**
     * @param lid the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }
    /**
     * @return the rpp
     */
    public ArrayList<Rawpp> getRpp() {
        return rpp;
    }
    /**
     * @param rpp the rpp to set
     */
    public void setRpp(ArrayList<Rawpp> rpp) {
        this.rpp = rpp;
    }
    /**
     * @return the rpc
     */
    public ArrayList<Rawpc> getRpc() {
        return rpc;
    }
    /**
     * @param rpc the rpc to set
     */
    public void setRpc(ArrayList<Rawpc> rpc) {
        this.rpc = rpc;
    }
    
    public void addRawpp(Rawpp rpp) {
        this.rpp.add(rpp);
        String ts = rpp.getTs();
        int index = -1;
        if (ppTsLookup.contains(ts)) {
            index = ppTsLookup.indexOf(ts);
        } else {
            ppTsLookup.add(ts);
            ArrayList<Rawpp> al = new ArrayList<Rawpp>();
            ppTsList.add(al);
            index = ppTsLookup.indexOf(ts);
        }
        
        if (index >= 0) {
            ArrayList<Rawpp> list = ppTsList.get(index);
            list.add(rpp);
        }
    }
    
    public void addRawpc(Rawpc rpc) {
        this.rpc.add(rpc);
        String ts = rpc.getTs();
        int index = -1;
        if (pcTsLookup.contains(ts)) {
            index = pcTsLookup.indexOf(ts);
        } else {
            pcTsLookup.add(ts);
            ArrayList<Rawpc> al = new ArrayList<Rawpc>();
            pcTsList.add(al);
            index = pcTsLookup.indexOf(ts);
        }
             
        if (index >= 0) {
            ArrayList<Rawpc> list = pcTsList.get(index);
            list.add(rpc);
        }
    }
    
    public ArrayList<String> getPpTsLookup() {
        return ppTsLookup;
    }
    
    public ArrayList<String> getPcTsLookup() {
        return pcTsLookup;
    }
    
    public ArrayList<Rawpp> getPpList(String ts) {
        ArrayList<Rawpp> retList = null;
        int index = -1;
        if (ppTsLookup.contains(ts)) {
            index = ppTsLookup.indexOf(ts);
        }
        if (index >= 0) {
            retList = ppTsList.get(index);
        }
        
        return retList;
    }
    
    public ArrayList<Rawpc> getPcList(String ts) {
        ArrayList<Rawpc> retList = null;
        int index = -1;
        if (pcTsLookup.contains(ts)) {
            index = pcTsLookup.indexOf(ts);
        }
        if (index >= 0) {
            retList = pcTsList.get(index);
        }
        
        return retList;
    }
    
    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        
        if (!(obj instanceof RawPrecipData)) {
            return false;
        }
        
        if ((((RawPrecipData) obj).getLid() == null) || (getLid() == null)) {
            return false;
        }
        
        if (((RawPrecipData) obj).getLid().equals(getLid())) {
            return true;
        }
        
        return false;
    }
    
    
}
