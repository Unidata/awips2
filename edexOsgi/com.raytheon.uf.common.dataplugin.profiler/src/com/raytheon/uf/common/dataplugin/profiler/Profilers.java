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
package com.raytheon.uf.common.dataplugin.profiler;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Profilers implements Serializable {

    private static final long serialVersionUID = 1L;

    private HashMap<Integer,ProfilerSite> stationIdMap = null;
    
    @XmlElement
    private List<ProfilerSite> site;
    
    public void addSite(ProfilerSite profilerSite) {
        if(site == null) {
            site = new ArrayList<ProfilerSite>();
        }
        site.add(profilerSite);
    }

    public List<ProfilerSite> getSites() {
        return site;
    }

    public void setSites(List<ProfilerSite> sites) {
        this.site = sites;
    }

    public boolean isLoaded() {
        return ((site != null) && (site.size() > 0));
    }

    /**
     * 
     * @param stationId
     * @return
     */
    public ProfilerSite get(Integer stationId) {
        return stationIdMap.get(stationId);
    }
    
    /**
     * Lazy creation of the stationIdMap.
     */
    private void populateMap() {
        stationIdMap = new HashMap<Integer,ProfilerSite>();
        for(ProfilerSite s : site) {
            try {
                Integer key = Integer.parseInt(s.getStationId());
                stationIdMap.put(key,s);
            } catch(NumberFormatException nfe) {
                
            }
        }
    }
    
    
    /**
     * 
     * @param filePath
     * @return
     */
    public static final Profilers loadProfilers(String filePath) {
        return loadProfilers(new File(filePath));
    }
    
    /**
     * 
     * @param filePath
     * @return
     */
    public static final Profilers loadProfilers(File file) {
        
        Profilers profilers = null;
        
        try {
            JAXBContext ctx = JAXBContext.newInstance(Profilers.class);

            Unmarshaller umsh = ctx.createUnmarshaller();
            
            profilers = (Profilers) umsh.unmarshal(file);

        } catch(Exception e) {
            e.printStackTrace();
        }
        profilers.populateMap();
        return profilers;
    }
    
}
