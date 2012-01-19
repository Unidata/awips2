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
package com.raytheon.viz.aviation.guidance;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.jobs.QueueJobRequest;

/**
 * Basic request to generate guidance
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009            njensen     Initial creation
 * Nov 12, 2010 6195       rferrel     Added types for clearing cache.
 * Apr 14, 2011 8065       rferrel     Implement equals
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GuidanceRequest extends QueueJobRequest<String[]> {

    public static enum GuidanceType {
        MOS("tafgen"), TAMP("tampgen"), GRID("gridgen"), ETA("tafgen"), METAR(
                "metargen"), METAR_CACHE("clearMetarCache"), TAF_CACHE(
                "clearCache");

        private String pythonMethod;

        private GuidanceType(String pythonMethod) {
            this.pythonMethod = pythonMethod;
        }

        public String getPythonMethod() {
            return pythonMethod;
        }
    }

    protected ArrayList<String> siteIDs;

    protected String model;

    protected String format;

    protected GuidanceType guidanceType;

    protected String tag;

    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("siteIDs", siteIDs);
        map.put("model", model);
        map.put("format", format);
        return map;
    }

    public ArrayList<String> getSiteIDs() {
        return siteIDs;
    }

    public void setSiteIDs(ArrayList<String> siteIDs) {
        this.siteIDs = siteIDs;
    }

    public String getModel() {
        return model;
    }

    public void setModel(String model) {
        this.model = model;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public GuidanceType getGuidanceType() {
        return guidanceType;
    }

    public void setGuidanceType(GuidanceType guidanceType) {
        this.guidanceType = guidanceType;
    }

    public String getTag() {
        return tag;
    }

    public void setTag(String tag) {
        this.tag = tag;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object o) {
        boolean state = false;
        if (o instanceof GuidanceRequest) {
            if (tag != null) {
                GuidanceRequest oReq = (GuidanceRequest) o;
                state = tag.equals(oReq.tag);
            }
        }
        if (state) {
            System.out.println("-- GuidanceRequest match tag:  " + tag);
        }
        return state;
    }
}
