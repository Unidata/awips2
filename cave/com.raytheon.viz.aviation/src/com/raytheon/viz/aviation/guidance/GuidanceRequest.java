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
 * Basic request to generate a request for the PythonGuidancdJob class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009            njensen     Initial creation
 * Nov 12, 2010 6195       rferrel     Added types for clearing cache.
 * Apr 14, 2011 8065       rferrel     Implement equals
 * 10Apr2014    #3005      lvenable    Added Eclipse generated hashcode method.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GuidanceRequest extends QueueJobRequest<String[]> {

    /**
     * This enumeration class defines the various type of guidance request that
     * can be received and associates the python method executed to service the
     * request. The METAR_CACHE and TAF_CACHE are special types used by
     * observer's to inform when new data has arrived and the cache needs to be
     * cleared and the new data retrieved.
     * 
     */
    public static enum GuidanceType {
        MOS("tafgen"), TAMP("tampgen"), GRID("gridgen"), ETA("tafgen"), METAR(
                "metargen"), METAR_CACHE("clearMetarCache"), TAF_CACHE(
                "clearCache");

        /**
         * The python method associated with the enumeration.
         */
        private String pythonMethod;

        /**
         * Constructor that associates a python method with the enumerator.
         * 
         * @param pythonMethod
         */
        private GuidanceType(String pythonMethod) {
            this.pythonMethod = pythonMethod;
        }

        /**
         * Get the enumeatior's python method.
         * 
         * @return pythonMethod
         */
        public String getPythonMethod() {
            return pythonMethod;
        }
    }

    /**
     * List of site ID to query for.
     */
    protected ArrayList<String> siteIDs;

    /**
     * the model being used.
     */
    protected String model;

    /**
     * The format to use.
     */
    protected String format;

    /**
     * Type of request.
     */
    protected GuidanceType guidanceType;

    /**
     * Tag associated with the request.
     */
    protected String tag;

    /**
     * This creates a default map with the mapping of the site IDs, model and
     * format. The intent is to pass the map to the python code. This can be
     * overriden by subclasses to fulfill the needs of a given python method.
     * 
     * @return map
     */
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("siteIDs", siteIDs);
        map.put("model", model);
        map.put("format", format);
        return map;
    }

    /**
     * The list of sites request is for.
     * 
     * @return siteIDs
     */
    public ArrayList<String> getSiteIDs() {
        return siteIDs;
    }

    /**
     * 
     * @param siteIDs
     *            list of sites array request is for
     */
    public void setSiteIDs(ArrayList<String> siteIDs) {
        this.siteIDs = siteIDs;
    }

    /**
     * The model argument for the request.
     * 
     * @return model
     */
    public String getModel() {
        return model;
    }

    /**
     * Set the model argument for the request.
     * 
     * @param model
     */
    public void setModel(String model) {
        this.model = model;
    }

    /**
     * The format argument for the request.
     * 
     * @return format
     */
    public String getFormat() {
        return format;
    }

    /**
     * Set the format argument for the request.
     * 
     * @param format
     */
    public void setFormat(String format) {
        this.format = format;
    }

    /**
     * 
     * @return guidanceType
     */
    public GuidanceType getGuidanceType() {
        return guidanceType;
    }

    /**
     * Set the guidance type for the request.
     * 
     * @param guidanceType
     */
    public void setGuidanceType(GuidanceType guidanceType) {
        this.guidanceType = guidanceType;
    }

    /**
     * The tag value for the request. Use to determine if the value has been
     * cached.
     * 
     * @return tag
     */
    public String getTag() {
        return tag;
    }

    /**
     * Set the tag value of the request.
     * 
     * @param tag
     */
    public void setTag(String tag) {
        this.tag = tag;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((format == null) ? 0 : format.hashCode());
        result = prime * result
                + ((guidanceType == null) ? 0 : guidanceType.hashCode());
        result = prime * result + ((model == null) ? 0 : model.hashCode());
        result = prime * result + ((siteIDs == null) ? 0 : siteIDs.hashCode());
        result = prime * result + ((tag == null) ? 0 : tag.hashCode());
        return result;
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

        // TODO remove debug statement.
        if (state) {
            System.out.println("-- GuidanceRequest match tag:  " + tag);
        }
        return state;
    }
}
