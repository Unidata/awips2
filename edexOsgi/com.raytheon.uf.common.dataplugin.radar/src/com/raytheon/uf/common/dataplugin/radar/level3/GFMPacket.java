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
package com.raytheon.uf.common.dataplugin.radar.level3;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataParameter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This is a GFM centric version of the Generic Packet
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/13     DCS51      zwang       Initial creation
 * 
 * </pre>
 * 
 * @author zwang
 * @version 1.0
 */

@DynamicSerialize
public class GFMPacket extends GenericDataPacket {
    public static enum GFMAttributeIDs {
    	DETECT_ID("detect_num"), FORECAST_DELTA_T("forecast_delta_t"),
    	PROPU("propU"), PROPV("propV"),
    	AVG_SPEED("avg_speed"), AVG_DIRECTION("avg_direction"),
    	WINDBEHINDU("windBehindU"), WINDBEHINDV("windBehindV"),
    	WINDBEHINDX("windBehindX"), WINDBEHINDY("windBehindY"),
    	WSHAZARD("wsHazard");

        private String name;

        private GFMAttributeIDs(String name) {
            this.name = name;
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @param name
         *            the name to set
         */
        public void setName(String name) {
            this.name = name;
        }

        public static GFMAttributeIDs getAttribute(String name) {
            for (GFMAttributeIDs id : GFMAttributeIDs.values()) {
                if (name.equals(id.getName())) {
                    return id;
                }
            }
            return null;
        }

        @Override
        public String toString() {
            return name;
        }
    }

    private static final int GFM_PRODUCT_CODE = 140;

    @DynamicSerializeElement
    private List<String> featureIDs;

    // Organizes the components/features based on gfm_ID(DETECT_ID:DELTA_T)
    @DynamicSerializeElement
    private HashMap<String, GenericDataComponent> features;

    @DynamicSerializeElement
    private HashMap<String, GenericDataParameter> params;

    static {
        PacketFactory.registerGenericPacketType(GFMPacket.class,
                GFM_PRODUCT_CODE);
    }

    public GFMPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public GFMPacket() {

    }

    public String getValue(String featureID, GFMAttributeIDs attributeID) {
        String rval = "";

        rval = features.get(featureID).getValue(attributeID.toString());

        return rval;
    }

    /**
     * @return the featureIDs
     */
    public List<String> getFeatureIDs() {
        return featureIDs;
    }

    /**
     * @param featureIDs
     *            the featureIDs to set
     */
    public void setFeatureIDs(List<String> featureIDs) {
        this.featureIDs = featureIDs;
    }

    /**
     * @return the features
     */
    public HashMap<String, GenericDataComponent> getFeatures() {
        return features;
    }

    /**
     * @param features
     *            the features to set
     */
    public void setFeatures(HashMap<String, GenericDataComponent> features) {
        this.features = features;
    }

    public GenericDataComponent getFeature(String eventID) {
        GenericDataComponent rval = null;

        rval = this.features.get(eventID);

        return rval;
    }

    public HashMap<String, GenericDataParameter> getParams() {
        return params;
    }

    public void setParams(HashMap<String, GenericDataParameter> params) {
        this.params = params;
    }

    /**
     * Converts the components to GFM Features and groups by gfmID
     */
    private void groupFeatures() {
        features = new HashMap<String, GenericDataComponent>();
        featureIDs = new ArrayList<String>();
        params = new HashMap<String, GenericDataParameter>();
        String detectID = "";
    	String deltaT = "";
    	String gfmID = "";
    	
        for (GenericDataParameter param : parameters) {
            params.put(param.getId(), param);
        }
        // Get the DETECT_ID out of each component
        for (GenericDataComponent currFeature : components) {
            // Loop through the values for the DETECT_ID
        	for (GenericDataParameter currParam : currFeature.getParameters()) {
                if (currParam.getId().equalsIgnoreCase(
                        GFMAttributeIDs.DETECT_ID.toString())) 
                	detectID = currParam.getValue();
                if (currParam.getId().equalsIgnoreCase(
                        GFMAttributeIDs.FORECAST_DELTA_T.toString())) 
                	deltaT = currParam.getValue();
            }
        	
        	// combine the detectID and deltaT to form a new ID (gfmID)
        	gfmID = detectID + ":" + deltaT;
        	features.put(gfmID, currFeature);
        	featureIDs.add(gfmID);
        }
    }
    
    @Override
    protected void init(DataInputStream in) throws IOException {
        super.init(in);
        
        // Relates all of the GFM feature based on the gfmID
        groupFeatures();
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
