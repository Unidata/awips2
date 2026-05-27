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
 * This is a MBA (Microburst AMDA) centric version of the Generic Packet
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/26/2014   DCS 16776  zwang       Initial creation
 * 
 * </pre>
 * 
 * @author zwang
 * @version 1.0
 */

@DynamicSerialize
public class MBAPacket extends GenericDataPacket {
    public static enum MBAAttributeIDs {
        DETECT_ID("detect_num"), DELTAV("deltav"), MAXWINDSPEED("maxWindSpeed"),
                MAXSHEAR("maxShear"), CATEGORY("category");

        private String name;

        private MBAAttributeIDs(String name) {
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

        public static MBAAttributeIDs getAttribute(String name) {
            for (MBAAttributeIDs id : MBAAttributeIDs.values()) {
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
    
    public static enum MBACategory {
        WindShear(0), Microburst(1), Macroburst(2), SpeedShear(3);

        private final int catValue;

        private MBACategory(int catValue) {
            this.catValue = catValue;
        }

        /**
         * @return the catValue
         */
        public int getCatValue() {
            return catValue;
        }

        public static String getCatName(int cat) {
            for (MBACategory id : MBACategory.values()) {
                if (cat == id.getCatValue()) {
                    return id.toString();
                }
            }
            return null;
        }
    }
    
    @DynamicSerializeElement
    private List<String> featureIDs;

    // Organizes the components/features based on DETECT_ID
    @DynamicSerializeElement
    private HashMap<String, GenericDataComponent> features;

    @DynamicSerializeElement
    private HashMap<String, GenericDataParameter> params;

    public MBAPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public MBAPacket() {
    }

    public String getValue(String featureID, MBAAttributeIDs attributeID) {
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

        for (GenericDataParameter param : parameters) {
            params.put(param.getId(), param);
        }
        // Get the DETECT_ID out of each component
        for (GenericDataComponent currFeature : components) {
            // Loop through the values for the DETECT_ID
            for (GenericDataParameter currParam : currFeature.getParameters()) {
                if (currParam.getId().equalsIgnoreCase(
                        MBAAttributeIDs.DETECT_ID.toString()))
                    detectID = currParam.getValue();
            }

            features.put(detectID, currFeature);
            featureIDs.add(detectID);
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
