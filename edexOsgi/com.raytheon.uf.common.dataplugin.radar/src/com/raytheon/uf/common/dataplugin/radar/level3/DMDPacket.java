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
 * This is a DMD centric version of the Generic Packet
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2009            askripsk    Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

@DynamicSerialize
public class DMDPacket extends GenericDataPacket {
    public static enum DMDAttributeIDs {
        MESO_ID("meso_event_id"), ASSOCIATE_STORM_ID("storm_id"), AGE("age"), STRENGTH_RANK(
                "strength_rank"), STRENGTH_RANK_TYPE("strength_rank_type"), DETECTION_STATUS(
                "detection_status"), NUM_OF_2D_FEATURE("num2d"), ASSOCIATED_TVS(
                "tvs_near"), SPEED("prop_spd"), DIRECTION("prop_dir"), BASE_AZIMUTH(
                "ll_azm"), BASE_RANGE("ll_rng"), BASE_ON_LOWEST_ELEV(
                "lowest_elev"), BASE_HEIGHT("base", "BASE"), DEPTH("depth",
                "DEPTH"), NUM_PAST_POSITIONS("num_past_pos"), NUM_FCST_POSITIONS(
                "num_fcst_pos"), PAST_LAT("past_lat"), PAST_LON("past_lon"), FCST_LAT(
                "fcst_lat"), FCST_LON("fcst_lon"), BASE_DIAMETER("ll_diam"), BASE_ROTATIONAL_VEL(
                "ll_rot_vel", "RV"), MAX_ROTATIONAL_VEL("max_rot_vel", "MRV"), HEIGHT_MAX_ROTATIONAL_VEL(
                "height_max_rot_vel", "HMRV"), BASE_SHEAR("ll_shear"), MAX_SHEAR(
                "max_shear"), HEIGHT_MAX_SHEAR("height_max_shear"), BASE_GTG_VEL_DIFF(
                "ll_gtg_vel_dif", "G2G"), OVERLAPS_LOWER_FEATURE(
                "display_filter"), MSI("msi"), STORM_RELATIVE_DEPTH(
                "storm_rel_depth"), _0_2KM_ARL_CONVERGENCE("ll_convergence"), _2_4KM_ARL_CONVERGENCE(
                "ml_convergence"), ELEVATION_ANGLES("elev_angle"), ELEVATION_TIMES(
                "elev_time"), ELEVATION_INDEX("tilt_num"), AVG_SPD("avg_spd"), AVG_DIR(
                "avg_dir"), LAST_ELEV_FLAG("last_elev_flag"), _2D_HEIGHT(
                "height"), _2D_DIAMETER("diam"), _2D_ROTATIONAL_VEL("rot_vel"), _2D_SHEAR(
                "shear"), _2D_GTG_VEL_DIFF("gtgmax"), _2D_STRENGTH_RANK("rank"), _2D_LAT(
                "2d_lat"), _2D_LON("2d_lon");

        private String name;

        private String alternateName;

        private DMDAttributeIDs(String name) {
            this.name = name;
        }

        private DMDAttributeIDs(String name, String alternateName) {
            this.name = name;
            this.alternateName = alternateName;
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

        public static DMDAttributeIDs getAttribute(String name) {
            for (DMDAttributeIDs id : DMDAttributeIDs.values()) {
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

        public static DMDAttributeIDs getAlternateValue(String val) {
            for (DMDAttributeIDs id : DMDAttributeIDs.values()) {
                if (val.equals(id.getAlternateName())) {
                    return id;
                }
            }
            return null;
        }

        /**
         * @return the alternateName
         */
        public String getAlternateName() {
            return alternateName;
        }

        /**
         * @param alternateName
         *            the alternateName to set
         */
        public void setAlternateName(String alternateName) {
            this.alternateName = alternateName;
        }
    }

    private static final int DMD_PRODUCT_CODE = 149;

    @DynamicSerializeElement
    private List<String> featureIDs;

    // Organizes the components/features based on Meso_event_id
    @DynamicSerializeElement
    private HashMap<String, GenericDataComponent> features;

    @DynamicSerializeElement
    private HashMap<String, GenericDataParameter> params;

    static {
        PacketFactory.registerGenericPacketType(DMDPacket.class,
                DMD_PRODUCT_CODE);
    }

    public DMDPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public DMDPacket() {

    }

    public String getValue(String featureID, DMDAttributeIDs attributeID) {
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
     * Converts the components to Storm Cell Features and groups by Meso Event
     * ID.
     */
    private void groupFeatures() {
        features = new HashMap<String, GenericDataComponent>();
        featureIDs = new ArrayList<String>();
        params = new HashMap<String, GenericDataParameter>();
        for (GenericDataParameter param : parameters) {
            params.put(param.getId(), param);
        }
        // Get the Meso_event_id out of each component
        FEATURE: for (GenericDataComponent currFeature : components) {
            // Loop through the values for the storm feature until the id is
            // found
            for (GenericDataParameter currParam : currFeature.getParameters()) {
                if (currParam.getId().equalsIgnoreCase(
                        DMDAttributeIDs.MESO_ID.toString())) {
                    // Save the Storm Feature based on the Meso Event ID
                    features.put(currParam.getValue(), currFeature);

                    // Save the Meso Event ID
                    featureIDs.add(currParam.getValue());

                    // Skip to the next Storm Feature
                    continue FEATURE;
                }
            }
        }

    }

    @Override
    protected void init(DataInputStream in) throws IOException {
        super.init(in);

        // Relates all of the storm data based on the Meso Event ID
        groupFeatures();
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
