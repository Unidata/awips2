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
package com.raytheon.uf.edex.plugin.npp.viirs;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Object that maps header ids to region and channel information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class VIIRSHeaderMapping implements ISerializableObject {

    @XmlAccessorType(XmlAccessType.NONE)
    public static class VIIRSRegionInfo {

        @XmlAttribute
        private String id;

        @XmlElement
        private String region;

        /**
         * @return the id
         */
        public String getId() {
            return id;
        }

        /**
         * @param id
         *            the id to set
         */
        public void setId(String id) {
            this.id = id;
        }

        /**
         * @return the region
         */
        public String getRegion() {
            return region;
        }

        /**
         * @param region
         *            the region to set
         */
        public void setRegion(String region) {
            this.region = region;
        }

    }

    @XmlElement
    private VIIRSRegionInfo[] regionInfo;

    @XmlElement
    private VIIRSChannelInfo[] channelInfo;

    private Map<String, String> regionMap;

    private Map<String, VIIRSChannelInfo> channelMap;

    /**
     * @return the regionInfo
     */
    public VIIRSRegionInfo[] getRegionInfo() {
        return regionInfo;
    }

    /**
     * @param regionInfo
     *            the regionInfo to set
     */
    public void setRegionInfo(VIIRSRegionInfo[] regionInfo) {
        this.regionInfo = regionInfo;
    }

    /**
     * @return the channelInfo
     */
    public VIIRSChannelInfo[] getChannelInfo() {
        return channelInfo;
    }

    /**
     * @param channelInfo
     *            the channelInfo to set
     */
    public void setChannelInfo(VIIRSChannelInfo[] channelInfo) {
        this.channelInfo = channelInfo;
    }

    /**
     * Returns the region name for the given id
     * 
     * @param regionId
     * @return
     */
    public String getRegion(String regionId) {
        if (this.regionMap == null) {
            Map<String, String> regionMap = new HashMap<String, String>();
            if (regionInfo != null) {
                for (VIIRSRegionInfo info : regionInfo) {
                    regionMap.put(info.getId(), info.getRegion());
                }
            }
            this.regionMap = regionMap;
        }
        return regionMap.get(regionId);
    }

    /**
     * Returns the {@link VIIRSChannelInfo} for the specified channel info id
     * 
     * @param channelInfoId
     * @return
     */
    public VIIRSChannelInfo getChannelInfo(String channelInfoId) {
        if (this.channelMap == null) {
            Map<String, VIIRSChannelInfo> channelMap = new HashMap<String, VIIRSChannelInfo>();
            if (channelInfo != null) {
                for (VIIRSChannelInfo info : channelInfo) {
                    channelMap.put(info.getId(), info);
                }
            }
            this.channelMap = channelMap;
        }
        return channelMap.get(channelInfoId);
    }
}
