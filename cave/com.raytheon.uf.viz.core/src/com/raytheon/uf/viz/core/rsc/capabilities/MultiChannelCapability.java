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
package com.raytheon.uf.viz.core.rsc.capabilities;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.Channel;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.ChannelData;

/**
 * Capability for multi channel imagery
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MultiChannelCapability extends AbstractCapability {

    @XmlAccessorType(XmlAccessType.NONE)
    public static class ChannelSerializable {

        @XmlAttribute
        private String name;

        @XmlAttribute
        private Channel channel;

        @XmlAttribute
        private boolean invert;

        @XmlElement
        private float dataMin;

        @XmlElement
        private float dataMax;

        @XmlElement
        private float cmapMin;

        @XmlElement
        private float cmapMax;

        public ChannelSerializable() {

        }

        public ChannelSerializable(Channel channel, ChannelData channelData) {
            this.channel = channel;
            this.name = channelData.name;
            this.invert = channelData.invert;
            ColorMapParameters params = channelData.parameters;
            this.dataMin = params.getDataMin();
            this.dataMax = params.getDataMax();
            this.cmapMin = params.getColorMapMin();
            this.cmapMax = params.getColorMapMax();
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

        /**
         * @return the channel
         */
        public Channel getChannel() {
            return channel;
        }

        /**
         * @param channel
         *            the channel to set
         */
        public void setChannel(Channel channel) {
            this.channel = channel;
        }

        /**
         * @return the dataMin
         */
        public float getDataMin() {
            return dataMin;
        }

        /**
         * @param dataMin
         *            the dataMin to set
         */
        public void setDataMin(float dataMin) {
            this.dataMin = dataMin;
        }

        /**
         * @return the dataMax
         */
        public float getDataMax() {
            return dataMax;
        }

        /**
         * @param dataMax
         *            the dataMax to set
         */
        public void setDataMax(float dataMax) {
            this.dataMax = dataMax;
        }

        /**
         * @return the cmapMin
         */
        public float getCmapMin() {
            return cmapMin;
        }

        /**
         * @param cmapMin
         *            the cmapMin to set
         */
        public void setCmapMin(float cmapMin) {
            this.cmapMin = cmapMin;
        }

        /**
         * @return the cmapMax
         */
        public float getCmapMax() {
            return cmapMax;
        }

        /**
         * @param cmapMax
         *            the cmapMax to set
         */
        public void setCmapMax(float cmapMax) {
            this.cmapMax = cmapMax;
        }

        /**
         * @return the invert
         */
        public boolean isInvert() {
            return invert;
        }

        /**
         * @param invert
         *            the invert to set
         */
        public void setInvert(boolean invert) {
            this.invert = invert;
        }

    }

    public static class Marshaller extends
            XmlAdapter<ChannelSerializable[], HashMap<Channel, ChannelData>> {

        /*
         * (non-Javadoc)
         * 
         * @see
         * javax.xml.bind.annotation.adapters.XmlAdapter#unmarshal(java.lang
         * .Object)
         */
        @Override
        public HashMap<Channel, ChannelData> unmarshal(ChannelSerializable[] v)
                throws Exception {
            HashMap<Channel, ChannelData> channelMap = new HashMap<Channel, ChannelData>();
            for (ChannelSerializable cs : v) {
                ColorMapParameters params = new ColorMapParameters();
                params.setDataMin(cs.dataMin);
                params.setDataMax(cs.dataMax);
                params.setColorMapMin(cs.cmapMin);
                params.setColorMapMax(cs.cmapMax);
                channelMap.put(cs.channel, new ChannelData(cs.name, params,
                        cs.invert));
            }
            return channelMap;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * javax.xml.bind.annotation.adapters.XmlAdapter#marshal(java.lang.Object
         * )
         */
        @Override
        public ChannelSerializable[] marshal(HashMap<Channel, ChannelData> v)
                throws Exception {
            ChannelSerializable[] serializable = new ChannelSerializable[v
                    .size()];
            int i = 0;
            for (Entry<Channel, ChannelData> entry : v.entrySet()) {
                ChannelSerializable cs = new ChannelSerializable(
                        entry.getKey(), entry.getValue());
                serializable[i++] = cs;
            }
            return serializable;
        }

    }

    @XmlJavaTypeAdapter(value = Marshaller.class)
    private HashMap<Channel, ChannelData> channelMap = new HashMap<Channel, ChannelData>();

    private String[] names;

    /**
     * @return the names
     */
    public String[] getNames() {
        return names;
    }

    /**
     * @param names
     *            the names to set
     */
    public void setNames(String[] names) {
        this.names = names;
    }

    /**
     * @return the channelMap
     */
    public Map<Channel, ChannelData> getChannelMap() {
        return channelMap;
    }

    /**
     * @param channelMap
     *            the channelMap to set
     */
    public void setChannelMap(HashMap<Channel, ChannelData> channelMap) {
        if (channelMap == null) {
            channelMap = new HashMap<Channel, ChannelData>();
        }
        this.channelMap = channelMap;
        capabilityChanged();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability#
     * capabilityChanged()
     */
    @Override
    public void capabilityChanged() {
        super.capabilityChanged();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability#clone()
     */
    @Override
    public AbstractCapability clone() {
        MultiChannelCapability cap = new MultiChannelCapability();
        cap.channelMap = new HashMap<Channel, ChannelData>(channelMap);
        return cap;
    }

}
