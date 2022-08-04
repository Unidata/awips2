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
package com.raytheon.viz.lightning;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Implements persistable lightning resource properties and factory
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 18, 2009  1959     chammack  Initial creation
 * Feb 27, 2013  DCS 152  jgerth    Support for WWLLN and multiple sources
 * Jun 19, 2014  3214     bclement  added pulse and cloud flash support
 * Jul 07, 2014  3333     bclement  removed plotLightSource field
 * Mar 05, 2015  4233     bsteffen  include source in cache key.
 * Jul 01, 2015  4597     bclement  added DisplayType
 * Jul 02, 2015  4605     bclement  don't show current bin as available
 * Sep 25, 2015  4605     bsteffen  repeat binning
 * Nov 04, 2015  5090     bsteffen  Add ability to get available times from a
 *                                  range.
 * May 19, 2016  3253     bsteffen  Deprecate countPosition in favor of
 *                                  dynamically positioned extra text
 * Jun 20, 2017  DR20107  kingfield/meyer Added GLM display functionality
 * Feb 13, 2020  DCS21150 mporricelli  Update legends and count labels
 *
 * </pre>
 *
 * @author chammack
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LightningResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LightningResourceData.class);

    /*
     * certain combination of lightning types get specific display labels
     */
    // Kingfield/Meyer DEV - Add labels in DisplayType for the GLM (GLM_FLASH,
    // GLM_EVENT, GLM_GROUP)
    public static enum DisplayType {
        UNDEFINED(""),
        CLOUD_FLASH("Cloud Flash"),
        NEGATIVE("Negative"),
        POSITIVE("Positive"),
        PULSE("Pulse"),
        TOTAL_FLASH("Total"),
        CLOUD_TO_GROUND("Cloud to Ground"),
        GLM_FLASH("GLM Flash"),
        GLM_EVENT("GLM Event"),
        GLM_GROUP("GLM Group"),
        GLD_PULSE("Cloud Pulse NLDN &"),
        GLD_CG("CG Return Stroke NLDN &"),
        GLD_CG_NEG("Negative CG Return Stroke NLDN &"),
        GLD_CG_POS("Positive CG Return Stroke NLDN &"),
        GLD_TOTAL("Total NLDN &"),
        NLDN_ENTLN_CG("CG Flash"),
        NLDN_ENTLN_CG_NEG("Negative CG Flash"),
        NLDN_ENTLN_CG_POS("Positive CG Flash");

        public final String label;

        private DisplayType(String name) {
            this.label = name;
        }
    }

    @XmlAttribute
    private boolean handlingPositiveStrikes = true;

    @XmlAttribute
    private boolean handlingNegativeStrikes = true;

    @XmlAttribute
    private boolean handlingCloudFlashes = false;

    @XmlAttribute
    private boolean handlingPulses = false;

    @XmlAttribute
    @Deprecated
    private Integer countPosition;

    /**
     * This field controls whether the frame for the current time is displayed.
     *
     * When it is true then all data is displayed including the current frame,
     * since the current frame may not be fully populated this frame may be more
     * sparse than others.
     *
     * When this is false then the frame for the current time is not displayed.
     */
    @XmlAttribute
    private boolean liveDisplay = false;

    /**
     * @See {@link RepeatingBinOffset}
     */
    @XmlElement
    private int binRepeatCount = 1;

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        if (countPosition != null && countPosition != 0) {
            statusHandler
                    .info("Ignoring count position of lightning resource.");
        }
        countPosition = null;
        LightningResource rsc = new LightningResource(this, loadProperties);
        List<BinLightningRecord> records = new ArrayList<>(objects.length);
        for (PluginDataObject pdo : objects) {
            if (pdo instanceof BinLightningRecord) {
                records.add((BinLightningRecord) pdo);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Received wrong type of data.  Got: " + pdo.getClass()
                                + " Expected: " + BinLightningRecord.class);
            }
        }
        rsc.addRecords(records);

        return rsc;
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] rval;
        DataTime[] allTimes = super.getAvailableTimes();

        if (liveDisplay || allTimes == null || allTimes.length < 1) {
            rval = allTimes;
        } else {
            SimulatedTime systemTime = SimulatedTime.getSystemTime();
            DataTime currentBin = binOffset
                    .getNormalizedTime(new DataTime(systemTime.getTime()));
            List<DataTime> pastTimes = new ArrayList<>(Arrays.asList(allTimes));
            /* remove current bin which may not be fully populated yet */
            pastTimes.remove(currentBin);
            rval = pastTimes.toArray(new DataTime[0]);
        }
        return rval;
    }

    @Override
    protected DataTime[] getAvailableTimes(
            Map<String, RequestConstraint> constraintMap, BinOffset binOffset)
            throws VizException {
        if (binOffset != null
                && binOffset.getInterval() <= TimeUtil.SECONDS_PER_MINUTE) {
            return getAvailableTimesFromRange(constraintMap, binOffset);
        } else {
            return super.getAvailableTimes(constraintMap, binOffset);

        }
    }

    /**
     * Replacement for {@link #getAvailableTimes(Map, BinOffset)} that includes
     * the binned end times in the result. Normally binning is only applied to
     * the start time of the range, so it is possible to miss frames where data
     * is available at the end of the range. For very small bins it is necessary
     * to use both start and end time to ensure all frames are displayed.
     */
    protected DataTime[] getAvailableTimesFromRange(
            Map<String, RequestConstraint> constraintMap, BinOffset binOffset)
            throws VizException {
        DbQueryRequest request = new DbQueryRequest(constraintMap);
        request.addRequestField(PluginDataObject.STARTTIME_ID);
        request.addRequestField(PluginDataObject.ENDTIME_ID);
        request.setDistinct(true);
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(request);
        Date[] startTimes = response
                .getFieldObjects(PluginDataObject.STARTTIME_ID, Date.class);
        Date[] endTimes = response.getFieldObjects(PluginDataObject.ENDTIME_ID,
                Date.class);
        Set<DataTime> binnedTimes = new HashSet<>(startTimes.length);
        for (Date time : startTimes) {
            DataTime dataTime = new DataTime(time);
            dataTime = binOffset.getNormalizedTime(dataTime);
            binnedTimes.add(dataTime);
        }
        for (Date time : endTimes) {
            DataTime dataTime = new DataTime(time);
            dataTime = binOffset.getNormalizedTime(dataTime);
            binnedTimes.add(dataTime);
        }
        DataTime[] allTimes = binnedTimes.toArray(new DataTime[0]);
        Arrays.sort(allTimes);
        return allTimes;
    }

    @Override
    protected PluginDataObject[] requestPluginDataObjects(
            Collection<DataTime> loadSet) throws VizException {
        if (binOffset == null || binRepeatCount <= 1) {
            return super.requestPluginDataObjects(loadSet);
        } else {
            Set<DataTime> newLoadSet = new HashSet<>();
            RepeatingBinOffset binRepeater = getRepeatingBinOffset();
            for (DataTime loadTime : loadSet) {
                newLoadSet.addAll(binRepeater.getBinsToRequest(loadTime));
            }
            return super.requestPluginDataObjects(newLoadSet);
        }
    }

    public RepeatingBinOffset getRepeatingBinOffset() {
        return new RepeatingBinOffset(binOffset, binRepeatCount);
    }

    @Override
    public boolean isRetrieveData() {
        return true;
    }

    /**
     * @return the handlingPositiveStrikes
     */
    public boolean isHandlingPositiveStrikes() {
        return handlingPositiveStrikes;
    }

    /**
     * @param handlingPositiveStrikes
     *            the handlingPositiveStrikes to set
     */
    public void setHandlingPositiveStrikes(boolean handlingPositiveStrikes) {
        this.handlingPositiveStrikes = handlingPositiveStrikes;
    }

    /**
     * @return the handlingNegativeStrikes
     */
    public boolean isHandlingNegativeStrikes() {
        return handlingNegativeStrikes;
    }

    /**
     * @param handlingNegativeStrikes
     *            the handlingNegativeStrikes to set
     */
    public void setHandlingNegativeStrikes(boolean handlingNegativeStrikes) {
        this.handlingNegativeStrikes = handlingNegativeStrikes;
    }

    /**
     * @return the handlingCloudFlashes
     */
    public boolean isHandlingCloudFlashes() {
        return handlingCloudFlashes;
    }

    /**
     * @param handlingCloudFlashes
     *            the handlingCloudFlashes to set
     */
    public void setHandlingCloudFlashes(boolean handlingCloudFlashes) {
        this.handlingCloudFlashes = handlingCloudFlashes;
    }

    /**
     * @return the handlingPulses
     */
    public boolean isHandlingPulses() {
        return handlingPulses;
    }

    /**
     * @param handlingPulses
     *            the handlingPulses to set
     */
    public void setHandlingPulses(boolean handlingPulses) {
        this.handlingPulses = handlingPulses;
    }

    /**
     * @deprecated count text is now positioned dynamically using extra text
     *             resource.
     */
    @Deprecated
    public int getCountPosition() {
        return countPosition;
    }

    /**
     * @deprecated count text is now positioned dynamically using extra text
     *             resource.
     */
    @Deprecated
    public void setCountPosition(int countPosition) {
        this.countPosition = countPosition;
    }

    public String getSource() {
        if (metadataMap != null
                && metadataMap.containsKey(LightningConstants.SOURCE)) {
            return metadataMap.get(LightningConstants.SOURCE)
                    .getConstraintValue();
        }
        return null;
    }

    public DisplayType getDisplayType() {
        DisplayType rval;
        String sourceType = this.getSource();
        byte bitset = 0x00;
        if (handlingCloudFlashes) {
            bitset |= 0x01;
        }
        if (handlingNegativeStrikes) {
            bitset |= 0x02;
        }
        if (handlingPositiveStrikes) {
            bitset |= 0x04;
        }
        if (handlingPulses) {
            bitset |= 0x08;
        }

        switch (bitset) {
        case 0x01:
            if (sourceType.equals("GLD")) {
                rval = DisplayType.GLD_PULSE;
            } else {
                rval = DisplayType.CLOUD_FLASH;
            }
            break;
        case 0x02:
            if (sourceType.equals("GLD")) {
                rval = DisplayType.GLD_CG_NEG;
            } else if (sourceType.equals("NLDN") || sourceType.equals("ENTLN")) {
                rval = DisplayType.NLDN_ENTLN_CG_NEG;
            } else {
                rval = DisplayType.NEGATIVE;
            }
            break;
        case 0x04:
            if (sourceType.equals("GLD")) {
                rval = DisplayType.GLD_CG_POS;
            } else if (sourceType.equals("NLDN") || sourceType.equals("ENTLN")) {
                rval = DisplayType.NLDN_ENTLN_CG_POS;
            } else {
                rval = DisplayType.POSITIVE;
            }
            break;
        case 0x06:
            if (sourceType.equals("GLD")) {
                rval = DisplayType.GLD_CG;
            } else if (sourceType.equals("NLDN")
                    || sourceType.equals("ENTLN")) {
                rval = DisplayType.NLDN_ENTLN_CG;

                // Kingfield/Meyer DEV - If source is from GLM - convert to GLM
                // type
                // for legend text
            } else if (sourceType.equals("GLMfl")) {
                rval = DisplayType.GLM_FLASH;
            } else if (sourceType.equals("GLMev")) {
                rval = DisplayType.GLM_EVENT;
            } else if (sourceType.equals("GLMgr")) {
                rval = DisplayType.GLM_GROUP;
            } else {
                rval = DisplayType.CLOUD_TO_GROUND;
            }
            break;
        case 0x07:
            if (sourceType.equals("GLD")) {
                rval = DisplayType.GLD_TOTAL;
            } else {
                rval = DisplayType.TOTAL_FLASH;
            }
            break;
        case 0x08:
            rval = DisplayType.PULSE;
            break;
        default:
            rval = DisplayType.UNDEFINED;
        }
        return rval;
    }

    public boolean isLiveDisplay() {
        return liveDisplay;
    }

    public void setLiveDisplay(boolean liveDisplay) {
        this.liveDisplay = liveDisplay;
    }

    public int getBinRepeatCount() {
        return binRepeatCount;
    }

    public void setBinRepeatCount(int binRepeatCount) {
        this.binRepeatCount = binRepeatCount;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + binRepeatCount;
        result = prime * result + (handlingCloudFlashes ? 1231 : 1237);
        result = prime * result + (handlingNegativeStrikes ? 1231 : 1237);
        result = prime * result + (handlingPositiveStrikes ? 1231 : 1237);
        result = prime * result + (handlingPulses ? 1231 : 1237);
        result = prime * result + (liveDisplay ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        LightningResourceData other = (LightningResourceData) obj;
        if (binRepeatCount != other.binRepeatCount)
            return false;
        if (handlingCloudFlashes != other.handlingCloudFlashes)
            return false;
        if (handlingNegativeStrikes != other.handlingNegativeStrikes)
            return false;
        if (handlingPositiveStrikes != other.handlingPositiveStrikes)
            return false;
        if (handlingPulses != other.handlingPulses)
            return false;
        if (liveDisplay != other.liveDisplay)
            return false;
        return true;
    }

}
