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
package com.raytheon.viz.mpe.ui.rsc;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.mpe.util.RFCSiteLookup;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData;
import com.raytheon.viz.mpe.ui.rfcmask.RfcMask;

/**
 * Resource data for MPEFieldResource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            mschenke    Initial creation
 * Mar 01, 2017 6160       bkowal      Implemented {@link #getDisplayString()}.
 * Oct 30, 2017 17911      wkwock      Display RFC QPE
 * May 04, 2018 20677      wkwock      Fix display WFO QPE data issue.
 * 
 * </pre>
 * 
 * @author mschenke
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MPEFieldResourceData extends AbstractMPEGriddedResourceData {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(MPEFieldResourceData.class);

    /** mask data of current RFC */
    private XmrgFile maskXmrg = null;

    public static enum ArealDisplay {
        GRID, BASIN, COUNTY, ZONE;
    }

    public static class MPEFieldFrame extends Frame {

        private final Date date;

        private short[] editedData;

        // data before masked
        private short[] origData = null;

        private short[] origEditedData;

        private List<RubberPolyData> polygonEdits;

        public MPEFieldFrame(Date date, short[] data,
                List<RubberPolyData> polygonEdits) {
            super(data);
            this.date = date;
            setPolygonEdits(polygonEdits);
        }

        public Date getDate() {
            return date;
        }

        public List<RubberPolyData> getPolygonEdits() {
            return polygonEdits;
        }

        public void setPolygonEdits(List<RubberPolyData> polygonEdits) {
            dispose();
            this.polygonEdits = new ArrayList<RubberPolyData>(polygonEdits);
            this.editedData = null;
        }

        public void setEditedData(short[] editedData) {
            this.editedData = editedData;
        }

        public short[] getEditedData() {
            return editedData;
        }
    }

    @XmlElement
    private DisplayFieldData fieldData = DisplayFieldData.mMosaic;

    @XmlElement
    private int accumulationInterval = 0;

    @XmlElement
    private ArealDisplay arealDisplay = ArealDisplay.GRID;

    @XmlAttribute
    private boolean displayIds = false;

    @XmlAttribute
    private boolean displayValues = false;

    /**
     * 
     */
    public MPEFieldResourceData() {
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                // TODO: Get MPELegendResource to append date/other attributes
                return fieldData.toString();
            }
        };
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new MPEFieldResource(this, loadProperties);
    }

    /**
     * @return the fieldData
     */
    public DisplayFieldData getFieldData() {
        return fieldData;
    }

    /**
     * @param fieldData
     *            the fieldData to set
     */
    public void setFieldData(DisplayFieldData fieldData) {
        this.fieldData = fieldData;
    }

    /**
     * @return the accumulationInterval
     */
    public int getAccumulationInterval() {
        return accumulationInterval;
    }

    /**
     * @param accumulationInterval
     *            the accumulationInterval to set
     */
    public void setAccumulationInterval(int accumulationInterval) {
        this.accumulationInterval = accumulationInterval;
    }

    /**
     * @return the arealDisplay
     */
    public ArealDisplay getArealDisplay() {
        return arealDisplay;
    }

    /**
     * @param arealDisplay
     *            the arealDisplay to set
     */
    public void setArealDisplay(ArealDisplay arealDisplay) {
        this.arealDisplay = arealDisplay;
    }

    /**
     * @return the displayIds
     */
    public boolean isDisplayIds() {
        return displayIds;
    }

    /**
     * @param displayIds
     *            the displayIds to set
     */
    public void setDisplayIds(boolean displayIds) {
        this.displayIds = displayIds;
    }

    /**
     * @return the displayValues
     */
    public boolean isDisplayValues() {
        return displayValues;
    }

    /**
     * @param displayValues
     *            the displayValues to set
     */
    public void setDisplayValues(boolean displayValues) {
        this.displayValues = displayValues;
    }

    @Override
    public int getDurationInHours() {
        return accumulationInterval;
    }

    @Override
    public String getCvUseString() {
        return fieldData.getCv_use();
    }

    @Override
    public String getDisplayString() {
        return fieldData.getDisplayString();
    }

    @Override
    public Unit<?> getDataUnits() {
        return getDataUnitsForField(fieldData);
    }

    @Override
    public Unit<?> getDisplayUnits() {
        return getDisplayUnitsForField(fieldData);
    }

    public static Unit<?> getDisplayUnitsForField(DisplayFieldData fieldData) {
        switch (fieldData) {
        case Height:
            return NonSI.FOOT;
        case Index:
        case Locspan:
        case Locbias:
        case LocspanDP:
        case LocbiasDP:
            return Unit.ONE;

        case mintempPrism:
        case maxtempPrism:
            return NonSI.FAHRENHEIT;

        case precipRatioField:
            return Unit.ONE; // unit-less

        default:
            return NonSI.INCH;
        }
    }

    public static Unit<?> getDataUnitsForField(DisplayFieldData fieldData) {
        switch (fieldData) {
        case Locbias:
        case LocbiasDP:
            return Unit.ONE.divide(100);
        case Height:
            return SI.METER;
        case Index:
        case Locspan:
        case LocspanDP:
            return Unit.ONE;
        case Prism:
            return SI.MILLIMETER;
        case mintempPrism:
        case maxtempPrism:
            return NonSI.FAHRENHEIT.divide(10);

        case precipRatioField:
            return Unit.ONE.divide(100); // unit-less

        default:
            return SI.MILLIMETER.divide(100);
        }
    }

    /**
     * mask data so data outside of RFC is not displayed.
     * 
     * @param frame
     */
    public void maskData(MPEFieldFrame frame) {
        if (frame.origData == null) {
            frame.origData = frame.data.clone();
            frame.origEditedData = frame.editedData.clone();

            if (maskXmrg == null) {
                AppsDefaults appsDefaults = AppsDefaults.getInstance();
                String mpeSiteId = appsDefaults.getToken("mpe_site_id")
                        .toUpperCase();
                String rfcName = RFCSiteLookup.RFCMAP.get(mpeSiteId);
                if (rfcName == null) {
                    logger.debug("Token mpe_site_id = " + mpeSiteId
                            + " is not a RFC.");
                    return;
                }
                try {
                    maskXmrg = RfcMask.getRFCMask(mpeSiteId);
                } catch (IOException e) {
                    logger.error(
                            "Failed to read " + mpeSiteId + "RFC mask file.",
                            e);
                }
            }

            short maskData[] = maskXmrg.getData();
            if (frame.data.length == maskData.length) {
                for (int i = 0; i < maskData.length; i++) {
                    if (maskData[i] == 0) {
                        frame.data[i] = CommonHydroConstants.MISSING_VALUE;
                        frame.editedData[i] = CommonHydroConstants.MISSING_VALUE;
                    }
                }
            } else {
                logger.warn(
                        "QPE data size does not match with RFC mask file size. Check file ascii/coord_*.dat.");
            }
            frame.disposeImage();
        }
    }

    /**
     * Undo what maskData() was done.
     * 
     * @param frame
     */
    public void unmaskData(MPEFieldFrame frame) {
        if (maskXmrg == null) {
            AppsDefaults appsDefaults = AppsDefaults.getInstance();
            String mpeSiteId = appsDefaults.getToken("mpe_site_id")
                    .toUpperCase();
            String rfcName = RFCSiteLookup.RFCMAP.get(mpeSiteId);
            if (rfcName == null) {
                logger.debug(
                        "Token mpe_site_id = " + mpeSiteId + " is not a RFC.");
                return;
            }

            try {
                maskXmrg = RfcMask.getRFCMask(mpeSiteId);
            } catch (IOException e) {
                logger.error("Failed to read " + mpeSiteId + "RFC mask file.",
                        e);
            }

        }
        short maskData[] = maskXmrg.getData();

        if (frame.origData != null) {
            for (int i = 0; i < frame.origData.length; i++) {
                frame.data[i] = frame.origData[i];
            }
            frame.origData = null;
        }

        if (frame.origEditedData != null) {
            for (int i = 0; i < maskData.length; i++) {
                if (maskData[i] == 0) {
                    frame.editedData[i] = frame.origEditedData[i];
                }
            }
            frame.origEditedData = null;
            frame.disposeImage();
        }

    }

}
