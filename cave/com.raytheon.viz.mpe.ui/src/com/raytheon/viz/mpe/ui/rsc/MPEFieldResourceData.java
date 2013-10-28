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

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData;

/**
 * Resource data for MPEFieldResource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MPEFieldResourceData extends AbstractMPEGriddedResourceData {

    public static enum ArealDisplay {
        GRID, BASIN, COUNTY, ZONE;
    }

    public static class MPEFieldFrame extends Frame {

        private final Date date;

        private short[] editedData;

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
                // TODO: Get MPELegendResource to append date/other crap
                return fieldData.toString();
            }
        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getDurationInHours
     * ()
     */
    @Override
    public int getDurationInHours() {
        return accumulationInterval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getCvUseString
     * ()
     */
    @Override
    public String getCvUseString() {
        return fieldData.getCv_use();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getDataUnits()
     */
    @Override
    public Unit<?> getDataUnits() {
        return getDataUnitsForField(fieldData);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getDisplayUnits
     * ()
     */
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
            return Unit.ONE;
        case mintempPrism:
        case maxtempPrism:
            return NonSI.FAHRENHEIT;
        }
        return NonSI.INCH;
    }

    public static Unit<?> getDataUnitsForField(DisplayFieldData fieldData) {
        switch (fieldData) {
        case Locbias:
            return Unit.ONE.divide(100);
        case Height:
            return SI.METER;
        case Index:
        case Locspan:
            return Unit.ONE;
        case Prism:
            return SI.MILLIMETER;
        case mintempPrism:
        case maxtempPrism:
            return NonSI.FAHRENHEIT.divide(10);
        }
        return SI.MILLIMETER.divide(100);
    }
}
