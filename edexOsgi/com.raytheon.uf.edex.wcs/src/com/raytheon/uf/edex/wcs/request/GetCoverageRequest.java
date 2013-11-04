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
package com.raytheon.uf.edex.wcs.request;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;

import javax.xml.bind.DatatypeConverter;

import net.opengis.gml.v_3_1_1.EnvelopeType;
import net.opengis.gml.v_3_1_1.TimeInstantPropertyType;
import net.opengis.gml.v_3_1_1.TimeInstantType;
import net.opengis.gml.v_3_1_1.TimePeriodPropertyType;
import net.opengis.gml.v_3_1_1.TimePeriodType;
import net.opengis.gml.v_3_1_1.TimePositionType;
import net.opengis.ows.v_1_1_0.BoundingBoxType;
import net.opengis.wcs.v_1_1_2.AxisSubset;
import net.opengis.wcs.v_1_1_2.DomainSubsetType;
import net.opengis.wcs.v_1_1_2.GetCoverage;
import net.opengis.wcs.v_1_1_2.GridCrsType;
import net.opengis.wcs.v_1_1_2.OutputType;
import net.opengis.wcs.v_1_1_2.RangeSubsetType;
import net.opengis.wcs.v_1_1_2.RangeSubsetType.FieldSubset;
import net.opengis.wcs.v_1_1_2.TimeSequenceType;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.gml3_1_1.EnvelopeConverter;
import com.raytheon.uf.edex.ogc.common.spatial.BoundingBoxUtil;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.reg.RangeAxis;
import com.raytheon.uf.edex.wcs.reg.RangeField;

public class GetCoverageRequest extends WcsRequest {

    protected String externalId;

    protected String internalId;

    protected String format;

    protected Composite3DBoundingBox bbox;

    protected List<RangeField> fields;

    protected boolean store = false;

    protected DataTime timeSequence;

    protected String gridBaseCrs;

    protected String gridType;

    protected List<Double> gridOrigin;

    protected List<Double> gridOffsets;

    protected boolean defacto = false;

    public GetCoverageRequest() {
        super(Type.GetCoverage);
    }

    public GetCoverageRequest(GetCoverage req) throws WcsException {
        super(Type.GetCoverage);
        this.request = req;
        if (req.isSetIdentifier()) {
            this.externalId = req.getIdentifier().getValue();
            this.internalId = externalToInternal(this.externalId);
        }
        if (req.isSetOutput()) {
            setOutput(req.getOutput());
        }
        if (req.isSetDomainSubset()) {
            setDomain(req.getDomainSubset());
        }
        if (req.isSetRangeSubset()) {
            setRange(req.getRangeSubset());
        }
    }

    /**
     * Set range fields from jaxb object
     * 
     * @param rangeSubset
     */
    private void setRange(RangeSubsetType range) {
        List<FieldSubset> subsets = range.getFieldSubset();
        if (subsets == null) {
            return;
        }
        this.fields = new ArrayList<RangeField>(subsets.size());
        for (FieldSubset sub : subsets) {
            this.fields.add(getField(sub));
        }
    }

    /**
     * Convert jaxb field object to internal representation
     * 
     * @param sub
     * @return
     */
    private RangeField getField(FieldSubset sub) {
        String id = sub.getIdentifier().getValue();
        RangeField rval = new RangeField(id, null);
        List<AxisSubset> axisList = sub.getAxisSubset();
        List<RangeAxis> target = new ArrayList<RangeAxis>(axisList.size());
        for (AxisSubset axis : axisList) {
            String axisId = axis.getIdentifier();
            List<String> keys = axis.getKey();
            target.add(new RangeAxis(axisId, new HashSet<String>(keys)));
        }
        rval.setAxis(target);
        return rval;
    }

    /**
     * Set domain from jaxb object
     * 
     * @param domain
     * @throws Exception
     */
    private void setDomain(DomainSubsetType domain) throws WcsException {
        if (domain.isSetBoundingBox()) {
            try {
                Object obj = domain.getBoundingBox().getValue();
                if (obj instanceof EnvelopeType) {
                    setBbox((EnvelopeType) obj);
                } else if (obj instanceof BoundingBoxType) {
                    setBbox((BoundingBoxType) obj);
                }
            } catch (OgcException e) {
                throw new WcsException(e);
            }
        }
        if (domain.isSetTemporalSubset()) {
            TimeSequenceType tseq = domain.getTemporalSubset();
            setTime(tseq);
        }
    }

    /**
     * Set bounding box from jaxb object
     * 
     * @param etype
     * @throws OgcException
     */
    public void setBbox(EnvelopeType etype) throws OgcException {
        if (EnvelopeConverter.getDims(etype) == 2) {
            ReferencedEnvelope ref = BoundingBoxUtil.convert2D(etype);
            this.bbox = new Composite3DBoundingBox(ref);
        } else if (EnvelopeConverter.getDims(etype) == 3) {
            this.bbox = BoundingBoxUtil.separate3DEnvelope(etype);
        } else {
            throw new OgcException(
                    com.raytheon.uf.edex.ogc.common.OgcException.Code.InvalidCRS,
                    "Unsupported number of dimensions");
        }
    }

    /**
     * Set bounding box from jaxb object
     * 
     * @param bbt
     * @throws OgcException
     */
    public void setBbox(BoundingBoxType bbt) throws OgcException {
        int dims = bbt.getLowerCorner().size();
        if (dims == 2) {
            this.bbox = new Composite3DBoundingBox(
                    BoundingBoxUtil.convert2D(bbt));
        } else if (dims == 3) {
            this.bbox = BoundingBoxUtil.separate3DEnvelope(bbt);
        } else {
            throw new OgcException(
                    com.raytheon.uf.edex.ogc.common.OgcException.Code.InvalidCRS,
                    "Unsupported number of dimensions");
        }
    }

    /**
     * Set time from jaxb object
     * 
     * @param tseq
     */
    private void setTime(TimeSequenceType tseq) {
        List<DataTime> times = parseTime(tseq);
        if (!times.isEmpty()) {
            // TODO handle multiple times
            this.timeSequence = times.get(0);
        }
    }

    /**
     * Convert jaxb time sequence into internal representation.
     * 
     * Returned data times will have instances in the refTime field and ranges
     * in the valid range field
     * 
     * @param tseq
     * @return
     */
    public static List<DataTime> parseTime(TimeSequenceType tseq) {
        if (!tseq.isSetTimePositionOrTimePeriod()) {
            return new ArrayList<DataTime>(0);
        }
        List<Object> times = tseq.getTimePositionOrTimePeriod();
        List<DataTime> rval = new ArrayList<DataTime>(times.size());
        for (Object obj : times) {
            if (obj instanceof TimeInstantType) {
                TimeInstantType inst = (TimeInstantType) obj;
                rval.add(getInstant(inst.getTimePosition()));
            } else if (obj instanceof TimePositionType) {
                rval.add(getInstant((TimePositionType) obj));
            } else if (obj instanceof TimeInstantPropertyType) {
                TimeInstantPropertyType prop = (TimeInstantPropertyType) obj;
                TimeInstantType inst = prop.getTimeInstant();
                rval.add(getInstant(inst.getTimePosition()));
            } else if (obj instanceof TimePeriodType) {
                TimePeriodType period = (TimePeriodType) obj;
                rval.add(getPeriod(period));
            } else if (obj instanceof TimePeriodPropertyType) {
                TimePeriodPropertyType prop = (TimePeriodPropertyType) obj;
                rval.add(getPeriod(prop.getTimePeriod()));
            } else if (obj instanceof net.opengis.wcs.v_1_1_2.TimePeriodType) {
                rval.add(getPeriod((net.opengis.wcs.v_1_1_2.TimePeriodType) obj));
            }
        }
        return rval;
    }

    /**
     * Convert jaxb time position to data time
     * 
     * Returned data time will have position value in refTime field
     * 
     * @param pos
     * @return
     */
    private static DataTime getInstant(TimePositionType pos) {
        return new DataTime(getTime(pos));
    }

    /**
     * Convert jaxb time period to data time
     * 
     * returned data time will have period value in valid period field
     * 
     * @param period
     * @return
     */
    private static DataTime getPeriod(
            net.opengis.wcs.v_1_1_2.TimePeriodType period) {
        TimePositionType begin = period.getBeginPosition();
        TimePositionType end = period.getEndPosition();
        return getPeriod(begin, end);
    }

    /**
     * Convert jaxb time period to data time
     * 
     * returned data time will have period value in valid period field
     * 
     * @param period
     * @return
     */
    private static DataTime getPeriod(TimePeriodType period) {
        TimePositionType begin;
        TimePositionType end;
        if (period.isSetBegin()) {
            TimeInstantPropertyType b = period.getBegin();
            TimeInstantType inst = b.getTimeInstant();
            begin = inst.getTimePosition();
        } else if (period.isSetBeginPosition()) {
            begin = period.getBeginPosition();
        } else {
            throw new IllegalArgumentException("No begin found in period");
        }
        if (period.isSetEnd()) {
            TimeInstantPropertyType b = period.getEnd();
            TimeInstantType inst = b.getTimeInstant();
            end = inst.getTimePosition();
        } else if (period.isSetEndPosition()) {
            end = period.getEndPosition();
        } else {
            throw new IllegalArgumentException("No end found in period");
        }
        return getPeriod(begin, end);
    }

    /**
     * Convert jaxb time period to data time
     * 
     * returned data time will have period value in valid period field
     * 
     * @param begin
     * @param end
     * @return
     */
    private static DataTime getPeriod(TimePositionType begin,
            TimePositionType end) {
        Calendar beginCal = getTime(begin);
        Calendar endCal = getTime(end);
        TimeRange validPeriod = new TimeRange(beginCal, endCal);
        return new DataTime(beginCal, validPeriod);
    }

    /**
     * Convert jaxb object to java calendar
     * 
     * @param pos
     * @return
     */
    private static Calendar getTime(TimePositionType pos) {
        String val = pos.getValue().get(0);
        return DatatypeConverter.parseDateTime(val);
    }

    /**
     * Set output format from jaxb object
     * 
     * @param output
     */
    private void setOutput(OutputType output) {
        if (output.isSetFormat()) {
            this.format = output.getFormat();
        }
        if (output.isSetGridCRS()) {
            GridCrsType base = output.getGridCRS();
            this.gridBaseCrs = base.getGridBaseCRS();
            this.gridType = base.getGridCS();
            this.gridOffsets = base.getGridOffsets();
            this.gridOrigin = base.getGridOrigin();
        }
        if (output.isSetStore()) {
            this.store = output.isStore();
        }
    }

    /**
     * @return the externalId
     */
    public String getExternalId() {
        return externalId;
    }

    /**
     * @param externalId
     *            the externalId to set
     */
    public void setExternalId(String externalId) {
        this.externalId = externalId;
    }

    /**
     * @return the internalId
     */
    public String getInternalId() {
        return internalId;
    }

    /**
     * @param internalId
     *            the internalId to set
     */
    public void setInternalId(String internalId) {
        this.internalId = internalId;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public List<RangeField> getFields() {
        return fields;
    }

    public void setFields(List<RangeField> fields) {
        this.fields = fields;
    }

    public boolean isStore() {
        return store;
    }

    public void setStore(boolean store) {
        this.store = store;
    }

    /**
     * @return the bbox
     */
    public Composite3DBoundingBox getBbox() {
        return bbox;
    }

    /**
     * @param bbox
     *            the bbox to set
     */
    public void setBbox(Composite3DBoundingBox bbox) {
        this.bbox = bbox;
    }

    public DataTime getTimeSequence() {
        return timeSequence;
    }

    public void setTimeSequence(DataTime timeSequence) {
        this.timeSequence = timeSequence;
    }

    public String getGridBaseCrs() {
        return gridBaseCrs;
    }

    public void setGridBaseCrs(String gridBaseCrs) {
        this.gridBaseCrs = gridBaseCrs;
    }

    public String getGridType() {
        return gridType;
    }

    public void setGridType(String gridType) {
        this.gridType = gridType;
    }

    public List<Double> getGridOrigin() {
        return gridOrigin;
    }

    public void setGridOrigin(List<Double> gridOrigin) {
        this.gridOrigin = gridOrigin;
    }

    public List<Double> getGridOffsets() {
        return gridOffsets;
    }

    public void setGridOffsets(List<Double> gridOffsets) {
        this.gridOffsets = gridOffsets;
    }

    /**
     * @param defacto
     *            the defacto to set
     */
    public void setDefacto(boolean defacto) {
        this.defacto = defacto;
    }

    /**
     * @return the defacto
     */
    public boolean isDefacto() {
        return defacto;
    }

    /**
     * @param identifier
     */
    public void setIdentifier(String identifier) {
        this.externalId = identifier;
        this.internalId = externalToInternal(this.externalId);
    }

}
