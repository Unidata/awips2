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
package com.raytheon.uf.edex.wcs.reg;

import java.awt.Point;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.measure.unit.Unit;

import org.apache.commons.collections.map.LRUMap;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.hibernate.Criteria;
import org.hibernate.SessionFactory;
import org.hibernate.classic.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.SimpleExpression;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.MathTransform2D;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.spatial.reprojection.AbstractDataReprojector.RequestWrapper;
import com.raytheon.uf.common.spatial.reprojection.ByteDataReprojector;
import com.raytheon.uf.common.spatial.reprojection.DataReprojector;
import com.raytheon.uf.common.spatial.reprojection.FloatDataReprojector;
import com.raytheon.uf.common.spatial.reprojection.IntDataReprojector;
import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.common.spatial.reprojection.ShortDataReprojector;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.AbstractOgcSource;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.PeekingIterator;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.filter.TemporalFilter;
import com.raytheon.uf.edex.ogc.common.spatial.AltUtil;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.ogc.common.spatial.RecordUtil;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate;
import com.raytheon.uf.edex.plugin.dataset.urn.URNLookup;
import com.raytheon.uf.edex.plugin.unitconverter.UnitLookup;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Default WcsSource
 * 
 * @author jelkins
 * @version 1.0
 */
public abstract class DefaultWcsSource<D extends SimpleDimension, L extends SimpleLayer<D>, T extends PluginDataObject>
        extends AbstractOgcSource implements IWcsSource<D, L> {

    protected LayerTransformer<D, L> transformer;

    private PluginDao _dao;

    protected PluginProperties props;

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    private static final String VALID_START = "dataTime.validPeriod.start";

    private static final String VALID_END = "dataTime.validPeriod.end";

    @SuppressWarnings("unchecked")
    private final Map<String, ReferencedDataRecord> fillCache = Collections
            .synchronizedMap(new LRUMap(2));

    /**
	 * 
	 */
    public DefaultWcsSource(PluginProperties props,
            LayerTransformer<D, L> transformer) {
        this.props = props;
        this.transformer = transformer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.reg.WcsSource#listCoverages()
     */
    @Override
    public List<CoverageDescription> listCoverages(boolean summary) {
        return getCoverageList(false, summary);
    }

    /**
     * @param wait
     * @return
     */
    protected List<CoverageDescription> getCoverageList(boolean wait,
            boolean summary) {
        List<CoverageDescription> coverageList = null;
        try {
            coverageList = getCoverageTransform().transform(
                    transformer.getLayers(), summary);
        } catch (Exception e) {
            log.error("Unable to get plugin dao", e);
            return new ArrayList<CoverageDescription>(0);

        }
        return coverageList;
    }

    /**
     * @return
     * @throws PluginException
     */
    protected PluginDao getDao() throws PluginException {
        if (_dao == null) {
            _dao = PluginFactory.getInstance().getPluginDao(
                    props.getPluginName());
        }
        return _dao;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.WcsSource#describeCoverage(java.lang.String)
     */
    @Override
    public CoverageDescription describeCoverage(String rawId)
            throws WcsException {
        String identifier = URNLookup.urnToLocal(rawId);
        return describeCoverageParsedId(identifier);
    }

    /**
     * Describe coverage using a parsed ID
     * 
     * @param parsedId
     * @return
     * @throws WcsException
     */
    public CoverageDescription describeCoverageParsedId(String parsedId)
            throws WcsException {
        CoverageDescription rval;
        try {
            L layer = transformer.find(parsedId);
            if (layer == null) {
                throw new WcsException(Code.LayerNotDefined);
            }
            rval = getCoverageTransform().transform(layer, false);
        } catch (OgcException e) {
            log.error("Problem accessing layers", e);
            throw new WcsException(Code.InternalServerError, e);
        } catch (Exception e) {
            log.error("Unable to get plugin dao", e);
            throw new WcsException(Code.InternalServerError);
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.reg.WcsSource#getCoverage(java.lang.String,
     * com.raytheon.uf.common.time.DataTime,
     * org.opengis.referencing.crs.CoordinateReferenceSystem,
     * com.vividsolutions.jts.geom.Envelope)
     */
    @Override
    public Coverage getCoverage(String rawId, DataTime time,
            Composite3DBoundingBox bbox, List<RangeField> rangeFields)
            throws WcsException {
        String identifier = URNLookup.urnToLocal(rawId);
        List<CoverageField> fields = getRecords(identifier, time, bbox,
                rangeFields);
        Coverage rval = new Coverage(rawId, fields);
        return rval;
    }

    /*
     * convenience wrapper for query arguments
     */
    protected static class QueryPackage {

        public String id;

        public Conjunction query;

        public Composite3DBoundingBox bbox;

        public DataTime time;

        /**
         * @param query
         * @param bbox
         * @param time
         */
        public QueryPackage(String id, Conjunction query,
                Composite3DBoundingBox bbox, DataTime time) {
            this.id = id;
            this.query = query;
            this.bbox = bbox;
            this.time = time;
        }

    }

    /**
     * Query and organize records for coverage
     * 
     * @param identifier
     * @param time
     * @param bbox
     * @param rangeFields
     * @return
     * @throws WcsException
     */
    protected List<CoverageField> getRecords(String identifier, DataTime time,
            Composite3DBoundingBox bbox, List<RangeField> rangeFields)
            throws WcsException {
        Map<String, Set<String>> fields = parseFields(rangeFields);
        Set<String> scalarValues = fields.get(getScalarKey());
        Conjunction and = getFilterClause(identifier, time, fields);

        List<CoverageField> rval;
        QueryPackage pkg = new QueryPackage(identifier, and, bbox, time);
        if (scalarValues == null || scalarValues.isEmpty()) {
            // all parameters
            rval = queryAllFieldValues(pkg);
        } else if (scalarValues.size() == 1) {
            // only one requested
            String param = scalarValues.iterator().next();
            rval = Arrays.asList(getFieldForScalar(param, pkg));
        } else {
            // for each requested
            // criterion aren't reusable
            byte[] orig = serialize(and);
            rval = new ArrayList<CoverageField>(scalarValues.size());
            for (String param : scalarValues) {
                pkg.query = deserialize(orig);
                rval.add(getFieldForScalar(param, pkg));
            }
        }

        return rval;
    }

    /**
     * Extract range fields into keyed values
     * 
     * @param rangeFields
     * @return
     * @throws WcsException
     */
    protected abstract Map<String, Set<String>> parseFields(
            List<RangeField> rangeFields) throws WcsException;

    /**
     * @return field name of scalar parameter
     */
    protected abstract String getScalarField();

    /**
     * @return key for scalar values in map returned by
     *         {@link DefaultWcsSource#parseFields(List)}
     */
    protected abstract String getScalarKey();

    /**
     * Get query for coverage records
     * 
     * @param identifier
     * @param time
     * @param fields
     * @return
     * @throws WcsException
     */
    protected abstract Conjunction getFilterClause(String identifier,
            DataTime time, Map<String, Set<String>> fields) throws WcsException;

    /**
     * Retrieve data for all values of the scalar field
     * 
     * @param restrictions
     * @param crs
     * @param bbox
     * @return
     * @throws WcsException
     */
    protected List<CoverageField> queryAllFieldValues(QueryPackage pkg)
            throws WcsException {
        List<T> records = query(pkg.query);
        Map<String, List<T>> groups = groupByField(records);
        List<CoverageField> rval = new ArrayList<CoverageField>(groups.size());
        for (String param : groups.keySet()) {
            CoverageField field = createField(param, groups.get(param), pkg);
            if (field != null) {
                rval.add(field);
            }
        }
        if (rval.isEmpty()) {
            String sampleField;
            if (!groups.isEmpty()) {
                sampleField = groups.keySet().iterator().next();
            } else {
                // panic, get first field for id
                CoverageDescription desc = describeCoverageParsedId(pkg.id);
                sampleField = desc.getRangeFields().get(0).getIdentifier();
            }
            rval.add(createFillField(sampleField, pkg));
        }
        return rval;
    }

    /**
     * Group records by scalar field value
     * 
     * @param records
     * @return
     * @throws WcsException
     */
    protected Map<String, List<T>> groupByField(List<T> records)
            throws WcsException {
        Map<String, List<T>> rval = new TreeMap<String, List<T>>();
        for (T record : records) {
            String value = getScalarValue(record);
            List<T> list = rval.get(value);
            if (list == null) {
                list = new ArrayList<T>();
                rval.put(value, list);
            }
            list.add(record);
        }
        return rval;
    }

    /**
     * Get scalar field value from record
     * 
     * @param record
     * @return
     * @throws WcsException
     */
    protected abstract String getScalarValue(T record) throws WcsException;

    /**
     * convert criterion to byte array
     * 
     * @param crit
     * @return
     * @throws WcsException
     */
    protected byte[] serialize(Criterion crit) throws WcsException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream out = null;
        try {
            out = new ObjectOutputStream(baos);
            out.writeObject(crit);
            out.close();
            return baos.toByteArray();
        } catch (IOException e) {
            log.error("Problem cloning criterion", e);
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * convert byte array to criterion
     * 
     * @param arr
     * @return
     * @throws WcsException
     */
    protected Conjunction deserialize(byte[] arr) throws WcsException {
        ByteArrayInputStream bais = new ByteArrayInputStream(arr);
        try {
            ObjectInputStream in = new ObjectInputStream(bais);
            Conjunction rval = (Conjunction) in.readObject();
            in.close();
            return rval;
        } catch (Exception e) {
            log.error("Problem cloning criterion", e);
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * Build CoverageField for scalar value
     * 
     * @param value
     * @return
     * @throws WcsException
     */
    protected CoverageField getFieldForScalar(String value, QueryPackage pkg)
            throws WcsException {
        List<T> results = queryScalar(value, pkg);
        CoverageField rval = createField(value, results, pkg);
        if (rval == null) {
            rval = createFillField(value, pkg);
        }
        return rval;
    }

    /**
     * Query for records that match specified scalar value
     * 
     * @param value
     * @param pkg
     * @return
     * @throws WcsException
     */
    protected List<T> queryScalar(String value, QueryPackage pkg)
            throws WcsException {
        pkg.query.add(getScalarCrit(value));
        return query(pkg.query);
    }

    /**
     * Create criterion for scalar value
     * 
     * @param value
     * @return
     */
    protected Criterion getScalarCrit(String value) {
        return Restrictions.eq(getScalarField(), value);
    }

    /**
     * Group results of query into coverage field
     * 
     * @param param
     * @return
     * @throws WcsException
     */
    protected CoverageField createField(String param, List<T> records,
            QueryPackage pkg) throws WcsException {
        if (records.isEmpty()) {
            return createFillField(param, pkg);
        }
        LinkedHashMap<Date, List<T>> map = groupByTime(records);
        LinkedHashMap<Date, List<VerticalSlice>> cubes = new LinkedHashMap<Date, List<VerticalSlice>>(
                map.size());
        T sample = map.get(map.keySet().iterator().next()).get(0);
        Composite3DBoundingBox target3d = getTarget3d(pkg.bbox, sample);
        for (Date time : map.keySet()) {
            List<VerticalSlice> slices = groupByAlt(map.get(time), target3d);
            if (!slices.isEmpty()) {
                cubes.put(time, slices);
            }
        }
        if (cubes.isEmpty()) {
            return null;
        }
        List<TemporalCube> rval = fillGaps(sample, pkg, cubes, target3d);
        IDataRecordFetcher fetcher = rval.get(0).getSlices().get(0).getRecord();
        ReferencedDataRecord rdr = fetcher.get(true);
        CoverageXYAxis xyAxis = new CoverageXYAxis(getGridGeometry(rdr),
                rdr.getEnvelope());
        return createField(rval, param, sample, xyAxis);
    }

    /**
     * Ensure that we have a filter for 3 dimensions. If bbox doesn't have
     * vertical bounds, an all-inclusive vertical bounding box is added
     * 
     * @param bbox
     * @param sample
     * @return
     * @throws WcsException
     */
    protected Composite3DBoundingBox getTarget3d(Composite3DBoundingBox bbox,
            T sample) throws WcsException {
        VerticalCoordinate sampleAlt = getAltitude(sample);
        if (sampleAlt == null) {
            log.error("Sample record has null altitude: " + sample.getDataURI());
            throw new WcsException(Code.InternalServerError);
        }
        if (bbox.hasVertical()) {
            Unit<?> units = sampleAlt.getUnits();
            if (units == null) {
                throw new WcsException(Code.InvalidParameterValue,
                        "3D query not applicable to 2D coverage");
            }
            return bbox;
        }
        VerticalCoordinate allFilter = new VerticalCoordinate(
                Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY,
                sampleAlt.getUnits(), sampleAlt.getRef());
        return new Composite3DBoundingBox(bbox.getHorizontal(), allFilter);
    }

    /**
     * Construct coverage field
     * 
     * @param cubes
     * @param param
     * @param sample
     * @return
     * @throws WcsException
     */
    protected CoverageField createField(List<TemporalCube> cubes, String param,
            T sample, CoverageXYAxis xyAxis) throws WcsException {
        Date[] times = new Date[cubes.size()];
        Iterator<TemporalCube> iter = cubes.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            TemporalCube cube = iter.next();
            times[i] = cube.getTime();
        }
        CoverageZAxis zAxis = cubes.get(0).getzAxis();
        CoverageDimensions dims = new CoverageDimensions(xyAxis, zAxis,
                new CoverageTAxis(times));
        CoverageField rval = new CoverageField(cubes, dims);
        rval.setName(param);
        rval.setStandardName(param);
        rval.setUnits(unidataUnitToString(getUcarUnit(sample)));
        rval.setPaddingValue(getFillValue(sample));
        return rval;
    }

    /**
     * Create a coverage field that is all fill values
     * 
     * @param param
     * @param pkg
     * @return
     * @throws WcsException
     */
    protected CoverageField createFillField(String param, QueryPackage pkg)
            throws WcsException {
        Criterion crit = getScalarCrit(param);
        T sample = querySample(crit);
        if (sample == null) {
            throw new WcsException(Code.InvalidParameterValue,
                    "Unable to find scalar field: " + param);
        }
        TemporalCube cube = createFillCube(getTarget3d(pkg.bbox, sample),
                sample, pkg);
        ReferencedEnvelope bbox = pkg.bbox.getHorizontal();
        CoverageXYAxis xyAxis = new CoverageXYAxis(getFillGeom(sample, bbox),
                bbox);
        return createField(Arrays.asList(cube), param, sample, xyAxis);
    }

    /**
     * Create a temporal cube that is all fill values
     * 
     * @param target3d
     * @param sample
     * @param pkg
     * @return
     */
    protected TemporalCube createFillCube(Composite3DBoundingBox target3d,
            T sample, QueryPackage pkg) {
        VerticalCoordinate vert = target3d.getVertical();
        String units = unidataUnitToString(UnitLookup.getInstance()
                .getUcarFromJsr(vert.getUnits()));
        VerticalSlice slice = createFillSlice(sample, pkg, vert.getMin());
        CoverageZAxis zAxis = new CoverageZAxis(
                new double[] { slice.getLevel() }, units,
                vert.getRef().longName, isUpPositive(sample));
        Date time;
        if (pkg.time != null) {
            time = pkg.time.getRefTime();
        } else {
            time = sample.getDataTime().getValidTime().getTime();
        }
        return new TemporalCube(Arrays.asList(slice), time, zAxis);
    }

    /**
     * Create a vertical slice that is all fill values
     * 
     * @param sample
     * @param pkg
     * @param level
     * @return
     */
    protected VerticalSlice createFillSlice(final T sample,
            final QueryPackage pkg, double level) {
        final ReferencedEnvelope bbox = pkg.bbox.getHorizontal();
        IDataRecordFetcher fillFetcher = new IDataRecordFetcher() {
            @Override
            public ReferencedDataRecord get(boolean cache) throws WcsException {
                return getFillRecord(sample, bbox);
            }
        };
        return new VerticalSlice(fillFetcher, level);
    }

    /**
     * Fill levels that are not in all cubes
     * 
     * @param sample
     * @param pkg
     * @param cubes
     * @param target3d
     * @return
     * @throws WcsException
     */
    protected List<TemporalCube> fillGaps(T sample, QueryPackage pkg,
            LinkedHashMap<Date, List<VerticalSlice>> cubes,
            Composite3DBoundingBox target3d) throws WcsException {
        TreeSet<Double> union = new TreeSet<Double>();
        for (Entry<Date, List<VerticalSlice>> e : cubes.entrySet()) {
            List<VerticalSlice> slices = e.getValue();
            for (VerticalSlice slice : slices) {
                union.add(slice.getLevel());
            }
        }

        VerticalCoordinate vert = target3d.getVertical();
        double[] levels = new double[union.size()];
        Iterator<Double> iter = union.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            levels[i] = iter.next();
        }
        String unitStr = vert.getUnits() == null ? "" : vert.getUnits()
                .toString();
        String refName = vert.getRef() == null ? "" : vert.getRef().longName;
        CoverageZAxis zAxis = new CoverageZAxis(levels, unitStr, refName,
                isUpPositive(sample));
        List<TemporalCube> rval = new ArrayList<TemporalCube>(cubes.size());
        for (Entry<Date, List<VerticalSlice>> e : cubes.entrySet()) {
            rval.add(fillGaps(sample, pkg, e.getValue(), e.getKey(), zAxis));
        }
        return rval;
    }

    /**
     * Fill levels that are in the expected set of levels but not in the cube
     * 
     * @param sample
     * @param pkg
     * @param slices
     * @param time
     * @param zAxis
     * @return
     */
    protected TemporalCube fillGaps(T sample, QueryPackage pkg,
            List<VerticalSlice> slices, Date time, CoverageZAxis zAxis) {
        PeekingIterator<VerticalSlice> from = new PeekingIterator<VerticalSlice>(
                slices.iterator());
        double[] expected = zAxis.getValue();
        List<VerticalSlice> target = new ArrayList<VerticalSlice>(
                expected.length);
        for (double level : expected) {
            if (!from.hasNext()) {
                // ran out of from, rest will be fills
                target.add(createFillSlice(sample, pkg, level));
                continue;
            }
            VerticalSlice possible = from.peek();
            if (possible.getLevel() == level) {
                target.add(possible);
                from.next(); // progress iterator
            } else {
                // missing level, use fill
                target.add(createFillSlice(sample, pkg, level));
            }
        }
        return new TemporalCube(target, time, zAxis);
    }

    /**
     * Safely convert units to string
     * 
     * @param u
     * @return
     */
    protected String javaxUnitToString(Unit<?> u) {
        return u != null ? u.toString() : "";
    }

    protected String unidataUnitToString(ucar.units.Unit u) {
        return u != null ? u.toString() : "";
    }

    /**
     * get field units from record
     * 
     * @param record
     * @return
     */
    protected abstract Unit<?> getScalarUnit(T record);

    protected abstract ucar.units.Unit getUcarUnit(T record);

    /**
     * @param record
     * @return true if increasing z values is up
     */
    protected abstract boolean isUpPositive(T record);

    /**
     * @param records
     * @return
     */
    private LinkedHashMap<Date, List<T>> groupByTime(List<T> records) {
        LinkedHashMap<Date, List<T>> rval = new LinkedHashMap<Date, List<T>>();
        for (T record : records) {
            Date time = record.getDataTime().getValidTime().getTime();
            List<T> list = rval.get(time);
            if (list == null) {
                list = new ArrayList<T>();
                rval.put(time, list);
            }
            list.add(record);
        }
        return rval;
    }

    /**
     * Group records into vertical slices
     * 
     * @param records
     * @param targetUnits
     * @param bbox
     * @return
     * @throws WcsException
     */
    protected List<VerticalSlice> groupByAlt(List<T> records,
            final Composite3DBoundingBox bbox) throws WcsException {
        final ReferencedEnvelope horiz = bbox.getHorizontal();
        VerticalCoordinate vertFilter = bbox.getVertical();
        List<VerticalSlice> rval = new ArrayList<VerticalSlice>(records.size());
        for (final T r : records) {
            VerticalCoordinate altitude = getAltitude(r);
            if (altitude == null) {
                log.error("Record missing altitude: " + r.getDataURI());
                continue;
            }
            VerticalCoordinate vertValue;
            try {
                vertValue = AltUtil.convert(vertFilter.getUnits(),
                        vertFilter.getRef(), altitude);
            } catch (Exception e) {
                log.debug("Unable to convert level", e.getMessage());
                continue;
            }
            if (vertFilter.compareTo(vertValue) != 0) {
                continue;
            }
            IDataRecordFetcher fetcher = new IDataRecordFetcher() {
                ReferencedDataRecord record;

                @Override
                public ReferencedDataRecord get(boolean cache)
                        throws WcsException {
                    if (record != null) {
                        return record;
                    }
                    ReferencedDataRecord rval = getDataRecord(r, horiz);
                    if (cache) {
                        record = rval;
                    }
                    return rval;
                }
            };
            rval.add(new VerticalSlice(fetcher, vertValue.getValue()));
        }
        Collections.sort(rval);
        return rval;
    }

    /**
     * get z value for record
     * 
     * @param record
     * @return
     * @throws WcsException
     */
    protected abstract VerticalCoordinate getAltitude(T record)
            throws WcsException;

    /**
     * @param time
     * @return null if time is null
     */
    protected Criterion parseTime(DataTime time) {
        if (time == null) {
            return null;
        }
        Criterion rval;
        // any interacts
        if (TemporalFilter.isRange(time)) {
            TimeRange period = time.getValidPeriod();
            SimpleExpression lhs = Restrictions
                    .le(VALID_START, period.getEnd());
            SimpleExpression rhs = Restrictions
                    .ge(VALID_END, period.getStart());
            rval = Restrictions.and(lhs, rhs);
        } else {
            SimpleExpression lhs = Restrictions.le(VALID_START,
                    time.getRefTime());
            SimpleExpression rhs = Restrictions
                    .ge(VALID_END, time.getRefTime());
            rval = Restrictions.and(lhs, rhs);
        }
        return rval;
    }

    @SuppressWarnings("unchecked")
    public T querySample(Criterion crit) throws WcsException {
        Session sess = null;
        try {
            PluginDao dao = getDao();
            SessionFactory sessFact = dao.getSessionFactory();
            sess = sessFact.openSession();
            Criteria criteria = sess.createCriteria(props.getRecord());
            criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
            criteria.setMaxResults(1);
            criteria.add(crit);
            criteria = modCriteria(criteria);
            return (T) criteria.uniqueResult();
        } catch (Exception e) {
            log.error("Problem querying", e);
            throw new WcsException(Code.InternalServerError);
        } finally {
            if (sess != null) {
                sess.close();
            }
        }
    }

    @SuppressWarnings("unchecked")
    public List<T> query(Criterion crit) throws WcsException {
        Session sess = null;
        try {
            PluginDao dao = getDao();
            SessionFactory sessFact = dao.getSessionFactory();
            sess = sessFact.openSession();
            Criteria criteria = sess.createCriteria(props.getRecord());
            criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
            criteria.add(crit);
            criteria = modCriteria(criteria);
            criteria.addOrder(Order.asc(VALID_START));
            return (List<T>) criteria.list();
        } catch (Exception e) {
            log.error("Problem querying", e);
            throw new WcsException(Code.InternalServerError);
        } finally {
            if (sess != null) {
                sess.close();
            }
        }
    }

    /**
     * Hook to add aliases to criteria, default is no change
     * 
     * @param criteria
     * @return
     */
    protected Criteria modCriteria(Criteria criteria) {
        return criteria;
    }

    /**
     * @param identifier
     * @return
     */
    protected Date getDefaultTime(String identifier) throws WcsException {
        try {
            SimpleLayer<?> layer = transformer.find(identifier);
            return layer.getDefaultTime();
        } catch (OgcException e) {
            log.error("Problem accessing layers", e);
            throw new WcsException(Code.InternalServerError, e);
        } catch (Exception e) {
            log.error("Unable to get plugin dao", e);
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * @param record
     * @param bbox
     * @return
     * @throws WcsException
     */
    protected ReferencedDataRecord getDataRecord(PluginDataObject record,
            ReferencedEnvelope bbox) throws WcsException {
        try {
            // get the projected slice from the DAO
            PluginDao dao = getDao();
            ReferencedDataRecord rval = RecordUtil.getProjected(dao, record,
                    bbox);

            // check if we need to pad ( possibly have no data and need to
            // create all "null" response
            if (rval == null) {
                // Need all "null" response
                rval = getFillRecord(record, bbox);
            } else if (rval.getEnvelope().equals(bbox)
                    || rval.getEnvelope().contains((Envelope) bbox)) {
                // All is well
            } else {
                // need to resize (grow only) and pad with nulls
                ReferencedDataRecord resultingRecord = padData(rval, bbox);
                rval = resultingRecord;
            }

            return rval;
        } catch (Exception e) {
            log.error("Unable to get reprojected data for record: " + record, e);
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * Get a referenced data record that is full of fill values. This method
     * caches internally.
     * 
     * @param record
     *            example record that is used as a template
     * @param bbox
     *            target bounds of record
     * @return
     * @throws WcsException
     */
    protected ReferencedDataRecord getFillRecord(PluginDataObject record,
            ReferencedEnvelope bbox) throws WcsException {
        ISpatialObject spatial = ((ISpatialEnabled) record).getSpatialObject();
        String key = toKey(spatial, bbox);
        ReferencedDataRecord rval = fillCache.get(key);
        if (rval != null) {
            return rval;
        }
        RequestWrapper req = getSlicedRequest(spatial, bbox);
        Point slicedDims = getSlicedDims(req);
        IDataRecord sample = getSample(record);
        Object array = makeArrayTypeFromRecord(sample, slicedDims.x
                * slicedDims.y);
        IDataRecord fill = makeDataRecordOfSameType(sample, array, 2,
                new long[] { slicedDims.x, slicedDims.y });
        ReferencedDataRecord tmp = new ReferencedDataRecord(fill, req.env);
        rval = padData(tmp, bbox);
        fillCache.put(key, rval);
        return rval;
    }

    /**
     * Create a cache key from spatial object and target bounds
     * 
     * @param spatial
     * @param env
     * @return
     */
    protected String toKey(ISpatialObject spatial, ReferencedEnvelope env) {
        StringBuilder b = new StringBuilder();
        b.append(spatial.getCrs().toWKT()).append(":");
        b.append(spatial.getGeometry().toString()).append(":");
        b.append(spatial.getNx()).append(":");
        b.append(spatial.getNy()).append(":");
        b.append(env.getCoordinateReferenceSystem().toWKT()).append(":");
        b.append(env.toString());
        return b.toString();
    }

    /**
     * Get target dimensions for fill record
     * 
     * @param req
     * @return
     * @throws WcsException
     */
    protected Point getSlicedDims(RequestWrapper req) throws WcsException {
        int[] maxes = req.req.getMaxIndexForSlab();
        int[] mins = req.req.getMinIndexForSlab();
        return new Point(maxes[0] - mins[0], maxes[1] - mins[1]);
    }

    /**
     * Get grid bounds and associated bounding box for slice
     * 
     * @param record
     * @param bbox
     * @return
     * @throws WcsException
     */
    protected RequestWrapper getSlicedRequest(ISpatialObject spatial,
            ReferencedEnvelope bbox) throws WcsException {
        try {
            ReferencedEnvelope nativeEnvelope = RecordUtil
                    .getNativeEnvelope(spatial);
            RequestWrapper rval = DataReprojector.getRequest(spatial,
                    nativeEnvelope, bbox);
            if (rval == null) {
                rval = new RequestWrapper();
                rval.req = Request.buildSlab(new int[] { 0, 0 }, new int[] { 1,
                        1 });
                CoordinateReferenceSystem targetCrs = bbox
                        .getCoordinateReferenceSystem();
                ReferencedEnvelope dataBounds = nativeEnvelope.transform(
                        targetCrs, true);
                double dx = dataBounds.getWidth() / spatial.getNx();
                double dy = dataBounds.getHeight() / spatial.getNy();
                double maxx = bbox.getMinX() + dx;
                double maxy = bbox.getMinY() + dy;
                rval.env = new ReferencedEnvelope(bbox.getMinX(), maxx,
                        bbox.getMinY(), maxy,
                        bbox.getCoordinateReferenceSystem());
            }
            return rval;
        } catch (Exception e) {
            log.error("Unable to make fill record", e);
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * Get grid geometry for fill record
     * 
     * @param record
     *            example record
     * @param bbox
     *            bounds of return
     * @return
     * @throws WcsException
     */
    protected GridGeometry2D getFillGeom(PluginDataObject record,
            ReferencedEnvelope bbox) throws WcsException {
        ReferencedDataRecord fill = getFillRecord(record, bbox);
        IDataRecord rec = fill.getRecord();
        int nx = (int) rec.getSizes()[0];
        int ny = (int) rec.getSizes()[1];
        return getGridGeometry(0, nx - 1, 0, ny - 1, bbox);
    }

    /**
     * Get small sample from record
     * 
     * @param record
     * @return
     * @throws WcsException
     */
    protected IDataRecord getSample(PluginDataObject record)
            throws WcsException {
        try {
            IDataStore store = getDao().getDataStore((IPersistable) record);
            Request request = Request.buildPointRequest(new Point(0, 0));
            return store.retrieve(record.getDataURI(),
                    DataStoreFactory.DEF_DATASET_NAME, request);
        } catch (Exception e) {
            log.error("Unable to sample data", e);
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * @param projectedRecord
     * @return
     */
    protected ReferencedDataRecord padData(
            ReferencedDataRecord projectedRecord, Envelope targetEnv)
            throws WcsException {
        GridGeometry2D gridGeom = getGridGeometry(projectedRecord.getRecord(),
                projectedRecord.getEnvelope());
        if (projectedRecord.getEnvelope().contains(targetEnv)) {
            // no need to pad
            return projectedRecord;
        } else if (projectedRecord.getEnvelope().intersects(targetEnv)) {
            // there is some padding
            try {
                int[][] dataRange = getContainingGridPoints(
                        gridGeom.getCRSToGrid2D(PixelOrientation.UPPER_LEFT),
                        projectedRecord.getEnvelope());
                int[][] targetRange = getContainingGridPoints(
                        gridGeom.getCRSToGrid2D(PixelOrientation.UPPER_LEFT),
                        targetEnv);

                int[][] padRange = calculatePadRange(dataRange, targetRange);

                int[] dataSize = new int[2];

                dataSize[0] = (int) projectedRecord.getRecord().getSizes()[0];
                dataSize[1] = (int) projectedRecord.getRecord().getSizes()[1];

                IDataRecord paddedDataRecord = padDataInternal(
                        projectedRecord.getRecord(), dataSize, padRange);

                // make an adjusted grid geometry to account for padding
                // shift old grid corners by minimum pad
                GridGeometry2D adjustedGridGeom = getGridGeometry(
                        padRange[0][0], dataSize[0] + padRange[0][0] - 1,
                        padRange[0][1], dataSize[1] + padRange[0][1] - 1,
                        projectedRecord.getEnvelope());

                ReferencedEnvelope finalEnvelope = makeReferencedEnvelope(
                        adjustedGridGeom, paddedDataRecord);

                ReferencedDataRecord record = new ReferencedDataRecord(
                        paddedDataRecord, finalEnvelope);

                return record;

            } catch (TransformException e) {
                log.error("Transform Exception while padding partial record", e);
                throw new WcsException(Code.InternalServerError);
            }

        } else {
            // need 100% nulls
            try {
                int[][] targetRange = getContainingGridPoints(
                        gridGeom.getCRSToGrid2D(), targetEnv);
                int[] targetDims = new int[] {
                        Math.abs(targetRange[0][0])
                                + Math.abs(targetRange[1][0]),
                        Math.abs(targetRange[0][1])
                                + Math.abs(targetRange[1][1]) };
                int targetSize = targetDims[0] * targetDims[1];

                IDataRecord record = projectedRecord.getRecord();

                Object targetArray = makeArrayTypeFromRecord(record, targetSize);

                fillArray(targetArray);

                IDataRecord paddedDataRecord = makeDataRecordOfSameType(record,
                        targetArray, 2, new long[] { targetDims[0],
                                targetDims[1] });

                ReferencedEnvelope finalEnvelope = makeReferencedEnvelope(
                        gridGeom, paddedDataRecord);

                ReferencedDataRecord referencedRecord = new ReferencedDataRecord(
                        paddedDataRecord, finalEnvelope);

                return referencedRecord;

            } catch (TransformException e) {
                log.error("Transform Exception while making all null pads", e);
                throw new WcsException(Code.InternalServerError);
            }
        }
    }

    /**
     * makes a geospatial referenced envelope
     * 
     * @param gridGeom
     * @param paddedDataRecord
     * @return
     */
    protected ReferencedEnvelope makeReferencedEnvelope(
            GridGeometry2D gridGeom, IDataRecord dataRecord)
            throws WcsException, TransformException {
        MathTransform transform = gridGeom
                .getGridToCRS(PixelOrientation.UPPER_LEFT);

        DirectPosition srcUpperLeft = new DirectPosition2D(0, 0);
        DirectPosition srcLowerRight = new DirectPosition2D(
                dataRecord.getSizes()[0], dataRecord.getSizes()[1]);

        try {
            DirectPosition destUpperLeft = transform.transform(srcUpperLeft,
                    null);
            DirectPosition destLowerRight = transform.transform(srcLowerRight,
                    null);

            ReferencedEnvelope rval = new ReferencedEnvelope(
                    destUpperLeft.getOrdinate(0),
                    destLowerRight.getOrdinate(0),
                    destLowerRight.getOrdinate(1),
                    destUpperLeft.getOrdinate(1),
                    gridGeom.getCoordinateReferenceSystem());

            return rval;
        } catch (MismatchedDimensionException e) {
            log.error(
                    "error while transforming grid coordinates to referenced envelope",
                    e);
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * @param record
     * @param dataSize
     * @param padRange
     * @return
     * @throws WcsBadTypeException
     */
    protected IDataRecord padDataInternal(IDataRecord record, int[] dataSize,
            int[][] padRange) throws WcsException {

        int[] newSize = new int[] {
                dataSize[0] + padRange[0][0] + padRange[1][0],
                dataSize[1] + padRange[0][1] + padRange[1][1] };
        int newTotalSize = newSize[0] * newSize[1];

        int dataArraySize = dataSize[0] * dataSize[1];

        Object sourceData = record.getDataObject();

        Object targetArray = makeArrayTypeFromRecord(record, newTotalSize);
        fillArray(targetArray);

        for (int srcIndex = 0; srcIndex < dataArraySize; ++srcIndex) {

            // get row and column index from data
            int srcRow = srcIndex / dataSize[0];
            int srcColumn = srcIndex % dataSize[0];

            // calculate new row and column
            // add min y pad to source row to get destination row
            int destRow = padRange[0][1] + srcRow;
            // add min x pad to source column for destination column
            int destColumn = padRange[0][0] + srcColumn;

            int targetIndex = (destRow * newSize[0]) + destColumn;

            copyValue(sourceData, srcIndex, targetArray, targetIndex);
        }

        IDataRecord rval = makeDataRecordOfSameType(record, targetArray, 2,
                new long[] { newSize[0], newSize[1] });

        return rval;
    }

    /**
     * @param record
     * @param newDataArray
     * @param dims
     * @param sizes
     * @return
     * @throws WcsException
     */
    protected IDataRecord makeDataRecordOfSameType(IDataRecord record,
            Object newDataArray, int dims, long[] sizes) throws WcsException {
        IDataRecord rval = null;

        if (record instanceof ByteDataRecord) {
            rval = new ByteDataRecord(record.getName(), record.getGroup(),
                    (byte[]) newDataArray, dims, sizes);
        } else if (record instanceof FloatDataRecord) {
            rval = new FloatDataRecord(record.getName(), record.getGroup(),
                    (float[]) newDataArray, dims, sizes);
        } else if (record instanceof IntegerDataRecord) {
            rval = new IntegerDataRecord(record.getName(), record.getGroup(),
                    (int[]) newDataArray, dims, sizes);
        } else if (record instanceof LongDataRecord) {
            rval = new LongDataRecord(record.getName(), record.getGroup(),
                    (long[]) newDataArray, dims, sizes);
        } else if (record instanceof ShortDataRecord) {
            rval = new ShortDataRecord(record.getName(), record.getGroup(),
                    (short[]) newDataArray, dims, sizes);
        } else if (record instanceof StringDataRecord) {
            rval = new StringDataRecord(record.getName(), record.getGroup(),
                    (String[]) newDataArray, dims, sizes);
        } else {
            log.error("Unknown IDataRecord type. type: "
                    + record.getClass().toString(), new Exception());
            throw new WcsException(Code.InternalServerError);
        }

        return rval;
    }

    /**
     * @param record
     * @param size
     * @return
     * @throws WcsException
     */
    protected Object makeArrayTypeFromRecord(IDataRecord record, int size)
            throws WcsException {
        Object targetArray = null;

        if (record instanceof ByteDataRecord) {
            targetArray = new byte[size];
            byte fill = new ByteDataReprojector().getFill();
            Arrays.fill((byte[]) targetArray, fill);
        } else if (record instanceof FloatDataRecord) {
            targetArray = new float[size];
            float fill = new FloatDataReprojector().getFill();
            Arrays.fill((float[]) targetArray, fill);
        } else if (record instanceof IntegerDataRecord) {
            targetArray = new int[size];
            int fill = new IntDataReprojector().getFill();
            Arrays.fill((int[]) targetArray, fill);
        } else if (record instanceof LongDataRecord) {
            targetArray = new long[size];
            long fill = 0l;
            Arrays.fill((long[]) targetArray, fill);
        } else if (record instanceof ShortDataRecord) {
            targetArray = new short[size];
            short fill = new ShortDataReprojector().getFill();
            Arrays.fill((short[]) targetArray, fill);
        } else if (record instanceof StringDataRecord) {
            targetArray = new String[size];
        } else {
            log.error("Unknown IDataRecord type when making array. type: "
                    + record.getClass().toString(), new Exception());
            throw new WcsException(Code.InternalServerError);
        }

        return targetArray;
    }

    /**
     * Get fill value for record
     * 
     * @param record
     * @return
     * @throws WcsException
     */
    protected Number getFillValue(T record) throws WcsException {
        IDataRecord sample = getSample(record);
        if (sample instanceof ByteDataRecord) {
            return new ByteDataReprojector().getFill();
        } else if (sample instanceof FloatDataRecord) {
            return new FloatDataReprojector().getFill();
        } else if (sample instanceof IntegerDataRecord) {
            return new IntDataReprojector().getFill();
        } else if (sample instanceof LongDataRecord) {
            return 0l;
        } else if (sample instanceof ShortDataRecord) {
            return new ShortDataReprojector().getFill();
        } else {
            log.error(
                    "Unknown IDataRecord type when getting fill value. type: "
                            + record.getClass().toString(), new Exception());
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * @param sourceData
     * @param srcIndex
     * @param targetArray
     * @param targetIndex
     */
    protected void copyValue(Object sourceData, int srcIndex,
            Object targetArray, int targetIndex) throws WcsException {

        if (sourceData instanceof byte[] && targetArray instanceof byte[]) {
            ((byte[]) targetArray)[targetIndex] = ((byte[]) sourceData)[srcIndex];
        } else if (sourceData instanceof float[]
                && targetArray instanceof float[]) {
            ((float[]) targetArray)[targetIndex] = ((float[]) sourceData)[srcIndex];
        } else if (sourceData instanceof short[]
                && targetArray instanceof short[]) {
            ((short[]) targetArray)[targetIndex] = ((short[]) sourceData)[srcIndex];
        } else if (sourceData instanceof int[] && targetArray instanceof int[]) {
            ((int[]) targetArray)[targetIndex] = ((int[]) sourceData)[srcIndex];
        } else if (sourceData instanceof long[]
                && targetArray instanceof long[]) {
            ((long[]) targetArray)[targetIndex] = ((long[]) sourceData)[srcIndex];
        } else if (sourceData instanceof String[]
                && targetArray instanceof String[]) {
            ((String[]) targetArray)[targetIndex] = ((String[]) sourceData)[srcIndex];
        } else {
            log.error("bad type when making record, "
                    + sourceData.getClass().toString() + " and "
                    + targetArray.getClass().toString(), new Exception(
                    "bad type when making record"));
            throw new WcsException(Code.InternalServerError);
        }

    }

    /**
     * @param targetArray
     * @param nullPadValue
     */
    protected void fillArray(Object targetArray) throws WcsException {

        if (targetArray instanceof byte[]) {
            byte fill = new ByteDataReprojector().getFill();
            Arrays.fill((byte[]) targetArray, fill);
        } else if (targetArray instanceof float[]) {
            float fill = new FloatDataReprojector().getFill();
            Arrays.fill((float[]) targetArray, fill);
        } else if (targetArray instanceof short[]) {
            short fill = new ShortDataReprojector().getFill();
            Arrays.fill((short[]) targetArray, fill);
        } else if (targetArray instanceof int[]) {
            int fill = new IntDataReprojector().getFill();
            Arrays.fill((int[]) targetArray, fill);
        } else if (targetArray instanceof long[]) {
            long fill = 0l;
            Arrays.fill((long[]) targetArray, fill);
        } else {
            log.error("Unknown array type when trying to fill array. type: "
                    + targetArray.getClass().toString(), new Exception());
            throw new WcsException(Code.InternalServerError);
        }

    }

    /**
     * @param dataRange
     * @param targetRange
     * @return
     */
    protected int[][] calculatePadRange(int[][] dataRange, int[][] targetRange) {
        // for minimum we expect dataRange to be less than or equal to
        // targetRange, if the difference is negative we need to pad
        int minxDiff = targetRange[0][0] - dataRange[0][0];
        int minyDiff = targetRange[0][1] - dataRange[0][1];
        // for maximum we expect dataRange to be greater than or equal to
        // targetRange, if the difference is positive we need to pad
        // swap min/max order because grid and crs count y differently
        int maxxDiff = targetRange[1][0] - dataRange[1][0];
        int maxyDiff = targetRange[1][1] - dataRange[1][1];

        // only pad, do not trim. so we need to check if the dataRange was
        // bigger
        if (minxDiff > 0) {
            minxDiff = 0;
        }
        if (minyDiff > 0) {
            minyDiff = 0;
        }

        if (maxxDiff < 0) {
            maxxDiff = 0;
        }
        if (maxyDiff < 0) {
            maxyDiff = 0;
        }

        return new int[][] { { Math.abs(minxDiff), Math.abs(minyDiff) },
                { Math.abs(maxxDiff), Math.abs(maxyDiff) } };
    }

    /**
     * @param crsToGrid
     * @param env
     * @return
     * @throws TransformException
     */
    protected int[][] getContainingGridPoints(MathTransform2D crsToGrid,
            Envelope env) throws TransformException {
        DirectPosition lower = new DirectPosition2D(env.getMinX(),
                env.getMinY());
        DirectPosition upper = new DirectPosition2D(env.getMaxX(),
                env.getMaxY());
        DirectPosition lowerGrid = crsToGrid.transform(lower, null);
        DirectPosition upperGrid = crsToGrid.transform(upper, null);

        // make sure to "grow" in the proper direction

        // x axis "direction" is the same in geo and grid
        double minx = lowerGrid.getOrdinate(0);
        double maxx = upperGrid.getOrdinate(0);

        // y axis "direction" is swapped between geo and grid
        double maxy = lowerGrid.getOrdinate(1);
        double miny = upperGrid.getOrdinate(1);

        minx = Math.round(minx);
        maxx = Math.round(maxx);

        miny = Math.round(miny);
        maxy = Math.round(maxy);

        return new int[][] { { (int) minx, (int) miny },
                { (int) maxx, (int) maxy } };
    }

    /**
     * @param ref
     * @return
     */
    protected GridGeometry2D getGridGeometry(ReferencedDataRecord ref) {
        return getGridGeometry(ref.getRecord(), ref.getEnvelope());
    }

    /**
     * @param record
     * @param env
     * @return
     */
    protected GridGeometry2D getGridGeometry(IDataRecord record,
            ReferencedEnvelope env) {
        return getGridGeometry(0, (int) record.getSizes()[0] - 1, 0,
                (int) record.getSizes()[1] - 1, env);
    }

    /**
     * @param gridXmin
     * @param gridXmax
     * @param gridYmin
     * @param gridYmax
     * @param env
     * @return
     */
    protected GridGeometry2D getGridGeometry(int gridXmin, int gridXmax,
            int gridYmin, int gridYmax, ReferencedEnvelope env) {
        GeneralEnvelope genEnvelope = convertEnvelopeToGeneralEnvelope(env);
        genEnvelope.setCoordinateReferenceSystem(env
                .getCoordinateReferenceSystem());
        GridGeometry2D gridGeom = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { gridXmin, gridYmin }, new int[] { gridXmax,
                        gridYmax }, true), genEnvelope);

        return gridGeom;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.reg.WcsSource#getKey()
     */
    @Override
    public String getKey() {
        return props.getPluginName();
    }

    /**
     * @param env
     * @return
     */
    protected GeneralEnvelope convertEnvelopeToGeneralEnvelope(Envelope env) {
        GeneralEnvelope rval = new GeneralEnvelope(2);
        rval.setRange(0, env.getMinX(), env.getMaxX());
        rval.setRange(1, env.getMinY(), env.getMaxY());
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.reg.WcsSource#hasId(java.lang.String)
     */
    @Override
    public boolean hasCoverage(String id) throws WcsException {
        try {
            return transformer.find(URNLookup.urnToLocal(id)) != null;
        } catch (OgcException e) {
            log.error("problem finding coverage", e);
            throw new WcsException(e);
        }
    }

}
