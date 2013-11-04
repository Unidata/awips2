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
package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;

import javax.measure.unit.Unit;
import javax.xml.bind.DatatypeConverter;

import org.apache.commons.collections.map.LRUMap;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.hibernate.Criteria;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.level.LevelDimUtil;
import com.raytheon.uf.edex.ogc.common.spatial.AltUtil;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;
import com.raytheon.uf.edex.plugin.unitconverter.UnitLookup;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.raytheon.uf.edex.wcs.reg.CoverageTransform;
import com.raytheon.uf.edex.wcs.reg.DefaultWcsSource;
import com.raytheon.uf.edex.wcs.reg.RangeAxis;
import com.raytheon.uf.edex.wcs.reg.RangeField;


/**
 * TODO Add Description
 * 
 * @author jelkins
 * @version 1.0
 */
public class GribWcsSource extends
        DefaultWcsSource<GribDimension, GribLayer, GridRecord> {

    private CoverageTransform<GribDimension, GribLayer> _cTransform;

    private static final String PARAM_KEY = GribDimension.PARAM_DIM;

    /**
     * @param props
     * @param layerTable
     */
    public GribWcsSource(PluginProperties props,
            LayerTransformer<GribDimension, GribLayer> transformer,
            GribLayerCollector collector) {
		super(props, transformer);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getCoverageTransform()
	 */
	@Override
    public CoverageTransform<GribDimension, GribLayer> getCoverageTransform() {
		if (_cTransform == null) {
            _cTransform = new CoverageTransform<GribDimension, GribLayer>(
                    transformer.getKey()) {
				@Override
                protected List<RangeField> getRangeFields(GribLayer layer) {
                    List<RangeField> rval = new ArrayList<RangeField>();
                    SimpleDimension params = layer
                            .getDimension(GribDimension.PARAM_DIM);
                    for (String name : params.getValues()) {
                        rval.add(new RangeField(name, null));
					}
                    SimpleDimension refDim = layer
                            .getDimension(GribDimension.REFTIME_DIM);
                    rval.add(convert(refDim));
					return rval;
				}

                protected RangeField convert(SimpleDimension dim) {
                    String name = dim.getName();
                    Set<String> fromVals = dim.getValues();
                    String units = dim.getUnits();
                    RangeField rf = new RangeField(name, null);
                    List<RangeAxis> axis = new ArrayList<RangeAxis>(1);
                    if (units == null) {
                        // use dim name as axis label
                        units = name;
                    }
                    axis.add(new RangeAxis(units, fromVals));
                    rf.setAxis(axis);
                    return rf;
                }

                @SuppressWarnings("unchecked")
                private final Map<String, Unit<?>> cache = Collections
                        .synchronizedMap(new LRUMap(3));

                private Unit<?> getUnit(String str) {
                    Unit<?> rval = cache.get(str);
                    if (rval == null) {
                        rval = Unit.valueOf(str);
                        cache.put(str, rval);
                    }
                    return rval;
                }

                @Override
                protected VerticalCoordinate getVertical(GribLayer layer)
                        throws WcsException {
                    if (!layer.isVertical()) {
                        return null;
                    }
                    List<GribDimension> levels = LayerTransformer
                            .getDimsByPrefix(layer,
                                    LevelDimUtil.LEVEL_DIM_PREFIX);
                    if (levels.isEmpty()) {
                        log.error("attempted to get vertical component of empty layer");
                        throw new WcsException(Code.InternalServerError);
                    }
                    // TODO get sample that represents majority
                    SimpleDimension sample = levels.get(0);
                    Unit<?> targetUnits = getUnit(sample.getUnits());
                    String levelName = sample.getName().substring(
                            LevelDimUtil.LEVEL_DIM_PREFIX.length());
                    Reference targetRef = VerticalLevelLookup
                            .getReference(levelName);
                    double min = Double.POSITIVE_INFINITY;
                    double max = Double.NEGATIVE_INFINITY;
                    for (SimpleDimension l : levels) {
                        String unitStr = l.getUnits();
                        Unit<?> unit = cache.get(unitStr);
                        if (unit == null) {
                            unit = Unit.valueOf(unitStr);
                            cache.put(unitStr, unit);
                        }
                        levelName = sample.getName().substring(
                                LevelDimUtil.LEVEL_DIM_PREFIX.length());
                        Reference ref = VerticalLevelLookup
                                .getReference(levelName);
                        for ( String val : l.getValues()){
                            VerticalCoordinate vert = parseLevelValue(val,
                                    unit, ref);
                            VerticalCoordinate convert = AltUtil.convert(
                                    targetUnits, targetRef, vert);
                            min = Math.min(min, convert.getMin());
                            max = Math.max(max, convert.getMax());
                        }
                    }
                    return new VerticalCoordinate(min, max, targetUnits,
                            targetRef);
                }

                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * com.raytheon.uf.edex.wcs.reg.CoverageTransform#getBboxes(
                 * com.raytheon.uf.edex.ogc.common.db.SimpleLayer)
                 */
                @Override
                protected List<Composite3DBoundingBox> getBboxes(GribLayer layer)
                        throws WcsException {
                    List<Composite3DBoundingBox> rval = new ArrayList<Composite3DBoundingBox>(
                            2);
                    VerticalCoordinate vert = getVertical(layer);
                    ReferencedEnvelope crs84Horiz = getHorizontal(layer);
                    rval.add(new Composite3DBoundingBox(crs84Horiz, vert));
                    try {
                        GribLayer griblayer = (GribLayer) layer;
                        CoordinateReferenceSystem crs = CRS.parseWKT(griblayer
                                .getCrsWkt());
                        ReferencedEnvelope nativeHoriz = new ReferencedEnvelope(
                                griblayer.getNativeMinX(),
                                griblayer.getNativeMaxX(),
                                griblayer.getNativeMinY(),
                                griblayer.getNativeMaxY(), crs);
                        String native2DCrsUrn = GribNativeCrsAuthority
                                .createURN(griblayer.getCoverageName(),
                                        (ProjectedCRS) crs);
                        rval.add(new Composite3DBoundingBox(nativeHoriz,
                                native2DCrsUrn, vert));
                    } catch (FactoryException e) {
                        log.error("Unable to determine native BBOX", e);
                    }
                    return rval;
                }

			};
		}
		return _cTransform;
	}

    private void addToMap(Map<String, Set<String>> map, String key, String item) {
        Set<String> list = map.get(key);
        if (list == null) {
            list = new TreeSet<String>();
            map.put(key, list);
        }
        list.add(item);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#parseFields(java.util.List)
     */
    @Override
    protected Map<String, Set<String>> parseFields(List<RangeField> fields)
            throws WcsException {
        if (fields == null) {
            return new HashMap<String, Set<String>>(0);
        }
        Map<String, Set<String>> rval = new HashMap<String, Set<String>>(
                fields.size());
        for (RangeField rf : fields) {
            String key = rf.getIdentifier().toLowerCase();
            if (rf.getAxis() == null || rf.getAxis().isEmpty()) {
				addToMap(rval, PARAM_KEY, rf.getIdentifier());
                continue;
            }
            for (RangeAxis ra : rf.getAxis()) {
                Set<String> keys = ra.getKeys();
                if (keys != null && !keys.isEmpty()) {
                    rval.put(key, keys);
                    break;
                }
            }
        }
        return rval;
    }

    /**
     * @param val
     * @return
     * @throws WcsException
     */
    private VerticalCoordinate parseLevelValue(String val, Unit<?> unit,
            Reference ref) throws WcsException {
        Matcher m = LevelDimUtil.levelPattern.matcher(val);
        if (m.matches()) {
            double val1 = Double.parseDouble(m.group(1));
            if (m.group(3) == null) {
                return new VerticalCoordinate(val1, unit, ref);
            } else {
                double val2 = Double.parseDouble(m.group(3));
                return new VerticalCoordinate(val1, val2, unit, ref);
            }
        }
        throw new WcsException(Code.InvalidParameterValue,
                "Invalid level field value: " + val);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getScalarField()
     */
    @Override
    protected String getScalarField() {
        return GribRecordFinder.PARAM_ABBV;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getFilterClause(java.lang
     * .String, com.raytheon.uf.common.time.DataTime, java.util.Map)
     */
    @Override
    protected Conjunction getFilterClause(String id, DataTime time,
            Map<String, Set<String>> fields) throws WcsException {
        Conjunction and = Restrictions.conjunction();
        try {
            and.add(GribRecordFinder.parseWcsId(id));
        } catch (OgcException e) {
            throw new WcsException(e);
        }
        for (Entry<String, Set<String>> e : fields.entrySet()) {
            if (e.getKey().equalsIgnoreCase(GribDimension.REFTIME_DIM)) {
                Set<String> values = e.getValue();
                if (values == null || values.isEmpty()) {
                    continue;
                }
                if (values.size() == 1) {
                    Calendar cal = DatatypeConverter.parseDateTime(values
                            .iterator().next());
                    and.add(Restrictions.eq(GribRecordFinder.REF_TIME,
                            cal.getTime()));
                    continue;
                }
                Disjunction or = Restrictions.disjunction();
                Iterator<String> iter = values.iterator();
                while (iter.hasNext()) {
                    String val = iter.next();
                    Calendar cal = DatatypeConverter.parseDateTime(val);
                    or.add(Restrictions.eq(GribRecordFinder.REF_TIME,
                            cal.getTime()));
                }
                and.add(or);
            }
        }
        addIfNotNull(and, parseTime(time));
        and.add(Restrictions.not(Restrictions.like(GribRecordFinder.PARAM_ABBV,
                "static%")));
        return and;
    }

    /**
     * Add criteria to conjunction if not null
     * 
     * @param and
     * @param crit
     */
    private void addIfNotNull(Conjunction and, Criterion crit) {
        if (crit != null) {
            and.add(crit);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getScalarValue(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected String getScalarValue(GridRecord record) throws WcsException {
		return GribRecordFinder.dbToOgcParameter(new GridFieldAdapter()
				.getCoverageField(record));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getAltitude(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected VerticalCoordinate getAltitude(GridRecord record)
            throws WcsException {
        return new GridVerticalEnabler().getVerticalCoordinate(record);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getScalarUnit(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected Unit<?> getScalarUnit(GridRecord record) {
        return record.getInfo().getParameter().getUnit();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#isUpPositive(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected boolean isUpPositive(GridRecord record) {
        MasterLevel masterLevel = record.getInfo().getLevel()
                .getMasterLevel();
        return !"DEC".equalsIgnoreCase(masterLevel.getType());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#modCriteria(org.hibernate
     * .Criteria)
     */
    @Override
    protected Criteria modCriteria(Criteria criteria) {
        return GribRecordFinder.modCriteria(criteria);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getScalarKey()
     */
    @Override
    protected String getScalarKey() {
        return PARAM_KEY;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getUcarUnit(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected ucar.units.Unit getUcarUnit(GridRecord record) {
        Unit<?> parameterUnitObject = record.getInfo().getParameter().getUnit();
        return UnitLookup.getInstance().getUcarFromJsr(parameterUnitObject);
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wcs.reg.DefaultWcsSource#getScalarCrit(java.lang
	 * .String)
	 */
	@Override
	protected Criterion getScalarCrit(String value) {
		return super.getScalarCrit(GribRecordFinder.ogcToDbParameter(value));
	}

}
