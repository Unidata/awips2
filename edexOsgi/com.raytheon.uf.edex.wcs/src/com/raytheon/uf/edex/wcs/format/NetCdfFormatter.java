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
package com.raytheon.uf.edex.wcs.format;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.parameter.Parameter;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.transform.ConcatenatedTransform;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.parameter.GeneralParameterValue;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.cs.CartesianCS;
import org.opengis.referencing.cs.CoordinateSystemAxis;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.Projection;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.nc4.NcDimension;
import com.raytheon.uf.common.nc4.NcDimension.DoubleDimension;
import com.raytheon.uf.common.nc4.NcVariable;
import com.raytheon.uf.common.nc4.NcVariable.ByteVariable;
import com.raytheon.uf.common.nc4.Netcdf;
import com.raytheon.uf.common.nc4.NetcdfException;
import com.raytheon.uf.common.nc4.cf.CfConstants;
import com.raytheon.uf.common.nc4.cf.CfDimensions;
import com.raytheon.uf.common.nc4.cf.CfGridMapper;
import com.raytheon.uf.common.nc4.cf.CfHorizontalDims;
import com.raytheon.uf.common.nc4.cf.CfNetcdf;
import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.BasicFileStore;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.raytheon.uf.edex.wcs.reg.Coverage;
import com.raytheon.uf.edex.wcs.reg.CoverageDimensions;
import com.raytheon.uf.edex.wcs.reg.CoverageField;
import com.raytheon.uf.edex.wcs.reg.CoverageTAxis;
import com.raytheon.uf.edex.wcs.reg.CoverageXYAxis;
import com.raytheon.uf.edex.wcs.reg.CoverageZAxis;
import com.raytheon.uf.edex.wcs.reg.TemporalCube;
import com.raytheon.uf.edex.wcs.reg.VerticalSlice;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class NetCdfFormatter implements IWcsDataFormatter {

    public static final String CONTENT_TYPE = "application/netcdf4";

    private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    private final BasicFileStore store;

    private static final AtomicLong STORAGE_ID = new AtomicLong();

    public NetCdfFormatter(BasicFileStore store) {
        this.store = store;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.format.WcsDataFormatter#getIdentifier()
     */
    @Override
    public String getIdentifier() {
        return CONTENT_TYPE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.format.WcsDataFormatter#format(com.raytheon.
     * uf.common.datastorage.records.IDataRecord, java.io.OutputStream, boolean)
     */
    @Override
    public InputStream format(Coverage coverage) throws Exception {
        final File file = store(coverage);
        // if the constructor throws, the file will be leaked
        return new FileInputStream(file) {
            @Override
            public void close() throws IOException {
                try {
                    super.close();
                } finally {
                    if (file != null) {
                        file.delete();
                    }
                }
            }
        };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.format.WcsDataFormatter#store(com.raytheon.uf
     * .edex.wcs.reg.Coverage)
     */
    @Override
    public File store(Coverage coverage) throws Exception {
        String id = createStoreId(coverage);
        File file = store.reserveFile(id);
        CfNetcdf ncfile = new CfNetcdf(file.getAbsolutePath(),
                Netcdf.NETCDF4_CLASSIC_MODE);
        try {
            ncfile.putStringAttribute(CfConstants.COVERAGE_ID_ATTR,
                    coverage.getName());
            List<CoverageField> fields = coverage.getFields();
            DimMapping dmap = defineDims(ncfile, fields);
            List<NcVariable> vars = new ArrayList<NcVariable>(fields.size());
            for (CoverageField field : fields) {
                vars.add(define(ncfile, dmap.fieldMap.get(field.getName()),
                        field));
            }
            ncfile.endFileDefinition();
            for (DimWriter w : dmap.writers) {
                w.write();
            }
            Iterator<NcVariable> iter = vars.iterator();
            for (CoverageField field : fields) {
                NcVariable var = iter.next();
                write(field, var);
            }

        } finally {
            ncfile.close();
        }
        return file;
    }

    /**
     * Write 4D cube in field to variable
     * 
     * @param field
     * @param var
     * @throws Exception
     */
    private void write(CoverageField field, NcVariable var) throws Exception {
        List<TemporalCube> timeCube = field.getTimeCube();
        TemporalCube firstCube = timeCube.get(0);
        VerticalSlice firstSlice = firstCube.getSlices().get(0);
        ReferencedDataRecord firstRecord = firstSlice.getRecord().get(true);
        NcWriter<? extends NcVariable> writer = NcWriter.create(firstRecord
                .getRecord());

        Iterator<TemporalCube> cubeIter = timeCube.iterator();
        for (int i = 0; cubeIter.hasNext(); ++i) {
            TemporalCube cube = cubeIter.next();
            Iterator<VerticalSlice> sliceIter = cube.getSlices().iterator();
            for (int j = 0; sliceIter.hasNext(); ++j) {
                VerticalSlice slice = sliceIter.next();
                writer.write(var, new int[] { i, j, 0, 0 }, slice.getRecord()
                        .get(false).getRecord());
            }
        }
    }

    /**
     * Define variable for field
     * 
     * @param ncfile
     * @param dims
     * @param field
     * @return
     * @throws Exception
     */
    private NcVariable define(CfNetcdf ncfile,
            CfDimensions<DoubleDimension> dims, CoverageField field)
            throws Exception {
        List<TemporalCube> timeCube = field.getTimeCube();
        TemporalCube firstCube = timeCube.get(0);
        VerticalSlice firstSlice = firstCube.getSlices().get(0);
        ReferencedDataRecord firstRecord = firstSlice.getRecord().get(true);
        NcWriter<? extends NcVariable> writer = NcWriter.create(firstRecord
                .getRecord());
        NcVariable variable = ncfile.defineVar(field.getName(), dims.toArray(),
                writer.getVarClass());
        variable.putStringAttribute(CfConstants.STANDARD_NAME_ATTR,
                field.getStandardName());
        variable.putStringAttribute(CfConstants.LONG_NAME_ATTR,
                field.getStandardName());
        variable.putStringAttribute(CfConstants.UNITS_ATTR, field.getUnits());
        variable.putNumberAttribute(CfConstants.MISSING_VAL_ATTR,
                new Number[] { field.getPaddingValue() });
        CfHorizontalDims<DoubleDimension> xyDims = dims.getXyDims();
        if (xyDims.isMapped()) {
            variable.putStringAttribute(CfConstants.GRID_MAP_NAME_ATTR,
                    xyDims.getGridMapping());
            variable.putStringAttribute(CfConstants.COORDS_ATTR,
                    xyDims.getCoords());
        }
        return variable;
    }

    /**
     * Interface to postpone writing to dimensions until after definition phase
     */
    private static interface DimWriter {
        public void write() throws NetcdfException;
    }

    /**
     * Wrapper for multi-part return
     */
    private static class DimMapping {
        public final Map<String, CfDimensions<DoubleDimension>> fieldMap;

        public final List<DimWriter> writers;

        public DimMapping(Map<String, CfDimensions<DoubleDimension>> fieldMap,
                List<DimWriter> writers) {
            this.fieldMap = fieldMap;
            this.writers = writers;
        }
    }

    /**
     * Define dimensions for fields.
     * 
     * @param ncfile
     * @param fields
     * @return
     * @throws Exception
     */
    private DimMapping defineDims(CfNetcdf ncfile, List<CoverageField> fields)
            throws Exception {
        Map<CoverageXYAxis, CfHorizontalDims<DoubleDimension>> horizMap = new HashMap<CoverageXYAxis, CfHorizontalDims<DoubleDimension>>();
        Map<CoverageZAxis, DoubleDimension> vertMap = new HashMap<CoverageZAxis, DoubleDimension>();
        Map<CoverageTAxis, DoubleDimension> timeMap = new HashMap<CoverageTAxis, DoubleDimension>();
        Map<String, CfDimensions<DoubleDimension>> fieldMap = new HashMap<String, CfDimensions<DoubleDimension>>();
        List<DimWriter> writers = new ArrayList<NetCdfFormatter.DimWriter>();

        for (CoverageField field : fields) {
            CoverageDimensions dims = field.getDimensions();
            CoverageXYAxis xyAxis = dims.getXyAxis();
            CfHorizontalDims<DoubleDimension> horiz = horizMap.get(xyAxis);
            if (horiz == null) {
                horiz = defineXY(ncfile, xyAxis, writers, horizMap.size());
                horizMap.put(xyAxis, horiz);
            }
            CoverageZAxis zAxis = dims.getZAxis();
            DoubleDimension vert = vertMap.get(zAxis);
            if (vert == null) {
                vert = defineZ(ncfile, zAxis, writers, vertMap.size());
                vertMap.put(zAxis, vert);
            }
            CoverageTAxis tAxis = dims.getTAxis();
            DoubleDimension time = timeMap.get(tAxis);
            if (time == null) {
                time = defineT(ncfile, tAxis, writers, timeMap.size());
                timeMap.put(tAxis, time);
            }
            fieldMap.put(field.getName(),
                    new CfDimensions<NcDimension.DoubleDimension>(horiz, vert,
                            time));
        }
        return new DimMapping(fieldMap, writers);
    }

    /**
     * Define horizontal dimensions
     * 
     * @param ncfile
     * @param xyAxis
     * @param writers
     * @param count
     * @return
     * @throws Exception
     */
    private CfHorizontalDims<DoubleDimension> defineXY(CfNetcdf ncfile,
            CoverageXYAxis xyAxis, List<DimWriter> writers, int count)
            throws Exception {
        double[][] lonLatAxis = getLonLatAxis(xyAxis.getGridGeometry());
        final double[] lonValue = lonLatAxis[0];
        final double[] latValue = lonLatAxis[1];
        final DoubleDimension lon = ncfile.defineXDim("lon" + count,
                "longitude", lonValue.length, CfConstants.LON_UNITS,
                DoubleDimension.class);
        final DoubleDimension lat = ncfile.defineYDim("lat" + count,
                "latitude", latValue.length, CfConstants.LAT_UNITS,
                DoubleDimension.class);
        writers.add(new DimWriter() {
            @Override
            public void write() throws NetcdfException {
                lon.putDim(lonValue);
                lat.putDim(latValue);
            }
        });

        ReferencedEnvelope env = xyAxis.getEnvelope();
        CoordinateReferenceSystem crs = env.getCoordinateReferenceSystem();
        if (crs instanceof GeographicCRS) {
            return new CfHorizontalDims<NcDimension.DoubleDimension>(lon, lat);
        } else if (crs instanceof ProjectedCRS) {
            return defineMappedXY(ncfile, xyAxis, writers, count, lon, lat);
        } else {
            log.error("Unsupport CRS object type: " + crs.getClass(),
                    new Exception());
            throw new WcsException(Code.InternalServerError);
        }
    }

    /**
     * Define non geographic horizontal dimensions
     * 
     * @param ncfile
     * @param xyAxis
     * @param writers
     * @param count
     * @param lon
     * @param lat
     * @return
     * @throws NetcdfException
     * @throws TransformException
     * @throws FactoryException
     * @throws MismatchedDimensionException
     * @throws InvalidGridGeometryException
     */
    private CfHorizontalDims<NcDimension.DoubleDimension> defineMappedXY(
            CfNetcdf ncfile, CoverageXYAxis xyAxis, List<DimWriter> writers,
            int count, DoubleDimension lon, DoubleDimension lat)
            throws Exception {
        ReferencedEnvelope env = xyAxis.getEnvelope();
        ProjectedCRS crs = (ProjectedCRS) env.getCoordinateReferenceSystem();
        CartesianCS cs = crs.getCoordinateSystem();
        CoordinateSystemAxis xAxis = cs.getAxis(0);
        CoordinateSystemAxis yAxis = cs.getAxis(1);
        GridGeometry2D geom = xyAxis.getGridGeometry();
        GridEnvelope2D gridEnv = geom.getGridRange2D();

        final DoubleDimension x = ncfile.defineXDim("x" + count,
                CfConstants.X_STD_NAME, gridEnv.width, xAxis.getUnit()
                        .toString(), DoubleDimension.class);
        final DoubleDimension y = ncfile.defineYDim("y" + count,
                CfConstants.Y_STD_NAME, gridEnv.height, yAxis.getUnit()
                        .toString(), DoubleDimension.class);

        double[][] xyVals = getXYAxis(geom, crs);
        final double[] xVals = xyVals[0];
        final double[] yVals = xyVals[1];

        writers.add(new DimWriter() {
            @Override
            public void write() throws NetcdfException {
                x.putDim(xVals);
                y.putDim(yVals);
            }
        });

        Projection conv = crs.getConversionFromBase();
        ParameterValueGroup params = conv.getParameterValues();
        String projName = params.getDescriptor().getName().getCode();

        Map<String, List<NumberedValue>> paramMap = getParamMap(params);
        NcVariable projVar = defineProjVar(ncfile, projName, paramMap, count);
        String coords = StringUtils.join(
                new String[] { lat.getName(), lon.getName() }, " ");
        return new CfHorizontalDims<NcDimension.DoubleDimension>(x, y, coords,
                projVar.getName());
    }

    protected static class NumberedValue implements Comparable<NumberedValue> {
        public int number;

        public Object value;

        public NumberedValue(int number, Object value) {
            this.number = number;
            this.value = value;
        }

        @Override
        public int compareTo(NumberedValue o) {
            return number - o.number;
        }

    }

    /**
     * Map parameter base names to values
     * 
     * @param params
     * @return
     */
    protected Map<String, List<NumberedValue>> getParamMap(
            ParameterValueGroup params) {
        final Pattern PARAM_PATTERN = Pattern.compile("^(.*)(_([0-9]+))$");
        Map<String, List<NumberedValue>> paramMap = new HashMap<String, List<NumberedValue>>();
        for (GeneralParameterValue v : params.values()) {
            Parameter<?> p = (Parameter<?>) v;
            String paramName = p.getDescriptor().getName().getCode().trim();
            Matcher m = PARAM_PATTERN.matcher(paramName);
            int number = 0;
            if (m.matches()) {
                paramName = m.group(1);
                number = Integer.parseInt(m.group(3));
            }
            paramName = CfGridMapper.getMappingAttributeName(paramName);
            List<NumberedValue> values = paramMap.get(paramName);
            if (values == null) {
                values = new ArrayList<NumberedValue>(2);
                paramMap.put(paramName, values);
            }

            values.add(new NumberedValue(number, p.getValue()));
        }
        return paramMap;
    }

    /**
     * Define grid projection mapping variable
     * 
     * @param ncfile
     * @param projName
     * @param paramMap
     * @param count
     * @return
     * @throws NetcdfException
     */
    private NcVariable defineProjVar(CfNetcdf ncfile, String projName,
            Map<String, List<NumberedValue>> paramMap, int count)
            throws NetcdfException {
        ByteVariable projVar = ncfile.defineVar(projName + count,
                new NcDimension[0], ByteVariable.class);
        projVar.putStringAttribute(CfConstants.GRID_MAP_NAME_ATTR, projName);
        for (Entry<String, List<NumberedValue>> e : paramMap.entrySet()) {
            List<NumberedValue> list = e.getValue();
            Collections.sort(list);
            if (list.get(0).value instanceof Number) {
                Number[] vals = new Number[list.size()];
                Iterator<NumberedValue> iter = list.iterator();
                for (int i = 0; iter.hasNext(); ++i) {
                    vals[i] = (Number) iter.next().value;
                }
                projVar.putNumberAttribute(e.getKey(), vals);
            } else {
                String[] vals = new String[list.size()];
                Iterator<NumberedValue> iter = list.iterator();
                for (int i = 0; iter.hasNext(); ++i) {
                    vals[i] = iter.next().value.toString();
                }
                projVar.putStringsAttribute(e.getKey(), vals);
            }
        }
        return projVar;
    }

    /**
     * Define vertical dimension
     * 
     * @param ncfile
     * @param zAxis
     * @param writers
     * @param count
     * @return
     * @throws NetcdfException
     */
    private DoubleDimension defineZ(CfNetcdf ncfile, final CoverageZAxis zAxis,
            List<DimWriter> writers, int count) throws NetcdfException {
        final DoubleDimension z = ncfile.defineZDim("level" + count,
                "height level", zAxis.getValue().length, zAxis.getUnits(),
                zAxis.isUpIsPositive(), DoubleDimension.class);
        writers.add(new DimWriter() {
            @Override
            public void write() throws NetcdfException {
                z.putDim(zAxis.getValue());
            }
        });
        return z;
    }

    /**
     * Define temporal dimension
     * 
     * @param ncfile
     * @param tAxis
     * @param writers
     * @param count
     * @return
     * @throws NetcdfException
     */
    private DoubleDimension defineT(CfNetcdf ncfile, CoverageTAxis tAxis,
            List<DimWriter> writers, int count) throws NetcdfException {
        final DoubleDimension t = ncfile.defineTimeDim("time" + count, "time",
                tAxis.getTimes().length, CfConstants.UNIX_TIME_UNITS,
                DoubleDimension.class);
        final double[] values = getTimes(tAxis.getTimes());
        writers.add(new DimWriter() {
            @Override
            public void write() throws NetcdfException {
                t.putDim(values);
            }
        });
        return t;
    }

    /**
     * Convert times to UNIX timestamps with milliseconds as decimal
     * 
     * @param times
     * @return
     */
    private double[] getTimes(Date[] times) {
        double[] rval = new double[times.length];
        for (int i = 0; i < rval.length; ++i) {
            rval[i] = times[i].getTime() / 1000;
        }
        return rval;
    }

    /**
     * Convert geometry to CRS84.
     * 
     * @param geom
     * @return array of size 2. Index 0 contains longitude, index 1 contains
     *         latitude. Not guaranteed to be the same length.
     * @throws InvalidGridGeometryException
     * @throws FactoryException
     * @throws MismatchedDimensionException
     * @throws TransformException
     */
    private double[][] getLonLatAxis(GridGeometry2D geom)
            throws InvalidGridGeometryException, FactoryException,
            MismatchedDimensionException, TransformException {
        return getXYAxis(geom, MapUtil.LATLON_PROJECTION);
    }

    /**
     * Convert geometry to Target CRS.
     * 
     * @param geom
     * @param targetCRS
     * @return
     * @throws InvalidGridGeometryException
     * @throws FactoryException
     * @throws MismatchedDimensionException
     * @throws TransformException
     */
    private double[][] getXYAxis(GridGeometry2D geom,
            CoordinateReferenceSystem targetCRS)
            throws InvalidGridGeometryException, FactoryException,
            MismatchedDimensionException, TransformException {
        double[][] rval = new double[2][];
        GridEnvelope2D gridEnv = geom.getGridRange2D();
        MathTransform gridToCRS = geom
                .getGridToCRS(PixelOrientation.UPPER_LEFT);
        CoordinateReferenceSystem origCrs = geom.getCoordinateReferenceSystem();
        MathTransform toTarget = CRS
                .findMathTransform(origCrs, targetCRS, true);
        MathTransform transform = ConcatenatedTransform.create(gridToCRS,
                toTarget);
        rval[0] = new double[gridEnv.width];
        rval[1] = new double[gridEnv.height];
        int maxLen = Math.max(gridEnv.width, gridEnv.height);
        int xIndex = 0;
        int yIndex = 0;
        for (int i = 0; i < maxLen; ++i) {
            DirectPosition2D point = new DirectPosition2D(xIndex, yIndex);
            DirectPosition2D target = new DirectPosition2D();
            transform.transform(point, target);
            rval[0][xIndex] = target.getX();
            rval[1][yIndex] = target.getY();
            xIndex = Math.min(++xIndex, gridEnv.width - 1);
            yIndex = Math.min(++yIndex, gridEnv.height - 1);
        }
        return rval;
    }

    /**
     * Create unique id for file store
     * 
     * @param coverage
     * @return
     */
    private String createStoreId(Coverage coverage) {
        String name = coverage.getName();
        String rval;
        do {
            HashCodeBuilder builder = new HashCodeBuilder();
            builder.append(name).append(System.currentTimeMillis());
            builder.append(STORAGE_ID.incrementAndGet());
            String hash = Integer.toHexString(builder.toHashCode());
            rval = name + hash + ".nc";
        } while (store.getFile(rval) != null);

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.format.WcsDataFormatter#matchesFormat(java.lang
     * .String)
     */
    @Override
    public boolean matchesFormat(String format) {
        return format.toLowerCase().contains("netcdf");
    }

}
