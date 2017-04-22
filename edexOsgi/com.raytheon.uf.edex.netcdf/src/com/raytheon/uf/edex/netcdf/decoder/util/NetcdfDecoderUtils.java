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
package com.raytheon.uf.edex.netcdf.decoder.util;

import java.io.IOException;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import javax.measure.unit.Unit;

import ucar.ma2.Array;
import ucar.ma2.DataType;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Range;
import ucar.ma2.Section;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.edex.netcdf.decoder.NetcdfRecordInfo;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.date.AbstractDateValue;
import com.raytheon.uf.edex.netcdf.description.field.date.DataTimeDescription;
import com.raytheon.uf.edex.netcdf.description.field.date.ForecastDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.DimensionDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;
import com.raytheon.uf.edex.netcdf.description.field.level.LevelDescription;

/**
 * Utility class for decoding netcdf files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2015 4698       nabowle     Initial creation
 * Mar 21, 2016 5450       nabowle     Extracted useful methods to here.
 * Apr 19, 2016 5450       nabowle     Add getTimeDimensionName.
 * May 23, 2016 5584       nabowle     Updates for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class NetcdfDecoderUtils {
    public static final String MISSING_VALUE = "missing_value";

    public static final String FILL_VALUE = "fill_value";

    public static final String[] NO_DATA_ATTRS = new String[] { MISSING_VALUE,
            FILL_VALUE, "_FillValue" };

    public static final String ADD_OFFSET = "add_offset";

    public static final String SCALE_FACTOR = "scale_factor";

    public static final Integer DEFAULT_SCALE_FACTOR = Integer.valueOf(1);

    public static final Integer DEFAULT_ADD_OFFSET = Integer.valueOf(0);

    public static final Pattern LONGITUDE_COORDINATE_PATTERN = Pattern.compile(
            "LON", Pattern.CASE_INSENSITIVE);

    public static final Pattern LATITUDE_COORDINATE_PATTERN = Pattern.compile(
            "LAT", Pattern.CASE_INSENSITIVE);

    private NetcdfDecoderUtils() {
        super();
    }

    /**
     * Get's the no-data value for the variable. If a no-data attribute cannot
     * be found, the supplied default value is returned.
     *
     * @param variable
     *            The variable to get the no-data value for.
     * @param defaultValue
     *            The value to return if a no-data attribute cannot be found.
     * @return The no-data value for the variable, or the supplied default value
     *         if it could not be determined.
     */
    public static Number getNoDataValue(Variable variable, Number defaultValue) {
        return getNumericAttributeValue(variable, defaultValue, NO_DATA_ATTRS);
    }

    /**
     * Gets the add offset for the variable.
     *
     * @param variable
     *            The variable.
     * @return The add offset for the variable, or {@link #DEFAULT_ADD_OFFSET}
     *         if not found.
     */
    public static Number getAddOffset(Variable variable) {
        return getNumericAttributeValue(variable, DEFAULT_ADD_OFFSET,
                ADD_OFFSET);
    }

    /**
     * Gets the scale factor for the variable.
     *
     * @param variable
     *            The variable.
     * @return The scale factor for the variable, or
     *         {@link #DEFAULT_SCALE_FACTOR} if not found.
     */
    public static Number getScaleFactor(Variable variable) {
        return getNumericAttributeValue(variable, DEFAULT_SCALE_FACTOR,
                SCALE_FACTOR);
    }

    /**
     * Gets a numeric attribute value from the variable. The supplied list of
     * names will be checked in order, returning the value for only the first
     * found attribute. If no attribute is found for any of the attribute names,
     * the default value is returned.
     *
     * @param variable
     *            The variable.
     * @param defaultVal
     *            The default value to return if the attribute cannot be found.
     * @param attributeNames
     *            The list of possible names for the attribute, case
     *            insensitive.
     * @return The numeric value for the attribute, or the supplied default
     *         value if the attribute could not be found.
     */
    public static Number getNumericAttributeValue(Variable variable,
            Number defaultVal, String... attributeNames) {
        Attribute attr;
        for (String attrName : attributeNames) {
            attr = variable.findAttributeIgnoreCase(attrName);
            if (attr != null) {
                return attr.getNumericValue();
            }
        }
        return defaultVal;
    }

    /**
     * Gets the {@link Section} of a variable that can be used to read one
     * record's worth of data. The Section is created based on the variable's
     * dimensions and the recordDimensionIndices mappings.
     *
     * For instance, if the map parameter contains the entry { "depth" => 1},
     * the "depth" dimension of the variable will have it's range limited to 1
     * in the created Section.
     *
     *
     * @param dataVar
     *            The variable to create a Section for.
     * @param recordDimensionIndices
     *            Map of the record-defining dimension indices. The keys of the
     *            map are expected to match any number of the variable dimension
     *            names. The values of the map are indices. Only dimensions that
     *            together separate records should have entries in this map.
     * @return The created Section.
     * @throws NetcdfDecoderException
     *             If a created range is invalid.
     */
    public static Section getRecordSection(Variable dataVar,
            Map<String, Integer> recordDimensionIndices)
            throws NetcdfDecoderException {
        List<Dimension> dimensions = dataVar.getDimensions();
        List<Range> ranges = dataVar.getRanges();

        /*
         * Add the Ranges to the Section. The Range for any variable in
         * recordDimensionIndices is limited to just the the mapped index, while
         * all others are left at their full Range.
         */
        Section sect = new Section();
        Dimension dim;
        Integer dimensionIndex;
        for (int i = 0; i < dimensions.size(); i++) {
            dim = dimensions.get(i);
            if ((dimensionIndex = recordDimensionIndices.get(dim.getName())) != null) {
                try {
                    // start and end range values are inclusive.
                    sect.appendRange(new Range(dimensionIndex, dimensionIndex));
                } catch (InvalidRangeException e) {
                    throw new NetcdfDecoderException(
                            "Unable to define the range with index "
                                    + dimensionIndex + " for Dimension "
                                    + dim.getName(), e);
                }
            } else {
                sect.appendRange(ranges.get(i));
            }
        }
        return sect;
    }

    /**
     * Get the dimension whose name that matches the a level name.
     *
     * @param dataVar
     *            The variable whose dimensions to check.
     * @param LevelDescription
     *            The level description
     * @return The level dimension, or null if one cannot be found.
     */
    public static Dimension getLevelDimension(Variable dataVar,
            LevelDescription level) {
        if (dataVar == null) {
            return null;
        }

        List<String> names = new ArrayList<>();

        String name;
        DimensionDescription dimDesc = level.getDimension();
        if (dimDesc != null && (name = dimDesc.getName()) != null) {
            names.add(name);
        }

        if (level.getLevelOneValue() != null
                && (name = level.getLevelOneValue().getName()) != null) {
            names.add(name);
        }
        if (level.getLevelTwoValue() != null
                && (name = level.getLevelTwoValue().getName()) != null) {
            names.add(name);
        }
        if (level.getMasterLevel() != null
                && (name = level.getMasterLevel().getName()) != null) {
            names.add(name);
        }
        return findDimension(dataVar, names, null);
    }

    /**
     * Returns the dimension whose name matches a DataTime name.
     *
     * @param dataVar
     *            The data variable.
     * @param timeDesc
     *            The datatime description.
     * @return The time dimension. If a time dimension can't be identified, null
     *         is returned.
     */
    public static Dimension getDataTimeDimension(Variable dataVar,
            DataTimeDescription timeDesc) {
        if (dataVar == null || timeDesc == null) {
            return null;
        }

        List<String> names = new ArrayList<>();

        String name;

        DimensionDescription dimDesc = timeDesc.getDimension();
        if (dimDesc != null && (name = dimDesc.getName()) != null) {
            names.add(name);
        }

        AbstractDateValue refTime = timeDesc.getRefTime();
        if (refTime != null && (name = refTime.getDelegate().getName()) != null) {
            names.add(name);
        }

        ForecastDescription forecast = timeDesc.getForecast();
        if (forecast != null
                && (name = forecast.getDelegate().getName()) != null) {
            names.add(name);
        }

        AbstractDateValue validTime = timeDesc.getValidTime();
        if (validTime != null
                && (name = validTime.getDelegate().getName()) != null) {
            names.add(name);
        }

        return findDimension(dataVar, names, null);
    }

    /**
     * Searches through the data variable's dimensions looking for the first
     * matching dimension. The dimensions will first be checked against the list
     * of possible names. If no matches are found within the list of possible
     * names, the dimensions will be checked against the supplied pattern.
     *
     * @param dataVar
     *            The data variable to find the dimension for.
     * @param possibleNames
     *            The list of dimension names to look for.
     * @param pattern
     *            The pattern to match against.
     * @return
     */
    public static Dimension findDimension(Variable dataVar,
            List<String> possibleNames, Pattern pattern) {
        if (dataVar == null) {
            return null;
        }

        List<Dimension> dimensions = dataVar.getDimensions();
        if (possibleNames != null && !possibleNames.isEmpty()) {
            for (Dimension dim : dimensions) {
                for (String name : possibleNames) {
                    if (dim.getName().equals(name)) {
                        return dim;
                    }
                }
            }
        }

        if (pattern != null) {
            for (Dimension dim : dimensions) {
                if (pattern.matcher(dim.getName()).matches()) {
                    return dim;
                }
            }
        }

        return null;
    }

    /**
     * Reads data from data variable.
     *
     * @param dataVariable
     *            The data variable.
     * @param sect
     *            The section of the data to read. If null, the full data will
     *            be read.
     * @param fillValue
     *            The value used by the Variable to indicate No Data for
     *            floating-point data. Values matching this will be replaced
     *            with NaN. This may be null and is ignored if null or the
     *            datatype is not {@link DOUBLE} or {@link FLOAT}.
     * @param scale
     *            If present, the data will be cast to a float and multiplied by
     *            this value.
     * @param addOffset
     *            If present, the data will be cast to a float and have this
     *            value added to it.
     * @return A Buffer containing the read data.
     * @throws NetcdfDecoderException
     *             if there is an issue reading the data.
     *
     */
    public static Buffer readData(Variable dataVariable, Section sect,
            Number fillValue, Number scale, Number addOffset,
            Set<Integer> flipDimensions) throws NetcdfDecoderException {
        if (sect == null) {
            sect = getRecordSection(dataVariable,
                    new HashMap<String, Integer>());
        }
        Buffer numericData = null;
        DataType dataType = dataVariable.getDataType();
        if (scale != null || addOffset != null) {
            dataType = DataType.FLOAT;
        }
        try {
            switch (dataType) {
            case DOUBLE:
                /*
                 * At this point in time(2015) there are no known cases of data
                 * being sent as a double and actually needing the extra
                 * precision, also not all pieces of CAVE currently support
                 * doubles so the precision would not be needed anyway. AMSR-2
                 * derived surface wind speed is being needlessly sent as
                 * doubles. Because of all this, just convert doubles to floats.
                 */
            case FLOAT:
                float[] fdata = (float[]) getDataArray(dataVariable, sect,
                        flipDimensions).get1DJavaArray(float.class);
                if (fillValue != null) {
                    float ffill = fillValue.floatValue();
                    for (int i = 0; i < fdata.length; i += 1) {
                        if (fdata[i] == ffill) {
                            fdata[i] = Float.NaN;
                        }
                    }
                }
                if (scale != null) {
                    float fScale = scale.floatValue();
                    for (int i = 0; i < fdata.length; i += 1) {
                        fdata[i] *= fScale;
                    }
                }
                if (addOffset != null) {
                    float fOffset = addOffset.floatValue();
                    for (int i = 0; i < fdata.length; i += 1) {
                        fdata[i] += fOffset;
                    }
                }
                numericData = FloatBuffer.wrap(fdata);
                break;
            case BYTE:
                byte[] bdata = (byte[]) getDataArray(dataVariable, sect,
                        flipDimensions).get1DJavaArray(byte.class);
                numericData = ByteBuffer.wrap(bdata);
                break;
            case SHORT:
                short[] sdata = (short[]) getDataArray(dataVariable, sect,
                        flipDimensions).get1DJavaArray(short.class);
                numericData = ShortBuffer.wrap(sdata);
                break;
            case INT:
                int[] idata = (int[]) getDataArray(dataVariable, sect,
                        flipDimensions).get1DJavaArray(int.class);
                numericData = IntBuffer.wrap(idata);
                break;
            case LONG:
                long[] ldata = (long[]) getDataArray(dataVariable, sect,
                        flipDimensions).get1DJavaArray(long.class);
                numericData = LongBuffer.wrap(ldata);
                break;
            default:
                throw new NetcdfDecoderException("Unable to read "
                        + dataVariable.getFullNameEscaped()
                        + " because the dataType " + dataType
                        + " was not recognized");
            }
        } catch (IOException | InvalidRangeException e) {
            throw new NetcdfDecoderException("Unable to read "
                    + dataVariable.getFullNameEscaped() + " due to "
                    + e.getMessage(), e);
        }
        return numericData;
    }

    /**
     * @param dataVariable
     * @param sect
     * @param flipDimensions
     * @return
     * @throws IOException
     * @throws InvalidRangeException
     */
    public static Array getDataArray(Variable dataVariable, Section sect,
            Set<Integer> flipDimensions) throws IOException,
            InvalidRangeException {
        Array arr = dataVariable.read(sect);
        if (arr != null && flipDimensions != null) {
            for (Integer dimIdx : flipDimensions) {
                arr = arr.flip(dimIdx.intValue());
            }
        }
        return arr;
    }

    /**
     * Attempts to update the given Parameter's name and unit from the supplied
     * Variable, assuming they are not already set.
     *
     * @param param
     *            The parameter to update. This object should be assumed to be
     *            modified by this method.
     * @param dataVariable
     *            The data variable to use.
     */
    public static void updateParameterFromVariable(Parameter param, Variable dataVariable) {
        if (param.getName() == null || param.getName().trim().isEmpty()) {
            Attribute longNameAttribute = dataVariable
                    .findAttribute("long_name");
            if (longNameAttribute != null) {
                param.setName(longNameAttribute.getStringValue());
            }
        }

        if (param.getUnit() == null || param.getUnit() == Unit.ONE) {
            Attribute unitsAttribute = dataVariable.findAttribute("units");
            if (unitsAttribute != null) {
                param.setUnitString(unitsAttribute.getStringValue());
            }
        }
    }

    /**
     * Removes 1-length sizes since they don't really affect the shape of the
     * data.
     */
    public static int[] trimShape(int[] shape) {
        int oneLengthedDims = 0;
        for (int size : shape) {
            if (size == 1) {
                oneLengthedDims++;
            }
        }
        if (oneLengthedDims > 0) {
            int[] temp = new int[shape.length - oneLengthedDims];
            for (int i = 0, j = 0; i < shape.length; i++) {
                if (shape[i] != 1) {
                    temp[j++] = shape[i];
                }
            }
            shape = temp;
        }
        return shape;
    }

    public static Variable getDataVariable(NetcdfRecordInfo info,
            NetcdfFile netcdfFile) {
        DataDescription dataDesc = (DataDescription) info
                .getDeferredDescription(DataDescription.DATA_KEY);
        if (dataDesc == null) {
            return null;
        }
        if (dataDesc.getVariable() == null) {
            return null;
        }
        String dataVarName = dataDesc.getVariable().getName();
        Variable dataVariable = netcdfFile.findVariable(dataVarName);
        if (dataVariable == null) {
            return null;
        }

        return dataVariable;
    }

    /**
     * Update the record infos with the given field. Non-scalar fields will be
     * deferred.
     *
     * @param netcdfFile
     * @param field
     * @param key
     * @param records
     * @throws InvalidDescriptionException
     */
    public static void updateField(NetcdfFile netcdfFile,
            DelegateFieldDescription field, String key,
            List<NetcdfRecordInfo> records) throws InvalidDescriptionException {
        if (field != null && field.isPresent(netcdfFile)) {
            long length = field.getLength(netcdfFile);
            if (length > 1) {
                for (NetcdfRecordInfo record : records) {
                    if (record.getBeanMap().get(key) == null) {
                        record.addDeferredDescription(key, field);
                    }
                }
            } else {
                String val = field.getString(netcdfFile);
                if (val != null) {
                    for (NetcdfRecordInfo record : records) {
                        record.addField(key, val);
                    }
                }
            }
        }
    }
}
