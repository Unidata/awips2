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
package com.raytheon.uf.edex.plugin.goesr.decoder.lookup;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import ucar.ma2.Array;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrDecoderException;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;
import com.raytheon.uf.edex.plugin.goesr.geospatial.GoesrProjectionFactory;

/**
 * A description of the data contained with a {@link NetcdfFile} that can be
 * used to extract the message data for use in a {@link SatelliteRecord}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataDescription {

    private static final int BITSET_FILL = 0xFF;

    @XmlAttribute
    private String variable;

    @XmlAttribute
    private List<String> bitset;

    @XmlAttribute
    private String maskVariable;

    @XmlAttribute
    private String verticalDimension;

    @XmlElement(name = "mask")
    private List<DataMaskDescription> masks;

    public String getVariable() {
        return variable;
    }

    public void setVariable(String variable) {
        this.variable = variable;
    }

    public List<String> getBitset() {
        return bitset;
    }

    public void setBitset(List<String> bitset) {
        this.bitset = bitset;
    }

    public String getMaskVariable() {
        return maskVariable;
    }

    public void setMaskVariable(String maskVariable) {
        this.maskVariable = maskVariable;
    }

    public List<DataMaskDescription> getMasks() {
        return masks;
    }

    public void setMasks(List<DataMaskDescription> masks) {
        this.masks = masks;
    }

    public String getVerticalDimension() {
        return verticalDimension;
    }

    public void setVerticalDimension(String verticalDimension) {
        this.verticalDimension = verticalDimension;
    }

    /**
     * Apply this description to the specified file and extract all
     * {@link SatelliteRecord}s.
     * 
     * @param cdfFile
     * @param projectionFactory
     * @return
     * @throws GoesrProjectionException
     */
    public List<SatelliteRecord> getData(NetcdfFile cdfFile,
            GoesrProjectionFactory projectionFactory)
            throws GoesrDecoderException {
        if (variable != null) {
            Variable dataVariable = cdfFile.findVariable(this.variable);
            if (dataVariable != null) {
                Attribute attr = dataVariable.findAttribute("grid_mapping");
                Object data;
                try {
                    data = dataVariable.read().copyTo1DJavaArray();
                } catch (IOException e) {
                    throw new GoesrDecoderException("Unable to read data from "
                            + this.variable, e);
                }
                SatMapCoverage coverage = projectionFactory.getCoverage(
                        cdfFile, attr.getStringValue());

                Map<String, Object> attributes = new HashMap<>();
                attr = dataVariable.findAttribute("add_offset");
                if (attr != null) {
                    attributes.put(SatelliteRecord.SAT_ADD_OFFSET, attr
                            .getNumericValue().floatValue());
                }
                attr = dataVariable.findAttribute("scale_factor");
                if (attr != null) {
                    attributes.put(SatelliteRecord.SAT_SCALE_FACTOR, attr
                            .getNumericValue().floatValue());
                }

                Number fillValue = getFillValue(dataVariable, data);


                String units = getUnits(dataVariable);

                if (verticalDimension == null) {
                    applyMask(cdfFile, data, fillValue);
                    SatelliteRecord record = createNewRecord(coverage, data,
                            fillValue, attributes);
                    if (units != null) {
                        record.setUnits(units);
                    }
                    return Arrays.asList(record);
                } else {
                    List<SatelliteRecord> records = build3D(cdfFile, coverage,
                            data, fillValue, attributes);
                    if (units != null) {
                        for (SatelliteRecord record : records) {
                            record.setUnits(units);
                        }
                    }
                    return records;
                }
            }
        } else if (bitset != null) {
            try {
                SatelliteRecord record = buildRecordFromBitset(cdfFile,
                        projectionFactory);
                return Arrays.asList(record);
            } catch (IOException e) {
                throw new GoesrDecoderException("Unable to read data from "
                        + variable, e);
            }
        }
        return null;
    }

    private void applyMask(NetcdfFile cdfFile, Object data, Number fillValue)
            throws GoesrDecoderException {
        if (this.maskVariable == null) {
            return;
        }
        Variable maskVariable = cdfFile.findVariable(this.maskVariable);
        Object maskData = null;
        try {
            maskData = maskVariable.read().copyTo1DJavaArray();
        } catch (IOException e) {
            throw new GoesrDecoderException("Unable to read data from "
                    + this.variable, e);
        }
        /*
         * Depending on the type one of bMaskData or sMaskData must be set to
         * not null.
         */
        byte[] bMaskData = null;
        short[] sMaskData = null;
        if (maskData instanceof byte[]) {
            bMaskData = (byte[]) maskData;
        } else if (maskData instanceof short[]) {
            sMaskData = (short[]) maskData;
        } else {
            throw new GoesrDecoderException("Unexpected data of type: "
                    + data.getClass().getSimpleName());
        }
        /* Depending on the type one of bdata or sdata must be set to not null. */
        byte[] bdata = null;
        short[] sdata = null;
        if (data instanceof short[]) {
            sdata = (short[]) data;
        } else if (data instanceof byte[]) {
            bdata = (byte[]) data;
        } else {
            throw new GoesrDecoderException("Unexpected data of type: "
                    + data.getClass().getSimpleName());
        }
        int length = java.lang.reflect.Array.getLength(data);
        for (int i = 0; i < length; i += 1) {
            boolean fill = true;
            for (DataMaskDescription mask : this.masks) {
                int maskVal = bMaskData == null ? sMaskData[i] : bMaskData[i];
                if (mask.getValue() == maskVal) {
                    if (!mask.isKeep()) {
                        if (bdata == null) {
                            sdata[i] = (short) mask.getFill();
                        } else {
                            bdata[i] = (byte) mask.getFill();
                        }
                    }
                    fill = false;
                    break;
                }
            }
            if (fill) {
                if (bdata == null) {
                    sdata[i] = fillValue.shortValue();
                } else {
                    bdata[i] = fillValue.byteValue();
                }
            }
        }
    }

    private List<SatelliteRecord> build3D(NetcdfFile cdfFile,
            SatMapCoverage coverage, Object data, Number fillValue,
            Map<String, Object> attributes) throws GoesrDecoderException {
        Variable thirdD = cdfFile.findVariable(verticalDimension);
        Array dimData;
        try {
            dimData = thirdD.read();
        } catch (IOException e) {
            throw new GoesrDecoderException("Unable to read data from "
                    + this.verticalDimension, e);
        }
        List<SatelliteRecord> records = new ArrayList<>();
        for (int i = 0; i < thirdD.getSize(); i += 1) {
            int index = 0;
            Object newData = null;
            if (data instanceof short[]) {
                short[] rawData = (short[]) data;
                short[] cutData = new short[coverage.getNx() * coverage.getNy()];
                for (int j = i; j < rawData.length; j += thirdD.getSize()) {
                    cutData[index++] = rawData[j];
                }
                newData = cutData;
            }else if(data instanceof byte[]){
                byte[] rawData = (byte[]) data;
                byte[] cutData = new byte[coverage.getNx() * coverage.getNy()];
                for (int j = i; j < rawData.length; j += thirdD.getSize()) {
                    cutData[index++] = rawData[j];
                }
                newData = cutData;
            }else{
                throw new GoesrDecoderException("Unexpected data of type: " + data.getClass().getSimpleName());
            }
            if (attributes == null) {
                attributes = new HashMap<>(4);
            } else {
                attributes = new HashMap<>(attributes);
            }
            applyMask(cdfFile, newData, fillValue);
            /*
             * This allows the vertical dimension to be used in
             * ProductDescriptions so that the records generated have unique
             * attributes.
             */
            attributes.put(verticalDimension, dimData.getFloat(i));
            SatelliteRecord record = createNewRecord(coverage, newData, fillValue,
                    attributes);
            records.add(record);
        }
        return records;
    }

    private SatelliteRecord buildRecordFromBitset(NetcdfFile cdfFile,
            GoesrProjectionFactory projectionFactory) throws IOException,
            GoesrDecoderException {
        if (bitset.size() > 7) {
            throw new GoesrDecoderException(
                    "Bitset can only currently support 7 fields, this is too many: "
                            + bitset.toString());
        }
        String grid_mapping = null;
        byte[] data = null;
        int bit = 0;
        for (String variable : bitset) {
            Variable dataVariable = cdfFile.findVariable(variable);
            Attribute attr = dataVariable.findAttribute("grid_mapping");
            if (grid_mapping == null) {
                grid_mapping = attr.getStringValue();
            } else if (attr != null
                    && !attr.getStringValue().equals(grid_mapping)) {
                throw new GoesrProjectionException(
                        "Bitset projections do not match.");
            }

            Object rawField = dataVariable.read().copyTo1DJavaArray();
            if (!(rawField instanceof byte[])) {
                throw new GoesrDecoderException("Unexpected type["
                        + rawField.getClass() + "] for variable[" + variable
                        + "]");
            }
            byte[] field = (byte[]) rawField;
            int fillValue = 0xFF & getFillValue(dataVariable, field).intValue();

            if (data == null) {
                data = field;
                if (fillValue != BITSET_FILL) {
                    for (int i = 0; i < field.length; i += 1) {
                        if (fillValue == (0xFF & field[i])) {
                            field[i] = (byte) BITSET_FILL;
                        }
                    }
                }

            } else {
                for (int i = 0; i < field.length; i += 1) {
                    if (fillValue == (0xFF & field[i])) {
                        /*
                         * Do Nothing, this is an assumption. When thinking
                         * about a bitset, normally fill is not an option for an
                         * input but this data has fill and it is used. Do
                         * nothing means if the data is already filled it will
                         * stay filled, so any index where all fields are filled
                         * will be filled but if any one field is valid then all
                         * filled fields will be 0s
                         */
                        continue;
                    } else if (BITSET_FILL == (0xFF & field[i])) {
                        data[i] = 0;
                    }
                    if (field[i] != 0) {
                        data[i] = (byte) (data[i] + (1 << bit));
                    }
                }
            }
            bit += 1;
        }
        SatMapCoverage coverage = projectionFactory.getCoverage(cdfFile,
                grid_mapping);
        applyMask(cdfFile, data, BITSET_FILL);
        return createNewRecord(coverage, data, BITSET_FILL, null);
    }

    private static String getUnits(Variable variable) {
        Attribute attr = variable.findAttribute("units");
        if (attr != null && !"1".equals(attr.getStringValue())) {
            return attr.getStringValue();
        }
        return null;
    }

    private static SatelliteRecord createNewRecord(SatMapCoverage coverage,
            Object data, Number fillValue, Map<String, Object> attributes) {
        long[] sizes = new long[] { coverage.getNx(), coverage.getNy() };
        IDataRecord storageRecord = DataStoreFactory.createStorageRecord(
                SatelliteRecord.SAT_DATASET_NAME, null, data, 2, sizes);
        storageRecord.setFillValue(fillValue);
        storageRecord.setDataAttributes(attributes);
        SatelliteRecord record = new SatelliteRecord();
        record.setCoverage(coverage);
        record.setMessageData(storageRecord);
        return record;
    }

    private static Number getFillValue(Variable variable, Object data) {
        Number fillValue = 0;
        Attribute attr = variable.findAttribute("_FillValue");
        if (attr != null) {
            fillValue = attr.getNumericValue();
            attr = variable.findAttribute("_Unsigned");
            if (attr != null) {
                boolean unsigned = attr.getStringValue().equals("true");
                if (unsigned && fillValue.intValue() < 0) {
                    if (data instanceof byte[]) {
                        fillValue = 0xFF & (fillValue.intValue());
                    } else if (data instanceof short[]) {
                        fillValue = 0xFFFF & (fillValue.intValue());
                    }
                }
            }
        }
        return fillValue;
    }
}
