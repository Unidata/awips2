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
package com.raytheon.uf.common.inventory.data;

import java.util.Arrays;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.inventory.exception.DataCubeException;

/**
 * Represents a simple alias, where a parameter represents the same data as
 * another parameter. This does automatic unit conversion if the source data is
 * in a different unit than this object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 15, 2010  3965     rjpeter     Initial creation
 * Jan 14, 2014  2661     bsteffen    Make vectors u,v only
 * Apr 11, 2014  2947     bsteffen    Perform unit conversion on more types of
 *                                    records.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class AliasRequestableData extends AbstractRequestableData {

    protected AbstractRequestableData sourceRecord;

    public AliasRequestableData(AbstractRequestableData sourceRecord) {
        this.sourceRecord = sourceRecord;
        this.dataTime = sourceRecord.getDataTime();
        this.space = sourceRecord.space;
    }

    @Override
    public Object getDataValue(Object arg) throws DataCubeException {
        return getDataAndConvert(sourceRecord, arg);
    }

    protected Object getDataAndConvert(AbstractRequestableData record,
            Object arg) throws DataCubeException {
        Object rval = record.getDataValue(arg);
        // Clone and rename records.
        if (rval instanceof IDataRecord) {
            rval = ((IDataRecord) rval).clone();
            if (this.parameter != null) {
                ((IDataRecord) rval).setName(this.parameter);
            }
        } else if (rval instanceof FloatDataRecord[]) {
            FloatDataRecord[] recs = (FloatDataRecord[]) rval;
            IDataRecord[] newRecs = new FloatDataRecord[recs.length];
            rval = newRecs;
            for (int i = 0; i < recs.length; i++) {
                newRecs[i] = recs[i].clone();
                if (this.parameter != null) {
                    newRecs[i].setName(this.parameter);
                }
            }
        } else if (rval instanceof IDataRecord[]) {
            IDataRecord[] recs = (IDataRecord[]) rval;
            IDataRecord[] newRecs = new IDataRecord[recs.length];
            rval = newRecs;
            for (int i = 0; i < recs.length; i++) {
                newRecs[i] = recs[i].clone();
                if (this.parameter != null) {
                    newRecs[i].setName(this.parameter);
                }
            }
        }
        // Handle Unit conversion
        if (!unit.equals(record.getUnit())) {
            Unit<?> sourceUnits = record.getUnit();
            if (sourceUnits.isCompatible(unit)) {
                UnitConverter converter = sourceUnits.getConverterTo(unit);
                if (rval instanceof Float) {
                    if ((Float) rval > -9999) {
                        rval = converter.convert((Float) rval);
                    }
                } else if (rval instanceof IDataRecord) {
                    rval = convertDataRecord(converter, (IDataRecord) rval);
                } else if (rval instanceof IDataRecord[]) {
                    IDataRecord[] recs = (IDataRecord[]) rval;
                    if (recs.length != 1 || !unit.equals(NonSI.DEGREE_ANGLE)) {
                        rval = recs = Arrays.copyOf(recs, recs.length);
                        for (int i = 0; i < recs.length; i++) {
                            recs[i] = convertDataRecord(converter, recs[i]);
                        }
                    }
                }
            }
        }
        return rval;
    }

    private static IDataRecord convertDataRecord(UnitConverter converter,
            IDataRecord record) {
        float[] newData = null;
        double fillValue = Double.NaN;
        if (record.getFillValue() != null) {
            fillValue = record.getFillValue().doubleValue();
        }
        if (record instanceof FloatDataRecord) {
            float[] data = ((FloatDataRecord) record).getFloatData();
            newData = new float[data.length];
            for (int c = 0; c < data.length; c++) {
                if (data[c] == fillValue) {
                    newData[c] = Float.NaN;
                } else {
                    newData[c] = (float) converter.convert(data[c]);
                }
            }
        } else if (record instanceof ByteDataRecord) {
            byte[] data = ((ByteDataRecord) record).getByteData();
            newData = new float[data.length];
            for (int c = 0; c < data.length; c++) {
                if (data[c] == fillValue) {
                    newData[c] = Float.NaN;
                } else {
                    newData[c] = (float) converter.convert(data[c]);
                }
            }
        } else if (record instanceof ShortDataRecord) {
            short[] data = ((ShortDataRecord) record).getShortData();
            newData = new float[data.length];
            for (int c = 0; c < data.length; c++) {
                if (data[c] == fillValue) {
                    newData[c] = Float.NaN;
                } else {
                    newData[c] = (float) converter.convert(data[c]);
                }
            }
        } else if (record instanceof IntegerDataRecord) {
            int[] data = ((IntegerDataRecord) record).getIntData();
            newData = new float[data.length];
            for (int c = 0; c < data.length; c++) {
                if (data[c] == fillValue) {
                    newData[c] = Float.NaN;
                } else {
                    newData[c] = (float) converter.convert(data[c]);
                }
            }
        } else{
            return record;
        }
        return new FloatDataRecord(record.getName(), record.getGroup(),
                newData, record.getDimension(), record.getSizes());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDependencies
     * ()
     */
    @Override
    public List<AbstractRequestableData> getDependencies() {
        return Arrays.asList(sourceRecord);
    }
}
