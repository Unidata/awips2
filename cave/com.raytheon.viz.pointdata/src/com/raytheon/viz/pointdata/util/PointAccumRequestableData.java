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
package com.raytheon.viz.pointdata.util;

import java.util.Arrays;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.pointdata.accumulate.AccumDataRequestMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamConstantField;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * Carries out the Accum derived parameter method by sending an accum request to
 * edex.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointAccumRequestableData extends AbstractRequestableData {

    private AccumDataRequestMessage request;

    private List<AbstractRequestableData> idRequesters;

    private AbstractRequestableData timeRequester;

    public PointAccumRequestableData(
            List<AbstractRequestableData> idRequesters,
            AbstractRequestableData timeRequester, DerivParamMethod method,
            String plugin) throws VizException {
        this.idRequesters = idRequesters;
        this.timeRequester = timeRequester;
        this.request = new AccumDataRequestMessage();
        int index = method.getFields().size() - 1;
        int minutes = ((DerivParamConstantField) method.getFields()
                .get(index--)).getValue().intValue();
        int inc = ((DerivParamConstantField) method.getFields().get(index--))
                .getValue().intValue();
        String accumParameter = ((DerivParamField) method.getFields().get(
                index--)).getParam();
        request.setPluginName(plugin);
        request.setTotalMinutes(minutes);
        request.setIncMinutes(inc);
        request.setParameter(accumParameter);
        request.setTimeParameter(timeRequester.getParameter());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDataValue
     * ()
     */
    @Override
    public Object getDataValue(Object arg) throws VizException {
        StringBuilder stationParameter = new StringBuilder();
        String[] stations = null;
        for (AbstractRequestableData idRequestor : idRequesters) {
            if (stationParameter.length() > 0) {
                stationParameter.append(",");
            }
            stationParameter.append(idRequestor.getParameter());
            String[] data = null;
            Object rec = idRequestor.getDataValue(null);
            if (rec instanceof StringDataRecord) {
                data = ((StringDataRecord) rec).getStringData();
            } else if (rec instanceof IntegerDataRecord) {
                int[] intData = ((IntegerDataRecord) rec).getIntData();
                data = new String[intData.length];
                for (int i = 0; i < intData.length; i++) {
                    data[i] = Integer.toString(intData[i]);
                }
            }
            if (stations == null) {
                stations = Arrays.copyOf(data, data.length);
            } else {
                for (int i = 0; i < data.length; i++) {
                    stations[i] = stations[i] + "," + data[i];
                }
            }
        }
        request.setStationParameter(stationParameter.toString());
        request.setStations(stations);
        LongDataRecord ldr = (LongDataRecord) timeRequester.getDataValue(null);
        long[] times = ldr.getLongData();
        if (SI.MILLI(SI.SECOND).isCompatible(timeRequester.getUnit())
                && !SI.MILLI(SI.SECOND).equals(timeRequester.getUnit())) {
            UnitConverter converter = timeRequester.getUnit().getConverterTo(
                    SI.MILLI(SI.SECOND));
            for (int i = 0; i < times.length; i++) {
                times[i] = (long) converter.convert(times[i]);
            }
        }
        request.setTimes(times);
        IDataRecord result = (IDataRecord) ThriftClient.sendRequest(request);
        result.setName(parameter);
        return result;
    }

}
