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

package com.raytheon.edex.plugin.obs.mesowest;

import java.text.DecimalFormat;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Decoder implementation for mesowest plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/14/06		139			bphillip	Initial creation	
 * 11/11/08     1684        chammack    Refactored to camel interfaces
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class MesowestDecoder extends AbstractDecoder {

    private static final DecimalFormat DECIMALTENTHS = new DecimalFormat("#.#");

    private static final DecimalFormat WHOLENUMBER = new DecimalFormat("#");

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public MesowestDecoder() throws DecoderException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IBinaryDecoder#decode(byte[])
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        byte[] messageData = null;

        String theMessage = new String(messageData);

        MesowestRecord record = new MesowestRecord();
        record.setMessageData(theMessage);
        record.setPluginName("obs");
        record.setReportType("MESOWEST");
        record.setMessageData(theMessage);

        String[] mwObs = theMessage.split(",");
        try {
            if (mwObs.length >= 7) {
                record.setStationID(mwObs[1]);
                String timeGroup = mwObs[6].substring(6, 8)
                        + mwObs[6].substring(9, 13) + "Z";
                String fileName = null;
                if (headers != null) {
                    fileName = (String) headers
                            .get(DecoderTools.INGEST_FILE_NAME);
                }
                record.setTimeObs(TimeTools
                        .findCurrentTime(timeGroup, fileName));
                record.setDataTime(new DataTime(Util
                        .findReferenceTime(timeGroup)));
                record.setRefHour(Util.findReferenceHour(timeGroup));
            }
            // Only want the observations in columns 7 through 15
            int mwArrayLength = mwObs.length < 17 ? mwObs.length : 17;
            for (int i = 7; i < mwArrayLength; i++) {
                if (i == 7 && mwObs[7].length() > 0) {
                    double temp = calculateCelsius(mwObs[7]);
                    record.setTemperature(Integer.parseInt(WHOLENUMBER
                            .format(temp)));
                    record.setTempFromTenths(Float.parseFloat(DECIMALTENTHS
                            .format(temp)));
                } else if (i == 9 && mwObs[9].length() > 0) {
                    double wSpeed = Double.valueOf(mwObs[9].trim())
                            .doubleValue();
                    record.setWindSpeed(Integer.parseInt(WHOLENUMBER
                            .format(wSpeed)));
                } else if (i == 10 && mwObs[10].length() > 0) {
                    double wGust = Double.valueOf(mwObs[10].trim())
                            .doubleValue();
                    record.setWindGust(Integer.parseInt(WHOLENUMBER
                            .format(wGust)));
                } else if (i == 11 && mwObs[11].length() > 0) {
                    record.setWindDir(mwObs[11]);
                } else if (i == 13 && mwObs[13].length() > 0) {
                    double dewp = calculateCelsius(mwObs[13]);
                    record.setDewPoint(Integer.parseInt(WHOLENUMBER
                            .format(dewp)));
                    record.setDewPointFromTenths(Float.parseFloat(DECIMALTENTHS
                            .format(dewp)));
                } else if (i == 15 && mwObs[15].length() > 0) {
                    record.setSeaLevelPress(Float.parseFloat(mwObs[15]));
                } else if (i == 16 && mwObs[16].length() > 0) {
                    record.setAltimeter(Float.parseFloat(mwObs[16]));
                }
            }

        } catch (Exception e) {
            throw new DecoderException("Unable to decode Mesowest data", e);
        }

        if (record == null)
            return new PluginDataObject[0];
        return new PluginDataObject[] { record };
    }

    private double calculateCelsius(String tempString) {
        double temp = Double.valueOf(tempString.trim()).doubleValue();
        double tempC = (temp - 32.0) * (5.0 / 9.0);
        return tempC;
    }

}
