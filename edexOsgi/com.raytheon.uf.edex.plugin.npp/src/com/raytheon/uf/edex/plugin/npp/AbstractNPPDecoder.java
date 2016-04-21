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
package com.raytheon.uf.edex.plugin.npp;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.UUID;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Abstract NPP decoder, opens NetcdfFile for processing and extracts the
 * DataTime
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractNPPDecoder extends AbstractDecoder {

    private static final String START_TIME_ID = "time_coverage_start";

    private static final String END_TIME_ID = "time_coverage_end";

    private static final String DATE_PARSE_STRING = "yyyy-MM-dd'T'HH:mm:ss'Z'";

    public final Object decode(byte[] data, Headers headers)
            throws DecoderException {
        try {
            NetcdfFile dataFile = NetcdfFile.openInMemory(UUID.randomUUID()
                    .toString(), data);
            DataTime dataTime = extractDataTime(dataFile);
            if (dataTime != null) {
                return decodeNetcdf(dataFile, dataTime, headers);
            } else {
                throw new IllegalStateException(
                        "Could not extract DataTime from NPP data file");
            }
        } catch (Exception e) {
            throw new DecoderException("Error decoding npp data file", e);
        }
    }

    protected abstract Object decodeNetcdf(NetcdfFile dataFile,
            DataTime dataTime, Headers headers);

    /**
     * Given the npp netcdf file, this method extracts a DataTime object from
     * the data's start/end coverage
     * 
     * @param dataFile
     * @return
     * @throws DecoderException
     */
    private DataTime extractDataTime(NetcdfFile dataFile) throws ParseException {
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_PARSE_STRING);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        Date start = null, end = null;
        for (Attribute attr : dataFile.getRootGroup().getAttributes()) {
            if (START_TIME_ID.equals(attr.getName())) {
                start = sdf.parse(attr.getStringValue());
                if (end != null) {
                    break;
                }
            } else if (END_TIME_ID.equals(attr.getName())) {
                end = sdf.parse(attr.getStringValue());
                if (start != null) {
                    break;
                }
            }
        }

        DataTime dataTime = null;
        if (start != null && end != null) {
            dataTime = new DataTime(start.getTime(), new TimeRange(start, end));
        }
        return dataTime;
    }

}
