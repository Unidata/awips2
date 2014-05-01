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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import java.text.DateFormat;
import java.text.Format;
import java.text.MessageFormat;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.props.PropertiesException;

/**
 * Applies the Data Delivery WMO header.
 * 
 * <pre>
 * From Brian Gockel:
 * 
 * The general form will be:
 *   LZ$$# KWBC 
 * where "$$#" is two character ($$) variables and one numeric (#) variable, 
 * so we have three characters with which to work: $, $, and #.
 * $ and $ can be assigned from A-Z, while # can be assigned from 1-6.
 * 
 * The first $ is the data-provider indicator, with values (so far):
 * 
 *   A = NOMADS
 *   B = MADIS
 *   C = PDA
 * 
 * 
 * The second $ is the data-format indicator (so far):
 * 
 *   A = binary grid (OPENDAP)
 *   B = <TBD point-data format, such as from MADIS>
 *   C = netCDF4
 * 
 * 
 * and the # is the general data type (so far):
 * 
 *   1 = model
 *   2 = observation
 *   3 = satellite 
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 06, 2013 1822       bgonzale     Initial creation
 * Oct 01, 2013 2267       bgonzale     Pass request parameter instead of components of request.
 *                                      Fixed ordering of elements in maps and defaults when
 *                                      element keys are null.
 * Oct 09, 2013 2267       bgonzale     Fix Wmo header cr and lf formatting.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class DataDeliveryRetrievalWmoHeaderApplier implements IWmoHeaderApplier {

    private final MessageFormat wmoHeaderMessage;
    
    private final Map<String, String> dataProviderMap;

    private final Map<String, String> dataFormatMap;

    private final Map<String, String> dataSourceMap;

    public DataDeliveryRetrievalWmoHeaderApplier(String wmoHeader,
            String dataProviderMapping, String dataFormatMapping,
            String dataSourceMapping) throws PropertiesException {
        this.wmoHeaderMessage = new MessageFormat(wmoHeader);
        // ensure any DateFormats in the given format are GMT
        for (Format format : this.wmoHeaderMessage.getFormats()) {
            if (format instanceof DateFormat) {
                ((DateFormat) format).setTimeZone(TimeUtil.GMT_TIME_ZONE);
            }
        }
        this.dataProviderMap = parseMapping(dataProviderMapping);
        this.dataFormatMap = parseMapping(dataFormatMapping);
        this.dataSourceMap = parseMapping(dataSourceMapping);
    }

    private static Map<String, String> parseMapping(String dataMapping)
            throws PropertiesException {
        String[] elementPairs = dataMapping.split(",");
        Map<String, String> resultMap = new LinkedHashMap<String, String>(
                elementPairs.length, 1);

        for (String elementPair : elementPairs) {
            String[] pair = elementPair.split(":");

            if (pair.length == 2) {
                String key = pair[0].toUpperCase();
                String value = pair[1].toUpperCase();

                resultMap.put(key, value);
            } else {
                throw new PropertiesException(
                        "Failed to Parse WMO Configuration Properties Mapping from: "
                                + dataMapping);
            }
        }
        return resultMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String applyWmoHeader(String dataProvider, String dataFormat,
            String sourceType, Date date, String data) {
        final Object formatArgs = createMessageFormatArgs(dataProvider,
                dataFormat, sourceType, date);
        final String formattedWmoHeader = wmoHeaderMessage.format(formatArgs);
        return formattedWmoHeader + data;
    }

    private Object createMessageFormatArgs(String dataProvider,
            String dataFormat, String sourceType, Date date) {
        final String provider = getElement(dataProvider, this.dataProviderMap);
        final String format = getElement(dataFormat, this.dataFormatMap);
        final String source = getElement(sourceType, this.dataSourceMap);
        return new Object[] { provider, format, source, date };
    }

    /*
     * if no element found, default to first element, or if empty, default to
     * null.
     */
    private static String getElement(String elementKey,
            Map<String, String> mapping) {
        String resultKey = null;
        
        for (String key : mapping.keySet()) {
            if (resultKey == null) {
                resultKey = key;
                if (elementKey == null) {
                    break;
                }
            }
            if (key.startsWith(elementKey)) {
                resultKey = key;
                break;
            }
        }
        return mapping.get(resultKey);
    }

}
