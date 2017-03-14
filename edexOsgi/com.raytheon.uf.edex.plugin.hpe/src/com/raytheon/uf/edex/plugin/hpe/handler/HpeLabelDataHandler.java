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
package com.raytheon.uf.edex.plugin.hpe.handler;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.plugin.hpe.request.HpeLabelDataRequest;
import com.raytheon.uf.common.plugin.hpe.request.HpeLabelDataResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.hpe.util.HpeDataAccessor;
import com.raytheon.uf.edex.plugin.hpe.util.HpeLabelGenerator;

/**
 * HPE bias source label request handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2014    3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeLabelDataHandler implements
        IRequestHandler<HpeLabelDataRequest> {
    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HpeLabelDataHandler.class);

    @Override
    public Object handleRequest(HpeLabelDataRequest request) throws Exception {
        return getData(request);
    }

    private HpeLabelDataResponse getData(HpeLabelDataRequest request) {
        // Generate the label string, put in response and send back
        HpeLabelDataResponse response = new HpeLabelDataResponse();
        Map<Date, String> labelMap = new HashMap<Date, String>();
        try {
            for (Date d : request.getDateList()) {
                String label = generateHpeLabel(request.getProductName(), d);
                labelMap.put(d, label);
            }

            response.setData(labelMap);
        } catch (Exception e) {
            statusHandler.error(e.getMessage(), e);
        }
        return response;
    }

    private String generateHpeLabel(String productName, Date date)
            throws Exception {
        HpeDataAccessor dataAccessor = new HpeDataAccessor();
        HpeLabelGenerator generator = new HpeLabelGenerator(dataAccessor);
        String text = generator.getHpeLabel(date, productName);
        return text;
    }
}
