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
package com.raytheon.uf.edex.ohd.satpre.handler;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.mpe.PrecipRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.mpe.fieldgen.PrecipDataKey;
import com.raytheon.uf.common.mpe.fieldgen.PrecipDataRequest;
import com.raytheon.uf.common.mpe.fieldgen.PrecipDataResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.ohd.satpre.PrecipDataException;
import com.raytheon.uf.edex.plugin.mpe.dao.metadata.impl.PrecipDao;

/**
 * Handles MPE precipitation data and inventory requests.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class PrecipDataHandler implements IRequestHandler<PrecipDataRequest> {

    @Override
    public Object handleRequest(PrecipDataRequest request) throws Exception {
        return retrieveData(request.getPrecipDataKey());
    }

    private PrecipDataResponse retrieveData(final PrecipDataKey precipDataKey)
            throws PrecipDataException {
        PrecipDao precipDao;
        try {
            precipDao = new PrecipDao();
        } catch (PluginException e) {
            throw new PrecipDataException("Failed to instantiate a Precip Dao.",
                    e);
        }
        PrecipRecord precipRecord = null;
        try {
            precipRecord = precipDao.getLatestRecordForHour(
                    precipDataKey.getField(), precipDataKey.getDate(),
                    precipDataKey.getHour());
        } catch (PluginException e) {
            throw new PrecipDataException(
                    "Failed to retrieve the latest precip data for: "
                            + precipDataKey.toString(),
                    e);
        }
        if (precipRecord == null) {
            return new PrecipDataResponse();
        }
        return new PrecipDataResponse(precipRecord.getHrapX(),
                precipRecord.getHrapY(), precipRecord.getHrapWidth(),
                precipRecord.getHrapHeight(),
                ((ShortDataRecord) precipRecord.getMessageData())
                        .getShortData());
    }
}