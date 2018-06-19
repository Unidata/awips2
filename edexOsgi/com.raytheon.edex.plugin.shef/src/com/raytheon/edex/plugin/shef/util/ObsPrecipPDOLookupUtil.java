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
package com.raytheon.edex.plugin.shef.util;

import java.io.Serializable;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.RawpcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpother;
import com.raytheon.uf.common.dataplugin.shef.tables.RawpotherId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.dataplugin.shef.tables.RawppId;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;

/**
 * Utility used to identify the potential ihfs data record that the obs_precip
 * stored procedure may insert/update.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2017 6554       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public final class ObsPrecipPDOLookupUtil {

    private ObsPrecipPDOLookupUtil() {
    }

    public static PDOLookupReturn lookupPDO(final String pe, final String locId,
            final ShefData shefData) {
        Class<?> daoClass = null;
        Serializable id = null;
        if (PhysicalElement.PRECIPITATION_ACCUMULATOR.getCode().equals(pe)) {
            daoClass = Rawpc.class;
            RawpcId rawpcId = new RawpcId();
            rawpcId.setLid(locId);
            rawpcId.setTs(shefData.getTypeSource().getCode());
            rawpcId.setExtremum(shefData.getExtremum().getCode());
            rawpcId.setObstime(shefData.getObservationTimeObj());
            id = rawpcId;
        } else if (PhysicalElement.PRECIPITATION_INCREMENT.getCode()
                .equals(pe)) {
            daoClass = Rawpp.class;
            RawppId rawppId = new RawppId();
            rawppId.setLid(locId);
            rawppId.setDur(shefData.getDurationValue());
            rawppId.setTs(shefData.getTypeSource().getCode());
            rawppId.setExtremum(shefData.getExtremum().getCode());
            rawppId.setObstime(shefData.getObservationTimeObj());
            id = rawppId;
        } else {
            daoClass = Rawpother.class;
            RawpotherId rawpotherId = new RawpotherId();
            rawpotherId.setLid(locId);
            rawpotherId.setPe(pe);
            rawpotherId.setDur(shefData.getDurationValue());
            rawpotherId.setTs(shefData.getTypeSource().getCode());
            rawpotherId.setExtremum(shefData.getExtremum().getCode());
            rawpotherId.setObstime(shefData.getObservationTimeObj());
            id = rawpotherId;
        }

        return new PDOLookupReturn(daoClass, id);
    }
}