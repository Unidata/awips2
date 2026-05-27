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
import java.util.Date;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcstdischarge;
import com.raytheon.uf.common.dataplugin.shef.tables.FcstdischargeId;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.dataplugin.shef.tables.FcstheightId;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcstother;
import com.raytheon.uf.common.dataplugin.shef.tables.FcstotherId;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcstprecip;
import com.raytheon.uf.common.dataplugin.shef.tables.FcstprecipId;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcsttemperature;
import com.raytheon.uf.common.dataplugin.shef.tables.FcsttemperatureId;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElementCategory;

/**
 * Utility used to identify the potential ihfs data record that the fcst_pe
 * stored procedure may insert/update.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2017 6554       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public final class FcstPEPDOLookupUtil {

    private FcstPEPDOLookupUtil() {
    }

    public static PDOLookupReturn lookupPDO(final String pe, final String locId,
            final ShefData shefData, final Date validTime) {
        if (pe == null) {
            return null;
        }
        Class<?> daoClass = null;
        Serializable id = null;
        final String peFirstChar = pe.substring(0, 1);
        final float probability = new Double(
                shefData.getProbability().getValue()).floatValue();
        Date basisTime = shefData.getCreationDateObj();
        if (basisTime == null) {
            basisTime = shefData.getObservationTimeObj();
        }
        if (PhysicalElementCategory.HEIGHT.getCode().equals(peFirstChar)) {
            daoClass = Fcstheight.class;
            FcstheightId fcstheightId = new FcstheightId();
            fcstheightId.setLid(locId);
            fcstheightId.setPe(pe);
            fcstheightId.setDur(shefData.getDurationValue());
            fcstheightId.setTs(shefData.getTypeSource().getCode());
            fcstheightId.setExtremum(shefData.getExtremum().getCode());
            fcstheightId.setProbability(probability);
            fcstheightId.setValidtime(validTime);
            fcstheightId.setBasistime(basisTime);
            id = fcstheightId;
        } else if (PhysicalElementCategory.PRECIPITATION.getCode()
                .equals(peFirstChar)) {
            daoClass = Fcstprecip.class;
            FcstprecipId fcstprecipId = new FcstprecipId();
            fcstprecipId.setLid(locId);
            fcstprecipId.setPe(pe);
            fcstprecipId.setDur(shefData.getDurationValue());
            fcstprecipId.setTs(shefData.getTypeSource().getCode());
            fcstprecipId.setExtremum(shefData.getExtremum().getCode());
            fcstprecipId.setProbability(probability);
            fcstprecipId.setValidtime(validTime);
            fcstprecipId.setBasistime(basisTime);
            id = fcstprecipId;
        } else if (PhysicalElementCategory.DISCHARGE.getCode()
                .equals(peFirstChar)) {
            daoClass = Fcstdischarge.class;
            FcstdischargeId fcstdischargeId = new FcstdischargeId();
            fcstdischargeId.setLid(locId);
            fcstdischargeId.setPe(pe);
            fcstdischargeId.setDur(shefData.getDurationValue());
            fcstdischargeId.setTs(shefData.getTypeSource().getCode());
            fcstdischargeId.setExtremum(shefData.getExtremum().getCode());
            fcstdischargeId.setProbability(probability);
            fcstdischargeId.setValidtime(validTime);
            fcstdischargeId.setBasistime(basisTime);
            id = fcstdischargeId;
        } else if (PhysicalElementCategory.TEMPERATURE.getCode()
                .equals(peFirstChar)) {
            daoClass = Fcsttemperature.class;
            FcsttemperatureId fcsttemperatureId = new FcsttemperatureId();
            fcsttemperatureId.setPe(pe);
            fcsttemperatureId.setDur(shefData.getDurationValue());
            fcsttemperatureId.setTs(shefData.getTypeSource().getCode());
            fcsttemperatureId.setExtremum(shefData.getExtremum().getCode());
            fcsttemperatureId.setProbability(probability);
            fcsttemperatureId.setValidtime(validTime);
            fcsttemperatureId.setBasistime(basisTime);
            id = fcsttemperatureId;
        } else {
            daoClass = Fcstother.class;
            FcstotherId fcstotherId = new FcstotherId();
            fcstotherId.setPe(pe);
            fcstotherId.setDur(shefData.getDurationValue());
            fcstotherId.setTs(shefData.getTypeSource().getCode());
            fcstotherId.setExtremum(shefData.getExtremum().getCode());
            fcstotherId.setProbability(probability);
            fcstotherId.setValidtime(validTime);
            fcstotherId.setBasistime(basisTime);
            id = fcstotherId;
        }
        return new PDOLookupReturn(daoClass, id);
    }
}