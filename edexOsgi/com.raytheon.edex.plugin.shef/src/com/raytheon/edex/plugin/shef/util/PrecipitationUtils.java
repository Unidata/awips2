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

import java.util.Date;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.data.precip.PrecipRecord;
import com.raytheon.edex.plugin.shef.data.precip.PrecipRecordStorage;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * Precipitation Utilities
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/02/2008	387        M. Duff     Initial Creation.
 * 10/16/2008   1548       jelkins     Integrated ParameterCode Types
 * 
 * </pre>
 * 
 * @author mduff
 * @version 1.0
 */

public class PrecipitationUtils {

    private static final Log log = LogFactory.getLog(PrecipitationUtils.class);

    /**
     * Returns the precipitation index.
     * 
     * @param pe -
     *            the physical element
     * @return the index (0, 1, 2, or 3)
     */
    public static int getPrecipitationIndex(PhysicalElement pe) {
        int index = ShefConstants.NOT_PRECIP;

        if (pe == PhysicalElement.PRECIPITATION_ACCUMULATOR) {
            index = ShefConstants.RAWPC;
        } else if (pe == PhysicalElement.PRECIPITATION_INCREMENT) {
            index = ShefConstants.RAWPP;
        } else if (pe == PhysicalElement.PRECIPITATION_MEASURABLE_PROBABILITY
                || pe == PhysicalElement.PRECIPITATION_NORMAL
                || pe == PhysicalElement.PRECIPITATION_RATE
                || pe == PhysicalElement.PRECIPITATION_TYPE) {
            index = ShefConstants.RAWPOTHER;
        }
        return index;
    }

    /**
     * Write a precipitation record to a persistent store.
     * 
     * @param shefData
     * @param dataType
     * @param dataValue
     * @param shefRecord
     * @param qualifier
     * @param locId
     * @param productTime
     * @param productId
     * @param qualityCode
     * @param postTime
     */
    public static void writePrecipGpp(ShefData shefData, ShefRecord shefRecord,
            long qualityCode, String productId, Date productTime,
            Date postTime, String locationId, String qualifier, String value) {

        PrecipRecordStorage.getStorage().incrementRecordCount();

        if(log.isDebugEnabled()) {
            
            PrecipRecord record = new PrecipRecord(shefData);
            record.setProductId(productId);
            record.setProductTime(productTime);
            record.setPostingTime(postTime);
            record.setQualCode(qualityCode);

            log.debug("Record added to storage: " + record);
        }
        log.info("PrecipRecords in storage:"
                + PrecipRecordStorage.getStorage().getRecordCount());
    }

    public static boolean checkPrecipWindow(String pe, Date obsTime) {
        boolean withinWindow = false;

        return withinWindow;
    }
}
