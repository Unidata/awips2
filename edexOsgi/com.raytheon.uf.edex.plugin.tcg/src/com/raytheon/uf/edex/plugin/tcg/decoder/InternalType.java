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
package com.raytheon.uf.edex.plugin.tcg.decoder;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            jsanchez     Initial creation
 *
 * </pre>
 *
 * @author jsanchez
 * @version 1.0	
 */

public enum InternalType {
    //CHGHUR
    PRODUCT,
    DATA_INFO,
    DATETIME_INFO,
    MODEL_INFO,
    //CHGQLM
    STORM_TYPE_INFO,
    
    FORECAST_POSITION_INFO,
    STORM_DISSIPATED,
    //TCE
    TCE_REFHOUR,
    STATIONID,  
    LATITUDE,
    LONGITUDE,
    
    //COMMON
    INIT_TIME_INFO,
    END;
}
