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

package com.raytheon.viz.hydrocommon.whfslib;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2008   1649     snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GagePPOptions {
    public shef_dup shef_duplicate;

    public upd_action upd_action;

    public int intpc;

    public int intlppp;

    public int intuppp;

    public float intppq;

    public static enum shef_dup {
        ALWAYS_OVERWRITE, USE_REVCODE, IF_DIFFERENT, IF_DIFFERENT_OR_REVCODE, IF_DIFFERENT_AND_REVCODE
    }

    public static enum upd_action {
        DONT_UPDATE_ACTION, UPDATE_ACTION, IF_DIFFERENT_UPDATE_ACTION
    }

    public static enum precip_rec {
        PRECIP_LID, PRECIP_PE, PRECIP_DUR, PRECIP_TS, PRECIP_EX, PRECIP_OBS, PRECIP_VAL, PRECIP_SHEF, PRECIP_QC, PRECIP_REV, PRECIP_ID, PRECIP_TIME, PRECIP_POST_TIME
    };

    public static enum dbmsResult {
        Ok, errNoDatabaseName, errSqlFail, errCloseFail
    };

    public GagePPOptions() {
    }
}