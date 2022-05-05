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
package com.raytheon.viz.texteditor.qc;

import org.apache.commons.lang3.StringUtils;

/**
 * Provides success or failure status for TextWS quality control (QC) checks.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2017  #6251     dgilling     Initial creation
 *
 * </pre>
 *
 * @author dgilling
 */

public final class QcCheckResponse {

    public static final QcCheckResponse DEFAULT_RESPONSE = new QcCheckResponse(
            true, StringUtils.EMPTY);

    private final boolean checksPassed;

    private final String message;

    public QcCheckResponse(boolean checksPassed, String message) {
        this.checksPassed = checksPassed;
        this.message = message;
    }

    public boolean isChecksPassed() {
        return checksPassed;
    }

    public String getMessage() {
        return message;
    }
}
