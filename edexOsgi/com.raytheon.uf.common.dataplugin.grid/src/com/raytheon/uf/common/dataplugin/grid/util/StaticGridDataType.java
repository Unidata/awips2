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
package com.raytheon.uf.common.dataplugin.grid.util;

import java.util.ArrayList;
import java.util.List;

/**
 * The static grid data types are the same values for a coverage and can be
 * calculated based off the coverage/model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public enum StaticGridDataType {
    dx, dy, coriolis, _dt;

    private static List<String> stringValues;

    public static List<String> getStringValues() {
        if (stringValues == null) {
            stringValues = new ArrayList<String>(values().length);
            for (StaticGridDataType type : values()) {
                stringValues.add(type.toString());
            }
        }
        return stringValues;
    }
}
