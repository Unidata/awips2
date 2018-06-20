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
package com.raytheon.uf.edex.plugin.mpe;

import java.util.Map;
import java.util.HashMap;

/**
 * Enumeration representative of the recognized hrap grid factors. Includes a
 * link back to the legacy numeric identifiers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public enum HrapGridFactor {
    FULL(1), QUARTER(4);

    private static Map<Integer, HrapGridFactor> hrapGridFactorNumLookupMap;
    
    public static final String LOOKUP_NUM_1 = "1";
    
    public static final String LOOKUP_NUM_4 = "4";

    private final int num;

    private HrapGridFactor(final int num) {
        this.num = num;
    }

    public static synchronized HrapGridFactor lookupGridFactorByNum(
            final Integer lookupNum) {
        if (lookupNum == null) {
            return null;
        }
        if (hrapGridFactorNumLookupMap == null) {
            hrapGridFactorNumLookupMap = new HashMap<>(
                    HrapGridFactor.values().length, 1.0f);
            for (HrapGridFactor hrapGridFactor : HrapGridFactor.values()) {
                hrapGridFactorNumLookupMap.put(hrapGridFactor.getNum(),
                        hrapGridFactor);
            }
        }

        return hrapGridFactorNumLookupMap.get(lookupNum);
    }

    public int getNum() {
        return num;
    }
}