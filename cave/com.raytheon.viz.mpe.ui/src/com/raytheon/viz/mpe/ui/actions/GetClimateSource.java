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
package com.raytheon.viz.mpe.ui.actions;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GetClimateSource {

    static String previousClimateTypeSource;

    static String previousClimateSourceName;

    static String[] pClimateTypeSources = { "PB", "RZ" };

    static String[] pClimateSourceNames = { "PRISM", "NCDC", "Unknown" };

    public String getClimateSource(String cparm) {

        int i;
        boolean status;

        if (previousClimateTypeSource != null
                && previousClimateSourceName != null) {
            status = (previousClimateTypeSource.equalsIgnoreCase(cparm
                    .substring(3, 5)));

            if (status == true) {
                return previousClimateSourceName;
            }
        }

        for (i = 0; i < 2; ++i) {
            status = (cparm.substring(3, 5)
                    .equalsIgnoreCase(pClimateTypeSources[i]));

            if (status == true) {
                previousClimateTypeSource = pClimateTypeSources[i];
                previousClimateSourceName = pClimateSourceNames[i];
                return previousClimateSourceName;
            }
        }

        previousClimateTypeSource = null;
        previousClimateSourceName = null;

        return pClimateSourceNames[2];

    }
}
