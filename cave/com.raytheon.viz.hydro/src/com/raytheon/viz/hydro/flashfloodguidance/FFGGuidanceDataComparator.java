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
package com.raytheon.viz.hydro.flashfloodguidance;

import java.util.Comparator;

import org.apache.commons.lang.builder.CompareToBuilder;

/**
 * Compares {@link FFGGuidanceData}s to ensure that they are supported by: 1)
 * identifier in ascending 2) duration in ascending 3) date/time in descending.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2016 5483       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class FFGGuidanceDataComparator implements Comparator<FFGGuidanceData> {

    public FFGGuidanceDataComparator() {
    }

    @Override
    public int compare(FFGGuidanceData o1, FFGGuidanceData o2) {
        return new CompareToBuilder()
                .append(o1.getIdentifier(), o2.getIdentifier())
                .append(o1.getDuration(), o2.getDuration())
                .append(o2.getDateTime(), o1.getDateTime()).toComparison();
    }
}