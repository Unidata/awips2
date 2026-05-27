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
package com.raytheon.uf.common.monitor.xml;

import java.util.List;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;

/**
 * Utilities related to supporting the interpretation of the FFMP run config and
 * FFMP source config files.
 * 
 * TODO: move more logic to this class where possible
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2018 6720       njensen     Initial creation
 *
 * </pre>
 *
 * @author njensen
 */

public class FFMPXmlUtils {

    public static String getSourceNameFromDisplayName(String displayName) {
        List<SourceXML> sources = FFMPSourceConfigurationManager.getInstance()
                .getSources();
        for (SourceXML s : sources) {
            if (displayName.equals(s.getDisplayName())) {
                return s.getSourceName();
            }
        }

        return null;
    }

}
