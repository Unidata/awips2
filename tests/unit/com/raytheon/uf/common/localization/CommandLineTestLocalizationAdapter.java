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
package com.raytheon.uf.common.localization;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.localization.TestPathManager.TestLocalizationAdapter;

/**
 * {@link ILocalizationAdapter} implementation for running tests on the
 * command-line.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class CommandLineTestLocalizationAdapter extends TestLocalizationAdapter {

    /**
     * @param site
     * @param savedLocalizationFileDir
     */
    public CommandLineTestLocalizationAdapter(String site,
            File savedLocalizationFileDir) {
        super(site, savedLocalizationFileDir);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    List<File> getDirectoriesWithPlugins() {
        return Arrays.asList(new File(".."));
    }
}