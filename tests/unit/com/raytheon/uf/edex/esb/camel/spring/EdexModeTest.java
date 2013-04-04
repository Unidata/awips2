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
package com.raytheon.uf.edex.esb.camel.spring;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;

import org.junit.Test;


/**
 * Test {@link EdexMode}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2012 1187       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EdexModeTest {

    @Test
    public void testExcludedPatternByIncludedModeIsExcluded() {
        EdexMode includedMode = new EdexMode(Arrays.asList(".*include.*"),
                Arrays.asList(".*exclude.*"),
                Collections.<EdexMode> emptyList());

        EdexMode mode = new EdexMode(Arrays.asList(".*someother.*"),
                Collections.<String> emptyList(), Arrays.asList(includedMode));
        mode.init();

        File file = new File("./res/spring/exclude.xml");
        assertFalse(mode.accept(file, file.getAbsolutePath()));
    }

    @Test
    public void testIncludedPatternByIncludedModeCanBeExcluded() {
        EdexMode includedMode = new EdexMode(Arrays.asList(".*include.*"),
                Collections.<String> emptyList(),
                Collections.<EdexMode> emptyList());

        EdexMode mode = new EdexMode(Collections.<String> emptyList(),
                Arrays.asList(".*include.*"), Arrays.asList(includedMode));
        mode.init();

        File file = new File("./res/spring/include.xml");
        assertFalse(mode.accept(file, file.getAbsolutePath()));
    }
}
