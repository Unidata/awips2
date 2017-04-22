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
package com.raytheon.uf.edex.core.modes;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

import org.junit.Test;

import com.raytheon.uf.edex.core.modes.EdexMode;


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
 * Sep 04, 2014 3365       ccody        Changes for removing Data_Delivery dependencies
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EdexModeTest {

    @Test
    public void testExcludedPatternByIncludedModeIsExcluded() {
        
        List<String> includeList = Arrays.asList(".*include.*");
        List<String> excludeList =  Arrays.asList(".*exclude.*");
        List<String> emptyList = new ArrayList<String>();
        EdexMode includedMode = new EdexMode(
                                        includeList,
                                        excludeList,
                                        emptyList);
      /*  (List<String> includeList, List<String> excludeList,
                List<String> includedModes) */
        List<String> someotherList = Arrays.asList(".*someother.*");
        List<String> includedModeList =  new ArrayList<String>();
        includedModeList.add(includedMode.getName());
        Arrays.asList(".*exclude.*");
        
        EdexMode mode = new EdexMode(someotherList,
                emptyList, includedModeList);
        Map<String, EdexMode> allModes = new HashMap<String, EdexMode>();
        try {
            mode.init(allModes);
        }
        catch(ModesException me) {
            me.printStackTrace();
            fail();
        }

        File file = new File("./res/spring/exclude.xml");
        assertFalse(mode.accept(file, file.getAbsolutePath()));
    }

    @Test
    public void testIncludedPatternByIncludedModeCanBeExcluded() {
        List<String> includeList = Arrays.asList(".*include.*");
        List<String> emptyList = new ArrayList<String>();

        EdexMode includedMode = new EdexMode(includeList,
                emptyList,
                emptyList);

        List<String> includedModeList =  new ArrayList<String>();
        includedModeList.add(includedMode.getName());
        
        EdexMode mode = new EdexMode(emptyList,
                includeList, includedModeList);
        
        
        Map<String, EdexMode> allModes = new HashMap<String, EdexMode>();
        try {
            mode.init(allModes);
        }
        catch(ModesException me) {
            me.printStackTrace();
            fail();
        }
        File file = new File("./res/spring/include.xml");
        assertFalse(mode.accept(file, file.getAbsolutePath()));
    }
}
