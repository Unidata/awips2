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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EdexModeTest {

    @Test
    public void testExcludedPatternByIncludedModeIsExcluded() {
        
        /* EdexMode(List<String> includeList, List<String> excludeList,
                ArrayList<String> arrayList) */
        
        List<String> includeList = new ArrayList<String>();
        includeList.add(".*include.*");
        List<String> excludeList = new ArrayList<String>();
        excludeList.add(".*exclude.*");
        ArrayList<String> emptyList = new ArrayList<String>();
        
        EdexMode includedMode = new EdexMode(includeList,
                                        excludeList, emptyList);

        List<String> someotherList = new ArrayList<String>();
        someotherList.add(".*someother.*");
        List<String> excludeEmptyList = new ArrayList<String>();
        ArrayList<String> modeList = new ArrayList<String>();
        //TODO HERE THIS WILL NOT WORK modeList.add(includedMode);
        EdexMode mode = new EdexMode(someotherList, excludeEmptyList,
                modeList);
        
        Map<String, EdexMode> allModes = new HashMap<String, EdexMode>();
        try {
            mode.init(allModes);
        }
        catch(Exception ex) {
            ex.printStackTrace();
        }

        File file = new File("./res/spring/exclude.xml");
        assertFalse(mode.accept(file, file.getAbsolutePath()));
    }

    @Test
    public void testIncludedPatternByIncludedModeCanBeExcluded() {
       /* EdexMode includedMode = new EdexMode(Arrays.asList(".*include.*"),
                Collections.<String> emptyList(),
                Collections.<EdexMode> emptyList());*/
        List<String> includeList = new ArrayList<String>();
        includeList.add(".*include.*");
        List<String> excludeList = new ArrayList<String>();
        ArrayList<String> emptyList = new ArrayList<String>();
        
        EdexMode includedMode = new EdexMode(includeList,
                                        excludeList, emptyList);
        
/* TODO HERE REINSTATE
        EdexMode mode = new EdexMode(Collections.<String> emptyList(),
                Arrays.asList(".*include.*"), Arrays.asList(includedMode));
        mode.init();

        File file = new File("./res/spring/include.xml");
        assertFalse(mode.accept(file, file.getAbsolutePath()));
        */
    }
}
