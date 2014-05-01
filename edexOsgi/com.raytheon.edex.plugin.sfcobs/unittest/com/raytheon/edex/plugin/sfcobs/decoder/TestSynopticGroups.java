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
package com.raytheon.edex.plugin.sfcobs.decoder;

import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticGroups;

import junit.framework.TestCase;

public class TestSynopticGroups extends TestCase {

    private static final Double epsilon = 0.0001D;
    
    public void testSynopticTemperature_1() {
        
        String [] groupInput = {
            "00000", "01000","00100", "01100","00999", "01999",
            "10000", "11000","10100", "11100","10999", "11999",
            "20000", "21000","20100", "21100","20999", "21999",
            "80000", "81000","80100", "81100","80999", "81999",
            "10000", "11000","10100", "11100","10999", "11999",
            "20000", "21000","20100", "21100","20999", "21999",
        };
        
        int [] lookingForGroup = {
            2,2,2,2,2,2,
            1,1,1,1,1,1,
            1,1,1,1,1,1,
            3,3,3,3,3,3,
            3,3,3,3,3,3,
            3,3,3,3,3,3,
        };
        
        String [] groupNames = {
            "seaSfcTemp","seaSfcTemp","seaSfcTemp","seaSfcTemp","seaSfcTemp","seaSfcTemp",
            "airTemp", "airTemp","airTemp", "airTemp","airTemp", "airTemp",
            "dewTemp", "dewTemp","dewTemp", "dewTemp","dewTemp", "dewTemp",
            "wetBulbTemp", "wetBulbTemp","wetBulbTemp", "wetBulbTemp","wetBulbTemp", "wetBulbTemp",
            "maxTemp", "maxTemp","maxTemp", "maxTemp","maxTemp", "maxTemp",
            "minTemp", "minTemp","minTemp", "minTemp","minTemp", "minTemp",
        };
        
        Double [] groupData = {
            273.13, 273.13, 283.13, 263.13,373.03, 173.23,
            273.13, 273.13, 283.13, 263.13,373.03, 173.23,
            273.13, 273.13, 283.13, 263.13,373.03, 173.23,
            273.13, 273.13, 283.13, 263.13,373.03, 173.23,
            273.13, 273.13, 283.13, 263.13,373.03, 173.23,
            273.13, 273.13, 283.13, 263.13,373.03, 173.23,
        };
        
        for(int i = 0;i < groupInput.length;i++) {
            DataItem di = SynopticGroups.decodeTemperature(groupInput[i],lookingForGroup[i]);
            
            assertNotNull("Null item, group["+i+"]",di);
            assertNotNull("Null item, group["+i+"]",di.getDataValue());
            assertNotNull("Null item, group["+i+"]",di.getDataName());
            
            assertEquals(groupData[i],di.getDataValue(),epsilon);
            
            assertEquals(groupNames[i],di.getDataName());
            // These data should have no period decoded.
            assertNull("Null item, group["+i+"]",di.getDataPeriod());
        }
    }
    
    /**
     * 
     */
    public void testRelativeHumidity_1() {
        
        String [] groupInput = {"29000", "29050","29100", };
        
        int [] lookingForGroup = {1,1,1, };
        
        String [] groupNames = {"relHum", "relHum","relHum", };
        
        Double [] groupData = {0.0, 50.0,100.0, };
        
        for(int i = 0;i < groupInput.length;i++) {
            // sending rh data to the temperature decoder must return null!
            DataItem di = SynopticGroups.decodeTemperature(groupInput[i],lookingForGroup[i]);
            assertNull("Null item, group["+i+"]",di);
            // Now send to the rh decoder.
            di = SynopticGroups.decodeRelativeHumidity(groupInput[i],lookingForGroup[i]);
            assertNotNull("Null item, group["+i+"]",di);
            
            assertNotNull("Null item, group["+i+"]",di.getDataValue());
            assertNotNull("Null item, group["+i+"]",di.getDataName());
            
            assertEquals(groupData[i],di.getDataValue(),epsilon);
            
            assertEquals(groupNames[i],di.getDataName());
        }
    }
}
