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
package com.raytheon.edex.plugin.obs.metar;

import static org.junit.Assert.*;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.jscience.physics.amount.Amount;
import org.junit.Test;

/**
 * Test code to demonstrate usage.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  20071217           472  jkorman     Initial coding.    
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class TestMetarRecordIGettable {

    @Test
    public void testGettable() {
        MetarRecord report = new MetarRecord();
        
        report.setTemperature(20);
        report.setDewPoint(18);
        report.setWindDir("235");
        report.setWindSpeed(17);
        report.setWindGust(23);
        report.setAltimeterInPa(101613);
        
        
        Amount<?> amt = report.getValue("T");
        assertNotNull(amt);
        assertEquals(amt.getEstimatedValue(),20.0);
        Unit<?> unit = amt.getUnit();
        assertEquals(unit,SI.CELSIUS);

        amt = report.getValue("DpT");
        assertNotNull(amt);
        assertEquals(amt.getEstimatedValue(),18.0);
        unit = amt.getUnit();
        assertEquals(unit,SI.CELSIUS);
        
        amt = report.getValue("WS");
        assertNotNull(amt);
        assertEquals(amt.getEstimatedValue(),17.0);
        unit = amt.getUnit();
        assertEquals(unit,NonSI.KNOT);
        
        amt = report.getValue("WGS");
        assertNotNull(amt);
        assertEquals(amt.getEstimatedValue(),23.0);
        unit = amt.getUnit();
        assertEquals(unit,NonSI.KNOT);
        
        amt = report.getValue("WD");
        assertNotNull(amt);
        assertEquals(amt.getEstimatedValue(),235);
        unit = amt.getUnit();
        assertEquals(unit,NonSI.DEGREE_ANGLE);
        
        amt = report.getValue("ASET");
        assertNotNull(amt);
        assertEquals(101613, amt.getEstimatedValue(),0.01);
        unit = amt.getUnit();
        assertEquals(unit,SI.PASCAL);
    }
}
