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
package com.raytheon.edex.plugin.ldad.common;

/**
 * Decoded LDAD data types.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/17/09					dfriedman	Initial creation
 * 
 * </pre>
 * 
 * @author dfriedman
 * @version 1.0
 */

public enum LdadDataType {
    STRING(0),
    DATE_TIME(1),
    SHORT(2),  INT(3),  LONG(4),
    FLOAT(5),  DOUBLE(6);
    
    private int id;
    private LdadDataType(int id) {
    	this.id = id; 
    }
    public boolean isNumeric() {
    	return this != STRING && this != DATE_TIME;
    }
    public static LdadDataType fromId(int id) {
    	for (LdadDataType t : LdadDataType.values())
    		if (t.getId() == id)
    			return t;
    	return null;
    }
	private int getId() {
		return id;
	}
}
