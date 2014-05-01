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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Decoded LDAD XML data structure.
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

@XmlRootElement
public class DecodedData {
	@XmlAttribute
	public String fileName; // original file name
	@XmlAttribute
	public String storageType; // "mesonet", "hydro", etc.
	@XmlAttribute
	public String type; // data type name or "msas_qc.<derived name>"
	@XmlAttribute
	public String root; // original data type name
	@XmlAttribute
	public String source; // same as type?
	@XmlAttribute
	public String provider; // data type name? or "MSAS_QC"
	@XmlAttribute
	public String missingValue;
	@XmlAttribute
	public long reportTime; // Report time in seconds since 1/1/1970
	
	@XmlElement(name="field")
	public List<LdadField> fields = new ArrayList<LdadField>();
	
	
}
