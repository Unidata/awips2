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
package com.raytheon.uf.edex.security;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.eclipse.jetty.util.security.Password;

/**
 * 
 * Class used with the WSS4j interceptors. This class extends the java
 * Properties class to allow obfuscated properties to be contained in the
 * properties file. The properties may be obfuscated using Jetty's obfuscation
 * methods. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 6/5/2014     1712        bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 * @see org.eclipse.jetty.util.security.Password.obfuscate(String)
 * @see org.eclipse.jetty.util.security.Password.deobfuscate(String)
 **/
public class EncryptedProperties extends Properties {

	private static final long serialVersionUID = -8799654229761166379L;

	/** The prefix prepended to an obfuscated property */
	private static final String OBFUSCATED_PREFIX = "OBF:";

	/**
	 * Creates a new EncryptedProperties object
	 * 
	 * @param filename
	 *            The file containing the properties
	 * @throws IOException
	 *             If errors occur while reading the properties file
	 */
	public EncryptedProperties(String filename) throws IOException {
		FileInputStream fis = null;
		try {
			fis = new FileInputStream(filename);
			load(fis);
		} finally {
			if (fis != null) {
				fis.close();
			}
		}
	}
	
	public String getProperty(String propertyName){
		String property = super.getProperty(propertyName);
		if (property != null
				&& property.startsWith(OBFUSCATED_PREFIX)) {
			return Password.deobfuscate(property);
		}
		return property;
	}
}
