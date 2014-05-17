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
package com.raytheon.rcm.config.std;

import java.io.File;
import java.io.FileNotFoundException;

public class ConfigRes {
	private static final String propBase = "com.raytheon.rcm";

	private File configDirectory;

	public ConfigRes() {
		String prop = System.getProperty(propBase + ".configDir", "/");
		configDirectory = new File(prop);
	}

	public File getDropInDir() {
		return new File(configDirectory, "drop-ins");
	}

	public File getDropInPath(String resName) {
		return new File(getDropInDir(), resName);
	}

	public File getPrivatePath(String resName) {
		return new File(getPrivateDir(), resName);
	}

	public File getPrivateDir() {
		return new File(configDirectory, "persist");
	}

	public File getNdmFile(String resName) throws FileNotFoundException {
		File f = getDropInPath(resName);
		if (f.isFile())
			return f;
		else
			throw new FileNotFoundException(f.toString());
	}
}
