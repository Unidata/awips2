/**
 * 
 */
package com.raytheon.wes2bridge.datalink;

/**
 * @author bkowal
 *
 * This is an extremely simple utility. This program
 * expects on argument: a postgresql database directory.
 * This utility will then extract the "/awips2/data" and
 * return only the name of the database directory.
 */
public class DataLinkUtility 
{
	private static final String AWIPSII_DATA = "/awips2/data/";
	
	/**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		if (args.length != 1)
		{
			System.out.print("Error: The name of the data directory must be specified.");
			System.exit(-1);
		}
		
		String dataDirectory = args[0];
		System.out.print(dataDirectory.replace(AWIPSII_DATA, ""));
		
		System.exit(0);
	}
}