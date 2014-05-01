package com.sun.jna;

public class PlatformEx
{
	private static boolean		winVista	= false;

	static
	{
		String osName = System.getProperty("os.name").toLowerCase();
		if (osName.startsWith("windows"))
		{
			winVista = osName.contains("vista") || osName.contains(" 7") || osName.contains("2008") || osName.contains(" 8");
		}
	}

	public static boolean isWinVista()
	{
		return winVista;
	}

}
