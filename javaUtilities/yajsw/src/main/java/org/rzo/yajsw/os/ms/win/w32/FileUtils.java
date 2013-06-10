/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.os.ms.win.w32;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.logging.Logger;

import org.apache.commons.io.filefilter.WildcardFileFilter;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.platform.win32.WinNT;

// TODO: Auto-generated Javadoc
/**
 * The Class FileUtils.
 */
public class FileUtils
{
	static Logger	log	= Logger.getLogger(FileUtils.class.getName());

	public static long created(File file)
	{

		throw new UnsupportedOperationException("Sorry, not implemented in this verson");
		// long result = -1;
		// SYSTEMTIME sysTime = new SYSTEMTIME();
		// sysTime.size();
		// Pointer hFile =
		// MyKernel32.INSTANCE.CreateFile(file.getAbsolutePath(),
		// MyKernel32..GENERIC_READ, 0, null, MyKernel32.OPEN_EXISTING, 0,
		// null);
		// if (hFile != null)
		// {
		// LongByReference created = new LongByReference();
		// if (MyKernel32.INSTANCE.GetFileTime(hFile, created, null, null))
		// {
		// // if (MyKernel32.INSTANCE.FileTimeToSystemTime(created, sysTime))
		// // {
		// // Calendar c = Calendar.getInstance();
		// // c.setTimeZone(TimeZone.getTimeZone("UTC"));
		// // c.set(sysTime.wYear, sysTime.wMonth-1, sysTime.wDay,
		// sysTime.wHour, sysTime.wMinute, sysTime.wSecond);
		// // result = c.getTimeInMillis();
		// // System.out.println(created.getValue() + " "+ result);
		// // }
		// // found in /org/apache/poi/hpsf/Util.java
		// final long ms_since_16010101 = created.getValue() / (1000 * 10);
		// final long ms_since_19700101 = ms_since_16010101 - EPOCH_DIFF;
		// result = ms_since_19700101;
		// }
		//			
		// MyKernel32.INSTANCE.CloseHandle(hFile);
		//			
		// }
		//		
		//		
		// return result;
	}

	public static long freeSpace(File file)
	{
		if (!file.isDirectory() || !file.exists())
			return -2;

		WinNT.LARGE_INTEGER.ByReference lpTotalNumberOfFreeBytes = new WinNT.LARGE_INTEGER.ByReference();
		lpTotalNumberOfFreeBytes.clear();

		boolean ret = Kernel32.INSTANCE.GetDiskFreeSpaceEx(file.getPath(), null, null, lpTotalNumberOfFreeBytes);
		if (ret)
			return lpTotalNumberOfFreeBytes.getValue();
		else
		{
			String s = Kernel32Util.formatMessageFromLastErrorCode(Kernel32.INSTANCE.GetLastError());
			log.severe("error in File.freeSpace getting for \"" + file.getPath() + "\" " + s);
		}

		return -1;
	}

	public static long totalSpace(File file)
	{
		if (!file.isDirectory() || !file.exists())
			return -2;

		WinNT.LARGE_INTEGER.ByReference lpTotalNumberOfBytes = new WinNT.LARGE_INTEGER.ByReference();
		lpTotalNumberOfBytes.clear();

		boolean ret = Kernel32.INSTANCE.GetDiskFreeSpaceEx(file.getPath(), null, lpTotalNumberOfBytes, null);
		if (ret)
			return lpTotalNumberOfBytes.getValue();
		else
		{
			String s = Kernel32Util.formatMessageFromLastErrorCode(Kernel32.INSTANCE.GetLastError());
			log.severe("error in File.totalSpace getting for \"" + file.getPath() + "\" " + s);
		}

		return -1;

	}

	/**
	 * Gets the files.
	 * 
	 * @param workingDir
	 *            the working dir
	 * @param pattern
	 *            the pattern
	 * 
	 * @return the files
	 */
	public static Collection getFiles(String workingDir, String pattern)
	{
		ArrayList result = new ArrayList();

		// check if we have a non patterned file name

		// check if we have an absolute file
		File res = new File(pattern);
		if (res.exists() && res.isAbsolute())
		{
			result.add(res);
			return result;
		}

		// check if we have a file relative working dir
		if (!res.isAbsolute())
		{
			res = new File(workingDir, pattern);
			if (res.exists())
			{
				result.add(res);
				return result;
			}
		}

		// so this must be a pattern try to figure out the files
		String[] s = pattern.split("[" + File.separator + "|/]");
		String[] sh;
		if (s.length == 1)
		{
			sh = new String[2];
			sh[0] = ".";
			sh[1] = s[0];
		}
		else
			sh = s;

		if (pattern.startsWith("/") && "".equals(sh[0]))
			sh[0] = "/";

		Collection paths = new HashSet();
		paths.add(sh[0]);
		for (int i = 1; i < sh.length; i++)
		{
			String file = sh[i];
			Collection newPaths = new HashSet();
			for (Iterator it = paths.iterator(); it.hasNext();)
			{
				String pathStr = (String) it.next();
				if (pathStr.endsWith(":"))
					pathStr += "/";
				File path = new File(pathStr);
				if (!path.isDirectory() || !path.exists() || !path.isAbsolute())
					path = new File(workingDir, pathStr);
				Collection files = getWildcardFiles(path.getAbsolutePath(), file);
				for (Iterator it2 = files.iterator(); it2.hasNext();)
				{
					File f = (File) it2.next();
					if (f.isDirectory())
						newPaths.add(f.getPath());
					else if (f.isFile())
						result.add(f);
				}
			}
			paths = newPaths;
		}

		/*
		 * String file = s[s.length-1]; String path = pattern.substring(0,
		 * pattern.lastIndexOf(file));
		 * 
		 * if (path == null || path.equals("")) path = "."; File fPath = null;
		 * try { fPath = new File(path); if (!fPath.isDirectory()) {
		 * log.warning("classpath directory "+fPath.getCanonicalPath()+" not
		 * found"); return result; } } catch (Exception ex) {
		 * log.warning("classpath directory "+path+" error" + ex.getMessage());
		 * return result; } FileFilter fileFilter = new
		 * WildcardFileFilter(file); File[] thisFiles =
		 * fPath.listFiles(fileFilter); for (int i=0; i< thisFiles.length; i++)
		 * { File f = thisFiles[i]; if (f.exists()) result.add(f); else
		 * log.warning("classpath file "+f.getName() +"not found"); }
		 */
		if (result.size() == 0)
			log.warning("No files found for " + pattern);
		return result;
	}

	/**
	 * Gets the wildcard files.
	 * 
	 * @param path
	 *            the path
	 * @param file
	 *            the file
	 * 
	 * @return the wildcard files
	 */
	private static Collection getWildcardFiles(String path, String file)
	{
		ArrayList result = new ArrayList();
		File fPath = new File(path);
		try
		{
			if (!fPath.isDirectory())
			{
				log.warning("classpath directory " + fPath.getCanonicalPath() + " not found");
				return result;
			}
		}
		catch (Exception ex)
		{
			log.warning("classpath directory " + path + " error" + ex.getMessage());
			return result;
		}
		FileFilter fileFilter = new WildcardFileFilter(file);
		File[] thisFiles = fPath.listFiles(fileFilter);
		for (int i = 0; i < thisFiles.length; i++)
		{
			File f = thisFiles[i];
			if (f.exists())
				result.add(f);
			else
				log.warning("classpath file " + f.getName() + "not found");
		}
		return result;
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		System.out.println(FileUtils.freeSpace(new File("C:\\")));
		System.out.println(FileUtils.totalSpace(new File("C:\\")));
		for (Iterator it = getFiles(".", "lib/*/*/*jar").iterator(); it.hasNext();)
			System.out.println(it.next());
	}
}
