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
package org.rzo.yajsw.wrapper;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.logging.Logger;

import org.apache.commons.configuration.AbstractConfiguration;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.ConfigurationConverter;
import org.apache.commons.configuration.EnvironmentConfiguration;
import org.apache.commons.io.filefilter.WildcardFileFilter;

// TODO: Auto-generated Javadoc
/**
 * The Class FileUtils.
 */
public class FileUtils
{

	/** The log. */
	static Logger	log	= Logger.getLogger(FileUtils.class.getName());

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
		File res = new File(pattern);
		if (res.exists() && res.isAbsolute())
		{
			result.add(res);
			return result;
		}
		// pk-20080626: added working dir
		// res = new File(workingDir, pattern);
		File workingDirectory = new File(workingDir);
		res = new File(workingDirectory.getAbsolutePath(), pattern);
		// System.out.println("FileUtils: filename="+res+", exists:"+res.exists());
		if (res.exists())
		{
			result.add(res);
			return result;
		}

		// so this must be a pattern try to figure out the files
		// does not work -> without separator
		//String[] s = pattern.split("[" + File.separator + "|/]");
		String[] s = pattern.split("[\\\\|/]");
		String[] sh;
		if (s.length == 1)
		{
			sh = new String[2];
			sh[0] = ".";
			sh[1] = s[0];
		}
		else
			sh = s;

		Collection paths = new HashSet();
		paths.add(sh[0]);
		for (int i = 1; i < sh.length; i++)
		{
			String file = sh[i];
			if (file.trim().length() == 0)
				continue;
			Collection newPaths = new HashSet();
			for (Iterator it = paths.iterator(); it.hasNext();)
			{
				String pathStr = (String) it.next();
				if (pathStr.endsWith(":"))
					pathStr += "/";
				File path = new File(pathStr);
				if ((!path.isDirectory()) || (!path.exists()) || (!(path.isAbsolute())))
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
		 * log.warning(
		 * "classpath directory "+fPath.getCanonicalPath()+" not found"); return
		 * result; } } catch (Exception ex) {
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
		file = file.trim();
		if (file.equals(".") || file.equals(".."))
		{
			result.add(new File(path+"/"+file));
			return result;
		}
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
		System.out.println(getFiles(".", "z:\\dev\\yajsw\\..\\yajsw\\*.jar").size());
		try
		{
			// String
			// fileName=FilenameUtils.separatorsToSystem("C:\\init\\MOBILEguard\\yajsw/lib/jvmstat/*.jar");
			// System.out.println("FileName: "+fileName);
			CompositeConfiguration compConfig = new CompositeConfiguration();
			AbstractConfiguration configuraton = new BaseConfiguration();
			compConfig.addConfiguration(new EnvironmentConfiguration());
			configuraton.setProperty("wrapper.java.classpath.1", "${VERSANT_ROOT}/lib/jvi.*jar");
			configuraton.setProperty("wrapper.java.classpath.2", "${GROOVY_HOME}/lib/*.jar");
			compConfig.addConfiguration(configuraton);
			System.out.println("Configuration: " + ConfigurationConverter.getProperties(compConfig));
			System.out.println("subset: " + ConfigurationConverter.getProperties(compConfig.subset("wrapper.java")));

			// Collection files=FileUtils.getFiles("../..",
			// "C:/versant/7_0_1/lib/jvi*.jar");
			// Collection collection=
			// org.apache.commons.io.FileUtils.listFiles(new File("C:/"),
			// new WildcardFileFilter("jvi*.jar"), new
			// WildcardFileFilter("*jar"));
			// File[] files= new
			// File("C:").listFiles((FilenameFilter)FileFilterUtils.nameFileFilter("C:/versant/7_0_1/lib/jvi*.jar"));

			//         
			// FileUtils.getFiles("C:/versant/7_0_1/lib/", "jvi*.jar");
			// System.out.println("FileList="+
			// FileUtils.getFiles("C:/versant/7_0_1/lib/", "jvi*.jar"));
			// java.util.Arrays.asList(files));
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
	}
}
