package org.rzo.yajsw.boot;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import org.rzo.yajsw.Constants;
import org.rzo.yajsw.app.WrapperJVMMain;

public class WrapperLoader
{
	
	private static boolean checkPath(String path)
	{
		int ix = path.indexOf("!");
		if (ix == -1)
		{
			System.out.println("<yajsw>/wrapper.jar not found, please check classpath. aborting wrapper !");
			Runtime.getRuntime().halt(999);// -> groovy eclipse plugin crashes
			return false;
		}
		return true;
		
	}
	
	
	/**
	 * Gets the wrapper jar.
	 * 
	 * @return the wrapper jar
	 */
	public static String getWrapperJar()
	{
		String cn = Constants.class.getCanonicalName();
		String rn = cn.replace('.', '/') + ".class";
		String path = ".";
		try
		{
			path = Constants.class.getClassLoader().getResource(rn).getPath();
			if (!checkPath(path))
				return null;
			path = path.substring(0, path.indexOf("!"));
			path = new URI(path).getPath();
			path.replaceAll("%20", " ");
			//System.out.println("wrapper jar "+path);
			// if (path.startsWith("/"))
			// path = path.substring(1);
			return path;
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
		}
		return null;
	}
	
	public static String getWrapperAppJar()
	{
		String cn = WrapperJVMMain.class.getCanonicalName();
		String rn = cn.replace('.', '/') + ".class";
		String path = ".";
		try
		{
			path = WrapperJVMMain.class.getClassLoader().getResource(rn).getPath();
			if (!checkPath(path))
				return null;
			path = path.substring(0, path.indexOf("!"));
			path = new URI(path).getPath();
			path.replaceAll("%20", " ");
			//System.out.println("wrapper jar "+path);
			// if (path.startsWith("/"))
			// path = path.substring(1);
			return path;
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
		}
		return null;
	}
	
	public static ArrayList getGroovyClasspath()
	{
		ArrayList result = new ArrayList();
		String wrapperHome = getWrapperHome();
		File groovyLib = new File(wrapperHome, "lib");
		if (! groovyLib.exists())
		{
			System.out.println("<yajsw>/lib folder not found. Please check that relative the lib folder is in the same folder as <yajsw>/wrapper.jar");
			return result;
		}
		File[] groovyLibs = groovyLib.listFiles();
		for (File file : groovyLibs)
		{
			if (file.isDirectory())
				result.addAll(getFiles(file));
			else
				try
				{
					result.add(file.toURI().toURL());
				}
				catch (MalformedURLException e)
				{
					System.out.println("Error in getGroovyClasspath: "+e.getMessage());
				}
		}
	    return result;
	}

	private static Collection getFiles(File parent)
	{
		ArrayList result = new ArrayList();
		File[] files = parent.listFiles();
		for (File file : files)
		{
			if (file.isDirectory())
				result.addAll(getFiles(file));
			else 
				try
			{
				result.add(file.toURI().toURL());
			}
			catch (MalformedURLException e)
			{
				System.out.println("Error in getGroovyClasspath: "+e.getMessage());
			}
		}
		return result;
	}

	public static URL[] getWrapperClasspath(String type, boolean logErrors)
	{
		String wrapperJar;
		if ("App".equals(type))
			 wrapperJar = getWrapperAppJar();
		else
			wrapperJar = getWrapperJar();
		Manifest manifest;
		try
		{
			manifest = new JarFile(wrapperJar).getManifest();
		}
		catch (IOException e1)
		{
			// TODO Auto-generated catch block
			e1.printStackTrace();
			return null;
		}
		Attributes attr = manifest.getMainAttributes();

		String cl = attr.getValue("Class-Path-" + type);
		if (cl == null)
			return null;

		ArrayList classpath = new ArrayList();
		classpath.add(new File(wrapperJar));
		String[] clArr = cl.split(" ");
		File parent = new File(wrapperJar).getParentFile();
		for (int i = 0; i < clArr.length; i++)
		{
			String file = clArr[i];
			File myFile;
			try
			{
				myFile = new File(parent, file);
				if (!myFile.exists() && logErrors)
					System.out.println("WARNING: lib not found: " + myFile.getCanonicalPath());
				classpath.add(myFile);
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}
		
		// add rt.jar
		if ("App".equals(type))
		try
		{
			String rt = getRTJar();
			File rtf = new File(rt);
			if (!rtf.exists())
				System.out.println("could not find rt.jar");
			else
				classpath.add(rtf);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}


		URL[] urlsArr = new URL[classpath.size()];
		int i = 0;
		for (Iterator it = classpath.iterator(); it.hasNext(); i++)
			try
			{
				urlsArr[i] = ((File) it.next()).toURI().toURL();
				// System.out.println("classpath: "+urlsArr[i]);
			}
			catch (Exception e)
			{
				// log.throwing(WrapperMain.class.getName(), "main", e);
				e.printStackTrace();
			}

		return urlsArr;

	}

	public static URLClassLoader getWrapperClassLoader()
	{
		URL[] core = getWrapperClasspath("Wrapper-Core", true);
		URL[] extended = getWrapperClasspath("Wrapper-Extended", false);
		URL[] urls = new URL[core.length+ extended.length];
		System.arraycopy(core, 0, urls, 0, core.length);
		System.arraycopy(extended, 0, urls, core.length, extended.length);
		return new WrapperClassLoader(urls, Thread.currentThread().getContextClassLoader());
	}
	
	public static String getWrapperHome()
	{
		return new File(getWrapperJar()).getParent();
	}
	
	public static String getRTJar()
	{
		String cn = Object.class.getCanonicalName();
		String rn = cn.replace('.', '/') + ".class";
		String path = ".";
		try
		{
			path = WrapperJVMMain.class.getClassLoader().getResource(rn).getPath();
			if (!checkPath(path))
				return null;
			path = path.substring(0, path.indexOf("!"));
			path = new URI(path).getPath();
			path.replaceAll("%20", " ");
			//System.out.println("wrapper jar "+path);
			// if (path.startsWith("/"))
			// path = path.substring(1);
			return path;
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
		}
		return null;
	}
	


}
