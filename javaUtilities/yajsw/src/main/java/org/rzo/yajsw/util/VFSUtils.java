package org.rzo.yajsw.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSelectInfo;
import org.apache.commons.vfs2.FileSelector;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemOptions;
import org.apache.commons.vfs2.FileType;
import org.apache.commons.vfs2.VFS;
import org.apache.commons.vfs2.impl.DefaultFileSystemManager;
import org.apache.commons.vfs2.provider.http.HttpFileSystemConfigBuilder;


public class VFSUtils
{
	static DefaultFileSystemManager	fsManager	= null;
	static FileSystemOptions		opts		= new FileSystemOptions();

	public static void init() throws FileSystemException
	{
		if (fsManager != null)
			return;

		fsManager = (DefaultFileSystemManager) VFS.getManager();
		String httpProxy = System.getProperty("http.proxyHost");
		String httpPort = System.getProperty("http.proxyPort");
		if (httpProxy != null)
		{
			HttpFileSystemConfigBuilder.getInstance().setProxyHost(opts, httpProxy);

			int port = 8080;
			if (httpPort != null)
				try
				{
					port = Integer.parseInt(httpPort);
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
			HttpFileSystemConfigBuilder.getInstance().setProxyPort(opts, port);
		}
	}

	public static FileObject resolveFile(String base, String file) throws FileSystemException
	{
		init();
		FileObject basef = null;
		if (base != null)
			basef = fsManager.resolveFile(new File("."), base);
		return resolveFile(basef, file);
	}

	public static FileObject resolveFile(FileObject basef, String file) throws FileSystemException
	{
		init();
		if (basef != null)
			return fsManager.resolveFile(basef, file, opts);
		else
			return fsManager.resolveFile(file, opts);
	}
	
	public static List<FileObject> resolveFiles(String value) throws Exception
	{
		init();
		return resolveFiles(fsManager.resolveFile(new File(".").getAbsolutePath()), value);
	}
	
	public static List<FileObject> resolveFiles(FileObject basef, String value) throws Exception
	{
		init();
		return resolveFiles(basef, value, fsManager);
	}
	
	public static List<FileObject> resolveFiles(FileObject basef, String value, DefaultFileSystemManager fsManager)
	{
		System.out.println("resolve files " + value);
		try
		{
			ArrayList<FileObject> result = new ArrayList<FileObject>();

			// if no wild card, just return the file
			if (!(value.contains("?") || value.contains("*")))
			{
				result.add(VFSUtils.resolveFile(basef, value));
				return result;
			}
			// if we have wild cards
			{
				// create a java pattern from the file pattern
				String pattern = value.replaceAll("\\.", "\\\\.");
				pattern = pattern.replaceAll("\\?", ".");
				if (pattern.contains("/**/"))
					pattern = pattern.replaceAll("/\\*\\*/", "/*/");
				pattern = pattern.replaceAll("\\*", ".*");
				pattern = basef.getName().getPath() + "/" + pattern;
				final Pattern pat = Pattern.compile(pattern);

				// find prefix with no pattern
				int istar = value.indexOf("*");
				if (istar <= 0)
					istar = Integer.MAX_VALUE;
				int iquest = value.indexOf("?");
				if (iquest <= 0)
					iquest = Integer.MAX_VALUE;
				int i = Math.min(istar, iquest);
				String prefix = null;
				if (i < Integer.MAX_VALUE)
				{
					prefix = value.substring(0, i);
				}

				int depth = 0;
				if (value.contains("**/"))
				{
					depth = Integer.MAX_VALUE;
				}
				else
					while ((i = value.indexOf("*/")) != -1)
					{
						depth++;
						value = value.substring(i + 2);
					}
				final int fdepth = depth;
				FileSelector fs = new FileSelector()
				{
					public boolean includeFile(FileSelectInfo info) throws Exception
					{
						// files /x/x causes exceptions -> these are imaginary
						// files -> ignore
						if (info.getFile().getType() == FileType.IMAGINARY)
							return false;
						boolean result = pat.matcher(info.getFile().getName().getPath()).matches();
						System.out.println(info.getFile().getName().getPath() + " " + result);
						return result;
					}

					public boolean traverseDescendents(FileSelectInfo info) throws Exception
					{
						return info.getDepth() <= fdepth;
					}

				};

				FileObject nbase;
				if (prefix != null)
					nbase = basef.resolveFile(prefix);
				else
					nbase = basef;

				FileObject[] files = nbase.findFiles(fs);
				if (files != null && files.length > 0)
					return Arrays.asList(files);
				else
					return new ArrayList();
			}
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		return null;
	}


}
