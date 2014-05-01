package org.rzo.builder;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemManager;
import org.apache.commons.vfs2.VFS;
import org.rzo.yajsw.util.VFSUtils;

/**
 * 
 * compress multiple jar files into a single jar file
 * 
 * 1. run a program with -verbose:class, so that all required classes are loaded and logged
 * 2. write log to a file
 * 3. call java JarBuilder log-file newjar
 * 
 * JarBuilder will build a jar with the files used by the application
 *
 */

public class JarBuilder
{
	// multimap: assignment of jar files to class files used in the application
	Map<String, Set<String>> _jar2class = new HashMap<String, Set<String>>();
	
	// populate jar2class
	private void parseLog(String file) throws IOException
	{
		InputStream in = new FileInputStream(file);
		// -verbose:class logs have the pattern [Loaded <class> from file:/<jar or folder>]
		Pattern p = Pattern.compile("\\[Loaded\\s(.*)\\s+from file\\:/(.*)\\]");
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));
		String line = null;
		try
		{
			while ((line = reader.readLine()) != null)
			{
				Matcher m = p.matcher(line);
				if (m.find())
				{
					String clazz = m.group(1);
					String jar = m.group(2);
					// TODO do not add java classes
					if (!jar.endsWith("/rt.jar"))
						addClazz(jar, clazz);
				}
			}
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}
	
	// add an entry to the multimap
	private void addClazz(String jar, String clazz)
	{
		Set<String> clazzes = _jar2class.get(jar);
		System.out.println("add "+jar+"!"+clazz);
		if (clazzes == null)
		{
			clazzes = new HashSet<String>();
			_jar2class.put(jar, clazzes);
		}
		clazzes.add(clazz);		
	}
	
	// add a file to the destination jar
	private void addFile(FileObject f, String name, ZipOutputStream out) throws IOException
	{
		InputStream in = f.getContent().getInputStream();
		out.putNextEntry(new ZipEntry(name));
		
		byte[] buf = new byte[1024];
		int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        // Complete the entry
        out.closeEntry();
        in.close();		
	}
	
	// build the jar
	// VFS currently does not support building zip files -> use java's ZipOutputStream
	private void buildJar(String newJar) throws IOException
	{
		ZipOutputStream out = new ZipOutputStream(new FileOutputStream(newJar ));
		FileSystemManager fsManager = VFS.getManager();
		for (String jar : _jar2class.keySet())
		{
			FileObject jarFile;
			if (jar.endsWith(".jar"))
				jarFile = fsManager.resolveFile( "jar:"+jar );
			else
				jarFile = fsManager.resolveFile( jar );
				
				for (String file : _jar2class.get(jar))
				{
					file = file.replaceAll("\\.", "/");
					file += ".class";
					FileObject f = fsManager.resolveFile(jarFile, file);
					if (f.exists())
						addFile(f, file, out);
					else
						System.out.println("file not found "+f);
				}	
				
		}
		out.close();
	}
	

	public static void main(String[] args) throws Exception
	{
		String logFile = args[0];
		String newJar = args[1];
		new File(newJar).delete();
		JarBuilder b = new JarBuilder();
		b.parseLog(logFile);
		b.buildJar(newJar);
		
	}

}
