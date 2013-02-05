package com.raytheon.uf.anttasks.includesgen;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.raytheon.uf.featureexplorer.FeatureException;
import com.raytheon.uf.featureexplorer.FeatureExplorer;

/**
 * Custom Ant task for generating includes files based on feature.xml
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             bclement    Initial creation
 * 28Jan2009    1930       MW Fegan    Added provider and plugin filtering.
 * Feb 4, 2013  #1577      bkowal      Remove component-deploy.xml suffixes for
 *                                     core and plugins; remove jar wildcard for cots
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class GenerateIncludesFromFeature extends Task {

	protected String baseDirectories;

	protected File baseDirectory;

	protected File featureFile;

	protected File cotsOut;

	protected File plugsOut;

	protected File coreOut;

	protected File allOut;

	/* attributes for filtering */
	private static final String PATTERN = "^.*(%s).*$";

	protected String provider = "raytheon";

	private Pattern providerPattern = Pattern.compile(String.format(PATTERN,
			provider));

	protected String plugin = "plugin";

	private Pattern pluginPattern = Pattern.compile(String.format(PATTERN,
			plugin));

	/**
	 * @return the baseDirectories
	 */
	public String getBaseDirectories() {
		return baseDirectories;
	}

	/**
	 * @param baseDirectories
	 *            the baseDirectories to set
	 */
	public void setBaseDirectories(String baseDirectories) {
		this.baseDirectories = baseDirectories;
	}

	/**
	 * Sets the component provider filter. This is a pipe (|) separated string
	 * of names that identify a component as a non-COTS component. The default
	 * provider filter is <em>raytheon</em>.
	 */
	public void setProviderFilter(String filter) {
		this.provider = filter;
		this.providerPattern = Pattern.compile(String.format("^.*(%s).*$",
				filter));
	}

	/**
	 * Sets the plug-in identifier filter. This is a pipe (|) separated string
	 * of names that identify a component as an EDEX plug-in. The default
	 * plug-in filter is <em>plugin</em>.
	 */
	public void setPluginFilter(String filter) {
		this.plugin = filter;
		this.pluginPattern = Pattern.compile(String
				.format("^.*(%s).*$", filter));
	}

	public void setAllOut(File f) {
		this.allOut = f;
	}

	public void setFeatureFile(File f) {
		this.featureFile = f;
	}

	public void setCotsOut(File f) {
		this.cotsOut = f;
	}

	public void setPlugsOut(File f) {
		this.plugsOut = f;
	}

	public void setCoreOut(File f) {
		this.coreOut = f;
	}

	public void setBaseDirectory(File aDir) {
		this.baseDirectory = aDir;
	}

	/**
	 * Main class called from ant. Tests to see if at least 1 includes file is
	 * to be generated. Only generates includes that have been specified from
	 * ant script. Throws BuildException if any error occurs, this halts the ant
	 * build and displays the error.
	 * 
	 */
	public void execute() throws BuildException {

		log("provider filter=" + this.providerPattern.toString());
		log("plugin filter=" + this.pluginPattern.toString());

		ArrayList<File> components = getComponents();
		if (cotsOut == null && plugsOut == null && coreOut == null
				&& allOut == null)
			throw new BuildException(
					"Must supply destination for at least one includes file");
		if (featureFile == null)
			throw new BuildException("Must supply a feature.xml file");
		if (cotsOut != null)
			generateCots(components);
		if (plugsOut != null)
			generatePlugs(components);
		if (coreOut != null)
			generateCore(components);
		if (allOut != null)
			generateAll(components);
	}

	/**
	 * Generates an includes file for all components in the feature. The
	 * directories in comps will be added to the includes file with a recursive
	 * reg ex. This means that the director and all of its sub directories and
	 * files will be included.
	 * 
	 * @param comps
	 */
	protected void generateAll(ArrayList<File> comps) {
		log("Generating ALL list in " + this.allOut.getPath());
		PrintWriter out = null;
		try {
			out = new PrintWriter(new FileWriter(allOut));
			SimpleDateFormat format = new SimpleDateFormat(
					"yyyy-MM-dd HH:mm:ss z");
			format.setTimeZone(TimeZone.getTimeZone("GMT"));
			out.println("## Feature includes file generated on "
					+ format.format(new Date()));

			for (File f : comps) {
				out.println(f.getName() + "/**");
			}
			log(String.format("Identified %d ALL list entries", comps.size()));
		} catch (IOException e) {
			throw new BuildException(e);
		} finally {
			if (out != null) {
				out.close();
			}
		}

	}

	/**
	 * Populates a list of project directories with one directory for each
	 * component stated in feature. This list is built based on manifest ids and
	 * version compared to what is stated in the feature. These can differ
	 * drastically from the name of project.
	 * 
	 * @return a populated list of directories based off feature
	 * @throws BuildException
	 *             if there are any problems accessing feature or finding
	 *             required components from feature.
	 */
	protected ArrayList<File> getComponents() throws BuildException {

		ArrayList<File> rval = null;

		FeatureExplorer fe = null;

		// list of directories overrides single directory
		if (baseDirectories != null) {
			String[] fileNames = baseDirectories.split(";");
			ArrayList<File> files = new ArrayList<File>(fileNames.length);
			for (String fName : fileNames) {
				File file = new File(fName);
				files.add(file);
			}
			fe = new FeatureExplorer(new WorkspaceFeatureSearch(files),
					new WorkspacePluginSearch(files));
		} else if (baseDirectory != null) {
			fe = new FeatureExplorer(baseDirectory, new WorkspaceFeatureSearch(
					baseDirectory), new WorkspacePluginSearch(baseDirectory));
		} else {
			throw new BuildException(
					"Did not have a baseDirectory or baseDirectories");
		}

		try {
			rval = fe.getPlugins(featureFile);
		} catch (FeatureException e) {
			throw new BuildException(e);
		}
		if (rval.isEmpty()) {
			throw new BuildException("Unable to access file " + featureFile);
		}

		return rval;
	}

	/**
	 * Generates an includes file for all components (comps) having a project
	 * directory name that does not match the {@link #setProviderFilter(String)
	 * provider filter}. The project's manifest will be searched for a list of
	 * jars to specify in the includes file. If the list is not present in the
	 * manifest, the default is to include all *.jar files in the includes file.
	 * 
	 * @param comps
	 *            The full list of feature specified components
	 * @throws BuildException
	 *             if any IOException occurs
	 */
	protected void generateCots(ArrayList<File> comps) throws BuildException {
		log("Generating COTS list in " + this.cotsOut.getPath());
		PrintWriter out = null;
		try {
			out = new PrintWriter(new FileWriter(cotsOut));
			SimpleDateFormat format = new SimpleDateFormat(
					"yyyy-MM-dd HH:mm:ss z");
			format.setTimeZone(TimeZone.getTimeZone("GMT"));
			out.println("## Cots includes file generated on "
					+ format.format(new Date()));
			int count = 0;
			for (File f : comps) {
				Matcher m = providerPattern.matcher(f.getName());
				if (!m.matches()) {
					out.println(f.getName());
					count++;
				}
			}
			log(String.format("Identified %d COTS list entries", count));
		} catch (IOException e) {
			throw new BuildException(e);
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	/**
	 * Generates an includes file for all components (comps) having a project
	 * directory name that matches both the {@link #setProviderFilter(String)
	 * provider filter} and the {@link #setPluginFilter(String) plug-in filter}.
	 * 
	 * @param comps
	 *            The full list of feature specified components
	 * @throws BuildException
	 *             if any IOException occurs
	 */
	protected void generatePlugs(ArrayList<File> comps) throws BuildException {
		log("Generating PLUGS list in " + this.plugsOut.getPath());
		PrintWriter out = null;
		try {
			out = new PrintWriter(new FileWriter(plugsOut));
			SimpleDateFormat format = new SimpleDateFormat(
					"yyyy-MM-dd HH:mm:ss z");
			format.setTimeZone(TimeZone.getTimeZone("GMT"));
			out.println("## Plug-in includes file generated on "
					+ format.format(new Date()));
			int count = 0;
			for (File f : comps) {
				Matcher prm = providerPattern.matcher(f.getName());
				Matcher plm = pluginPattern.matcher(f.getName());
				if (prm.matches() && plm.matches()) {
					out.println(f.getName());
					count++;
				}
			}
			log(String.format("Identified %d PLUGS list entries", count));
		} catch (IOException e) {
			throw new BuildException(e);
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}

	/**
	 * Generates an includes file for all components (comps) having a project
	 * directory name that matches the {@link #setProviderFilter(String)
	 * provider filter} and does not match the {@link #setPluginFilter(String)
	 * plug-in filter}.
	 * 
	 * @param comps
	 *            The full list of feature specified components
	 * @throws BuildException
	 *             if any IOException occurs
	 */
	protected void generateCore(ArrayList<File> comps) throws BuildException {
		log("Generating CORE list in " + this.coreOut.getPath());
		PrintWriter out = null;
		try {
			out = new PrintWriter(new FileWriter(coreOut));
			SimpleDateFormat format = new SimpleDateFormat(
					"yyyy-MM-dd HH:mm:ss z");
			format.setTimeZone(TimeZone.getTimeZone("GMT"));
			out.println("## Core includes file generated on "
					+ format.format(new Date()));
			int count = 0;
			for (File f : comps) {
				Matcher prm = providerPattern.matcher(f.getName());
				Matcher plm = pluginPattern.matcher(f.getName());
				if (prm.matches() && !plm.matches()) {
					out.println(f.getName());
					count++;
				}
			}
			log(String.format("Identified %d CORE list entries", count));
		} catch (IOException e) {
			throw new BuildException(e);
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}
}
