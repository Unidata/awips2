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
package com.raytheon.uf.nativelib.cimport;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;

import org.w3c.dom.*;
import org.xml.sax.SAXException;

/**
 * Import and build all projects in the given workspace.
 * <p>
 * If the clean option is given all projects are cleaned before building.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009       1996 jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */
public class CimportApplication implements IApplication {

	protected static final String ECLIPSE_PROJECT = ".project";
	private static final String CLEAN_OPTION = "clean";
	private static final String CDT_PROJECT = ".cproject";
	private static final int CHECK_USAGE = 1;
	private IWorkspace workspace = ResourcesPlugin.getWorkspace();
	private IProgressMonitor progressMonitor = new SystemOutProgressMonitor();

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.equinox.app.IApplication#start(org.eclipse.equinox.app.
	 * IApplicationContext)
	 */
	public Object start(IApplicationContext context) throws CoreException {

		List<String> buildOptions = Arrays.asList((String[]) context
				.getArguments().get("application.args"));
		
		List<String> buildConfigurations = new ArrayList<String>();
		List<String> buildType = new ArrayList<String>();

		for (int i = 0; i < buildOptions.size(); i++) {
			
			String option = buildOptions.get(i);
			
			if (option.startsWith("-c") && ++i < buildOptions.size()) {
				
				buildConfigurations.add(buildOptions.get(i));
			}
			
			if (option.startsWith("-b") && ++i < buildOptions.size()) {
				
				buildType.add(buildOptions.get(i));
			}
			
			if (option.startsWith("-h")) {
				printUsage();
				System.exit(CHECK_USAGE);
			}
			
		}
		
		// --- Obtain a list of project Directories ---

		File workspaceDir = new File(Platform.getLocation().toOSString());
		File[] projectDirs = null;

		System.out.println("Using workspace: " + workspaceDir.toString());

		FileFilter fileFilter = new FileFilter() {

			@Override
			public boolean accept(File pathname) {
				boolean isDir = pathname.isDirectory();
				boolean isEclipseProject = new File(pathname.toString()
						+ File.separator + ECLIPSE_PROJECT).exists();
				return (isDir && isEclipseProject);
			}

		};

		projectDirs = workspaceDir.listFiles(fileFilter);

		// activate the given cconfiguration(s)
		
		for (String target : buildConfigurations) {
			
			System.out.println("Activing "+target+" configuration ...");

			for (File project : projectDirs) {
				File cproject = new File(project.toString() + File.separator
						+ CDT_PROJECT);
				boolean isCDTProject = cproject.exists();

				if (isCDTProject) {
							activateConfiguration(target, cproject);

				}
			}
		}

		// Don't build the projects as they are imported.
		System.out.println("Disabling auto-building...");

		IWorkspaceDescription wsDescription = workspace.getDescription();

		wsDescription.setAutoBuilding(false);
		workspace.setDescription(wsDescription);

		// --- Import the project Directories into the workspace ---

		int importCount = 0;

		for (File project : projectDirs) {
			System.out.println("Importing " + project.getName().toString()
					+ " ...");

			try {
				if (importExisitingProject(new org.eclipse.core.runtime.Path(
						project.toString())) == false) {
					System.err.println(project.getName() + " exists.");
				} else {
					importCount++;
				}
			} catch (CoreException e) {
				System.err.println("ERROR: " + project.getName()
						+ " failed to import. " + e.getMessage());
				throw e;
			}
		}

		System.out.println("Found " + importCount + " new projects.");
		System.out.println(projectDirs.length
				+ " projects are currently imported into the workspace.");

		System.out.println("Building workspace ...");

		if (buildType.contains(CLEAN_OPTION)) {
			workspace.build(IncrementalProjectBuilder.CLEAN_BUILD,
					progressMonitor);
		}
		workspace.build(IncrementalProjectBuilder.FULL_BUILD, progressMonitor);
		
		return IApplication.EXIT_OK;
	}

	private void printUsage() {
		
		String usage = "-b buildType Specify the build type such as clean.\n";
		usage += "-c buildConfiguration Actives the given configuration such as i386-pc-linux-gnu.debug.\n";
		
		System.out.println(usage);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.equinox.app.IApplication#stop()
	 */
	public void stop() {
		// nothing to do
	}

	/**
	 * Imports the given path into the workspace as a project. Returns true if
	 * the operation succeeded, false if it failed to import due to an overlap.
	 * 
	 * from http://www.eclipsezone.com/eclipse/forums/t107019.html
	 * 
	 * @param projectPath
	 * @return
	 * @throws CoreException
	 * @throws CoreException
	 *             if operation fails catastrophically
	 */
	private boolean importExisitingProject(IPath projectPath)
			throws CoreException {
		// Load the project description file
		final IProjectDescription description = workspace
				.loadProjectDescription(projectPath.append(IPath.SEPARATOR
						+ IProjectDescription.DESCRIPTION_FILE_NAME));
		final IProject project = workspace.getRoot().getProject(
				description.getName());

		// Only import the project if it doesn't appear to already exist.
		if (project.exists()) {
			return false;
		}
		IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				project.create(description, monitor);
				project.open(IResource.NONE, monitor);
			}
		};
		workspace.run(runnable, workspace.getRuleFactory().modifyRule(
				workspace.getRoot()), IResource.NONE, progressMonitor);
		return true;
	}

	/**
	 * Activates the given cconfiguration in the given cprojects file
	 */
	private void activateConfiguration(String configurationName,
			File cprojectFile) {
		
		try {
		
		// Parse the cprojectFile
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		
		DocumentBuilder parser = factory.newDocumentBuilder();
		Document document = parser.parse(cprojectFile);
		
		XPath rootConfigurationPath =  XPathFactory.newInstance().newXPath();
		Node rootConfiguration = (Node) rootConfigurationPath.evaluate("/cproject/storageModule", document, XPathConstants.NODE);
		
		XPath desiredConfigurationPath = XPathFactory.newInstance().newXPath();
		Node desiredConfiguration = (Node) desiredConfigurationPath.evaluate("/cproject/storageModule/cconfiguration/storageModule[@name='"+configurationName+"']/parent::*", document, XPathConstants.NODE);
		
		XPath otherConfigurationsPath = XPathFactory.newInstance().newXPath();
		Node otherConfigurations = (Node)otherConfigurationsPath.evaluate("/cproject/storageModule/cconfiguration/storageModule[not(@name='"+configurationName+"')]/parent::*", document, XPathConstants.NODE);
		
		
		if (desiredConfiguration != null) {
			System.out.println(cprojectFile.getParentFile().getName());
			
			// set the desire configuration to the default
			rootConfiguration.insertBefore(desiredConfiguration, otherConfigurations);
			
			// Write the changes to the file
			Transformer transformer = TransformerFactory.newInstance().newTransformer();			
			StreamResult result = new StreamResult(cprojectFile);
			
			DOMSource source = new DOMSource(document);
			transformer.transform(source,result);
		}
		
		
		
		}
		catch (TransformerConfigurationException e) {
			//TODO
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XPathExpressionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	/**
	 * Sends all progress to StdOut
	 */
	private static class SystemOutProgressMonitor extends NullProgressMonitor {

		String lastTask = "";

		public void beginTask(String name, int totalWork) {
			outputTask(name);
		}

		public void subTask(String name) {
			outputTask(name);
		}
		
		private void outputTask(String name) {
			if (name != null && name.length() > 0 && !name.equals(lastTask)) {
				System.out.println(name);
				lastTask = name;
			}
		}
	}

}
