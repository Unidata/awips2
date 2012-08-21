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
package com.raytheon.uf.viz.ui.menus;

import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.eclipse.core.expressions.Expression;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.AbstractContributionFactory;
import org.eclipse.ui.menus.IContributionRoot;
import org.eclipse.ui.menus.IMenuService;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.menus.MenuSerialization;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuItem;
import com.raytheon.uf.common.menus.xml.CommonMenuContributionFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.ui.menus.xml.IncludeMenuItem;

/**
 * Discover the menu contributions present in localization.
 * 
 * This will check several locations:
 * <UL>
 * <LI>The plugin localization directory
 * <LI>The cave static directory
 * <LI>The user and site localization directory
 * </UL>
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2009            chammack     Initial creation
 * Apr 27, 2012   #562     dgilling     Ensure call to MenuCreationJob
 *                                      uses proper method to retrieve
 *                                      localized site.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DiscoverMenuContributions {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscoverMenuContributions.class);

    private static boolean ran = false;

    public static Schema schema;

    public static void discoverContributions() {
        discoverContributions(new String[] { "menus" }, true);
    }

    public static void discoverContributions(boolean scheduleJob) {
        discoverContributions(new String[] { "menus" }, scheduleJob);
    }

    public static void discoverContributions(final String[] menuArray) {
        discoverContributions(menuArray, true);
    }

    public static synchronized void discoverContributions(
            final String[] menuArray, boolean scheduleJob) {
        if (ran) {
            return;
        }

        if (scheduleJob) {
            MenuCreationJob job = new MenuCreationJob(LocalizationManager
                    .getInstance().getSite());

            job.schedule();
            try {
                job.join();
            } catch (InterruptedException e2) {
                statusHandler.handle(Priority.PROBLEM,
                        e2.getLocalizedMessage(), e2);
            }
        }

        ran = true;
        try {

            URL url = FileLocator.find(Activator.getDefault().getBundle(),
                    new Path("menus.xsd"), null);
            if (url == null) {
                statusHandler
                        .handle(Priority.CRITICAL,
                                "Unable to load menu schema, menus will not operate properly");
            }
            url = FileLocator.resolve(url);

            InputStream is = (InputStream) url.getContent();
            StreamSource ss = new StreamSource(is);
            schema = SchemaFactory.newInstance(
                    XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(ss);

        } catch (Exception e1) {
            statusHandler
                    .handle(Priority.CRITICAL,
                            "Error reading menu schema, menus will not operate properly",
                            e1);
        }

        LocalizationFile[] file = null;

        if (menuArray.length == 1) {
            file = PathManagerFactory.getPathManager().listStaticFiles(
                    menuArray[0], new String[] { "index.xml" }, true, true);
        } else {
            List<LocalizationFile> fileList = new ArrayList<LocalizationFile>();

            for (String menu : menuArray) {
                LocalizationFile[] files = PathManagerFactory.getPathManager()
                        .listStaticFiles(menu, new String[] { "index.xml" },
                                true, true);
                for (LocalizationFile lf : files) {
                    fileList.add(lf);
                }
            }
            file = new LocalizationFile[fileList.size()];
            fileList.toArray(file);
        }

        Unmarshaller um = null;
        try {
            JAXBContext c = MenuSerialization.getJaxbContext();
            um = c.createUnmarshaller();
            um.setSchema(schema);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating jaxb context", e);
            return;
        }

        for (LocalizationFile lf : file) {
            try {
                final CommonMenuContributionFile mcf = (CommonMenuContributionFile) um
                        .unmarshal(lf.getFile());
                if (mcf.contribution != null) {
                    IMenuService menuService = (IMenuService) PlatformUI
                            .getWorkbench().getService(IMenuService.class);
                    for (final CommonIncludeMenuItem im : mcf.contribution) {
                        final IncludeMenuItem imc = new IncludeMenuItem();
                        imc.fileName = im.fileName;
                        imc.installationLocation = im.installationLocation;
                        imc.removals = im.removals;
                        imc.subMenuName = im.subMenuName;
                        imc.substitutions = VariableSubstitution.combine(
                                mcf.substitutions, im.substitutions);
                        imc.visibleOnActionSet = im.visibleOnActionSet;
                        AbstractContributionFactory viewMenuAddition = new AbstractContributionFactory(
                                imc.installationLocation, Activator.PLUGIN_ID) {

                            /*
                             * (non-Javadoc)
                             * 
                             * @see
                             * org.eclipse.ui.menus.AbstractContributionFactory#
                             * createContributionItems
                             * (org.eclipse.ui.services.IServiceLocator,
                             * org.eclipse.ui.menus.IContributionRoot)
                             */
                            @SuppressWarnings("restriction")
                            @Override
                            public void createContributionItems(
                                    IServiceLocator serviceLocator,
                                    IContributionRoot additions) {

                                IContributionItem[] items = null;

                                try {
                                    items = imc.getContributionItems(null,
                                            new VariableSubstitution[0],
                                            new HashSet<String>());
                                } catch (VizException e) {
                                    statusHandler.handle(Priority.SIGNIFICANT,
                                            "Error setting up menus", e);
                                }

                                Expression exp = null;
                                if (imc.visibleOnActionSet != null) {
                                    org.eclipse.core.internal.expressions.WithExpression we = new org.eclipse.core.internal.expressions.WithExpression(
                                            "activeContexts");

                                    org.eclipse.core.internal.expressions.IterateExpression oe = null;
                                    try {
                                        oe = new org.eclipse.core.internal.expressions.IterateExpression(
                                                "or");
                                    } catch (CoreException e) {
                                        // ignore, since this is hardcoded
                                    }
                                    for (String str : imc.visibleOnActionSet) {
                                        org.eclipse.core.internal.expressions.EqualsExpression ee = new org.eclipse.core.internal.expressions.EqualsExpression(
                                                str);
                                        oe.add(ee);
                                    }
                                    we.add(oe);
                                    exp = we;
                                }

                                for (IContributionItem item : items) {
                                    additions.addContributionItem(item, exp);
                                }
                            }
                        };
                        menuService.addContributionFactory(viewMenuAddition);
                    }
                }
            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing menu file: "
                                + lf.getFile().getAbsolutePath(), e);

            }
        }
    }
}
