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
package com.raytheon.edex.plugin.redbook.ingest;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.edex.plugin.redbook.ingest.xml.ButtonXML;
import com.raytheon.edex.plugin.redbook.ingest.xml.RedbookHazardMenusXML;
import com.raytheon.edex.plugin.redbook.ingest.xml.SubmenuXML;
import com.raytheon.edex.plugin.redbook.ingest.xml.SubstituteXML;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;

/**
 * Builds the NCEP/Hydro SPC menu contributions for NDM (Redbook hazards).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2014    2858     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RedbookHazardsMenuUtil extends AbstractMenuUtil {
    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookHazardsMenuUtil.class);

    /** Menu type constant */
    private static final String MENU_TYPE = "spc";

    /** SPC hazard menu file */
    private static final String HAZARD_MENU = "hazardMenus.xml";

    /** Redbook bundle file */
    private static final String BUNDLE_FILE = "bundles/Redbook.xml";

    /** Redbook SPC hazard menu xml object */
    private RedbookHazardMenusXML xml;

    /** Jaxb unmarshaller */
    private Unmarshaller unmarshaller;

    /**
     * Constructor.
     */
    public RedbookHazardsMenuUtil() {
        createContext();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createMenus(String filename) {
        xml = read(filename);

        createMenus();
    }

    /**
     * Create the JAXB context
     */
    private void createContext() {
        Class[] classes = new Class[] { SubmenuXML.class, SubstituteXML.class,
                ButtonXML.class, RedbookHazardMenusXML.class };

        try {
            JAXBContext jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Read the xml file.
     * 
     * @param filename
     *            fully qualified file name to read.
     * 
     * @return The xml object
     */
    private RedbookHazardMenusXML read(String filename) {
        RedbookHazardMenusXML xml = null;
        File f = new File(filename);
        try {
            xml = (RedbookHazardMenusXML) unmarshaller.unmarshal(f);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, "Error unmarshalling "
                    + filename, e);
        }

        return xml;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createMenus() {
        MenuTemplateFile menuTemplate = new MenuTemplateFile();

        List<CommonSubmenuContribution> submenuList = new ArrayList<CommonSubmenuContribution>();

        for (SubmenuXML submenu : xml.getSubMenus()) {
            CommonSubmenuContribution subMenuContribution = new CommonSubmenuContribution();
            List<CommonBundleMenuContribution> commonBundleMenuContributions = new ArrayList<CommonBundleMenuContribution>();
            subMenuContribution.menuText = submenu.getName();
            subMenuContribution.id = submenu.getId();
            List<ButtonXML> buttonList = submenu.getButtonList();
            for (ButtonXML button : buttonList) {
                CommonBundleMenuContribution commonBundleMenuContribution = new CommonBundleMenuContribution();
                commonBundleMenuContribution.bundleFile = BUNDLE_FILE;
                commonBundleMenuContribution.id = button.getId();
                commonBundleMenuContribution.text = button.getMenuText();

                List<VariableSubstitution> subList = new ArrayList<VariableSubstitution>();
                for (int i = 0; i < button.getSubstitutionList().size(); i++) {
                    SubstituteXML sub = button.getSubstitutionList().get(i);
                    VariableSubstitution var = new VariableSubstitution();
                    var.key = sub.getKey();
                    var.value = sub.getValue();
                    subList.add(var);
                }
                commonBundleMenuContribution.substitutions = subList
                        .toArray(new VariableSubstitution[subList.size()]);
                commonBundleMenuContributions.add(commonBundleMenuContribution);
            }

            subMenuContribution.contributions = commonBundleMenuContributions
                    .toArray(new CommonBundleMenuContribution[commonBundleMenuContributions
                            .size()]);
            submenuList.add(subMenuContribution);
        }

        menuTemplate.contributions = submenuList
                .toArray(new CommonSubmenuContribution[submenuList.size()]);

        toXml(menuTemplate, "menus" + File.separator + MENU_TYPE
                + File.separator + HAZARD_MENU);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean checkCreated() {
        return super.checkCreated(HAZARD_MENU, MENU_TYPE);
    }
}
