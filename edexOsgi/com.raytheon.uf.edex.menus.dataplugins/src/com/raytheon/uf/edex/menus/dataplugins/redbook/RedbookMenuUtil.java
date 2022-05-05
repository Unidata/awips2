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
package com.raytheon.uf.edex.menus.dataplugins.redbook;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.menus.AbstractMenuUtil;
import com.raytheon.uf.edex.menus.dataplugins.redbook.xml.MenuEntry;
import com.raytheon.uf.edex.menus.dataplugins.redbook.xml.MenuEntryType;
import com.raytheon.uf.edex.menus.dataplugins.redbook.xml.RedbookMenusXML;

/**
 * Parent class for NCEP/Hydro Redbook menu contributions for NDM.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 07, 2014  2858     mpduff    Initial creation
 * Mar 14, 2014  2855     mpduff    Refactored common code.
 * Mar 19, 2014  2860     mpduff    Implemented Redbook UpperAir.
 * Apr 30, 2014  2860     mpduff    Fixed instances of empty substitution tags.
 * Jun 09, 2014  3266     njensen   Removed reference to dataURIs
 * Jun 26, 2015  4512     mapeters  Implement createMenusFromFile() here, takes
 *                                  in {@link RedbookMenusXML} object.
 * Apr 08, 2016  5435     bsteffen  Move to menus plugin.
 * 
 * </pre>
 * 
 * @author mpduff
 */
public abstract class RedbookMenuUtil extends AbstractMenuUtil {
    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookMenuUtil.class);

    protected static final String NCEP_HYDRO = "ncepHydro";

    protected static final String MENUS = "menus";

    /** Jaxb unmarshaller */
    protected Unmarshaller unmarshaller;

    /** Redbook menu xml object */
    protected RedbookMenusXML xml;

    /**
     * Constructor.
     */
    public RedbookMenuUtil() {
        createContext();
    }

    /**
     * Method called from the RedbookMenuSubscriber when new NDM files are
     * placed in /awips2/edex/data/ndm
     * 
     * @param xml
     *            The Redbook menu XML object generated from the file(s) dropped
     *            into /awips2/edex/data/ndm
     */
    public void createMenusFromFile(RedbookMenusXML xml) {
        this.xml = xml;
        createMenus();
    }

    /**
     * Create the JaxB context.
     */
    protected void createContext() {
        try {
            JAXBContext jax = JAXBContext.newInstance(RedbookMenusXML.class);
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
    protected RedbookMenusXML read(String filename) {
        File f = new File(filename);
        try {
            xml = (RedbookMenusXML) unmarshaller.unmarshal(f);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, "Error unmarshalling "
                    + filename, e);
        }

        return xml;
    }

    /**
     * Recursively called to process the nested MenuEntity items.
     * 
     * @param menuEntry
     *            The menu entry to process
     * 
     * @return The generated menu contribution item
     */
    protected CommonAbstractMenuContribution processEntry(MenuEntry menuEntry) {
        if (menuEntry.getType() == MenuEntryType.Title) {
            CommonTitleContribution title = new CommonTitleContribution();
            title.titleText = menuEntry.getText();
            title.id = menuEntry.getId();

            return title;
        } else if (menuEntry.getType() == MenuEntryType.Separator) {
            CommonSeparatorMenuContribution separator = new CommonSeparatorMenuContribution();
            separator.id = menuEntry.getId();

            return separator;
        } else if (menuEntry.getType() == MenuEntryType.Submenu) {
            List<CommonAbstractMenuContribution> menuContributionList = new ArrayList<>();

            CommonSubmenuContribution subMenuContribution = new CommonSubmenuContribution();
            subMenuContribution.menuText = menuEntry.getText();
            subMenuContribution.id = menuEntry.getId();

            for (MenuEntry menu : menuEntry.getMenuEntryList()) {
                CommonAbstractMenuContribution menuC = processEntry(menu);
                menuContributionList.add(menuC);
            }

            subMenuContribution.contributions = menuContributionList
                    .toArray(new CommonAbstractMenuContribution[menuContributionList
                            .size()]);

            return subMenuContribution;
        } else if (menuEntry.getType() == MenuEntryType.ProductButton) {
            CommonBundleMenuContribution commonBundleMenuContribution = new CommonBundleMenuContribution();
            commonBundleMenuContribution.bundleFile = menuEntry.getFile();
            commonBundleMenuContribution.id = menuEntry.getId();
            commonBundleMenuContribution.text = menuEntry.getText();

            if (!menuEntry.getMenuEntryList().isEmpty()) {
                List<VariableSubstitution> subList = new ArrayList<>();
                List<String> dataUriList = new ArrayList<>();

                for (MenuEntry me : menuEntry.getMenuEntryList()) {
                    if (MenuEntryType.Substitute == me.getType()) {
                        VariableSubstitution var = new VariableSubstitution();
                        var.key = me.getKey();
                        var.value = me.getValue();
                        statusHandler.info("Substitution: " + var.key + " <> "
                                + var.value);
                        subList.add(var);
                    } else if (MenuEntryType.DataUri == me.getType()) {
                        dataUriList.add(me.getDataUri());
                    }
                }

                commonBundleMenuContribution.substitutions = subList
                        .toArray(new VariableSubstitution[subList.size()]);
            }

            return commonBundleMenuContribution;
        } else {
            throw new IllegalArgumentException("Unknown menu type: "
                    + menuEntry.getType());
        }
    }

    /**
     * Create menus for the provided file.
     * 
     * @param file
     *            The file defining the menu
     */
    protected void createMenusForFile(String file) {
        MenuTemplateFile menuTemplate = new MenuTemplateFile();

        List<CommonAbstractMenuContribution> menuContributionList = new ArrayList<>();

        for (MenuEntry entry : xml.getMenuEntryList()) {
            CommonAbstractMenuContribution menuC = processEntry(entry);
            menuContributionList.add(menuC);
        }

        menuTemplate.contributions = menuContributionList
                .toArray(new CommonAbstractMenuContribution[menuContributionList
                        .size()]);

        toXml(menuTemplate, file);
    }
}
