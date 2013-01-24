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
package com.raytheon.uf.viz.ui.menus.xml;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.jface.action.IContributionItem;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.menus.MenuSerialization;
import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuItem;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.DiscoverMenuContributions;
import com.raytheon.uf.viz.ui.menus.widgets.SubmenuContributionItem;

/**
 * Utilized in the index file, provides an include capability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2009            chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class IncludeMenuItem extends CommonIncludeMenuItem implements
        IContribItemProvider, ISerializableObject {
    static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IncludeMenuItem.class);

    private SubmenuContributionItem submenuCont = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.ui.menus.xml.IContribItemProvider#getContributionItems
     * (com.raytheon.uf.viz.ui.menus.xml.VariableSubstitution[], java.util.Set)
     */
    @Override
    public IContributionItem[] getContributionItems(
            final CommonAbstractMenuContribution items,
            VariableSubstitution[] incomingSubs, Set<String> removalsIn)
            throws VizException {
        if (subMenuName != null) {
            submenuCont = new SubmenuContributionItem(incomingSubs,
                    subMenuName, null, removalsIn) {

                @Override
                protected synchronized IContributionItem[][] getContributionItems() {
                    if (this.contributionItems == null) {
                        try {
                            this.contributionItems = new IContributionItem[][] { getAllContributionItems(
                                    items, subs, removals) };
                        } catch (VizException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                    return this.contributionItems;
                }

                /*
                 * (non-Javadoc)
                 * 
                 * @see org.eclipse.jface.action.MenuManager#getId()
                 */
                @Override
                public String getId() {
                    return "IncludeSubMenuContributionId_"+subMenuName;
                }
            };
            return new IContributionItem[] { submenuCont };
        }
        return getAllContributionItems(items, incomingSubs, removalsIn);
    }

    public IContributionItem[] getAllContributionItems(
            CommonAbstractMenuContribution items,
            VariableSubstitution[] incomingSubs, Set<String> removalsIn)
            throws VizException {
        List<IContributionItem> contribList = new ArrayList<IContributionItem>();
        try {
            // Read the file
            JAXBContext ctx = MenuSerialization.getJaxbContext();

            Unmarshaller um = ctx.createUnmarshaller();
            um.setSchema(DiscoverMenuContributions.schema);

            File file = PathManagerFactory.getPathManager().getStaticFile(
                    fileName.getPath());
            if (file == null || !file.exists())
                throw new VizException("File does not exist: "
                        + fileName.getPath());

            final MenuTemplateFile mtf = (MenuTemplateFile) um.unmarshal(file);

            // Create aggregated list of subs
            VariableSubstitution[] combinedSub = VariableSubstitution.combine(
                    incomingSubs, substitutions);

            Set<String> removalsSet = new HashSet<String>();
            if (removals != null) {
                removalsSet.addAll(Arrays.asList(removals));
            }

            if (mtf.contributions != null) {
                for (CommonAbstractMenuContribution c : mtf.contributions) {
                    AbstractMenuContributionItem<?> amc = MenuXMLMap.xmlMapping
                            .get(c.getClass());
                    if (removalsSet.contains(c.id))
                        continue;

                    if (amc == null) {
                        System.out.println("There is no xml mapping for "
                                + c.getClass());
                    }
                    IContributionItem[] contribItems = amc
                            .getContributionItems(c, combinedSub, removalsSet);

                    if (contribItems != null && contribItems.length > 0) {
                        contribList.addAll(Arrays.asList(contribItems));
                    }
                }
            }

            IContributionItem[] ciArray = contribList
                    .toArray(new IContributionItem[contribList.size()]);
            return ciArray;

        } catch (JAXBException e) {
            throw new VizException("Unable to unmarshal sub-xml file: "
                    + fileName, e);
        }

    }
}
