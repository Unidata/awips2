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
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.jface.action.IContributionItem;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.menus.MenuSerialization;
import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.DiscoverMenuContributions;
import com.raytheon.uf.viz.ui.menus.widgets.SubmenuContributionItem;

/**
 * Providex ability to include menus from other localization files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Apr 27, 2009           chammack    Initial creation
 * Dec 11, 2013  2602     bsteffen    Update MenuXMLMap.
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class IncludeMenuContribution extends
        AbstractMenuContributionItem<CommonIncludeMenuContribution> {

    @Override
    public IContributionItem[] getContributionItems(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {
        CommonIncludeMenuContribution item = (CommonIncludeMenuContribution) items;
        if (item.subMenuName != null) {
            CommonIncludeMenuContribution newItem = new CommonIncludeMenuContribution();
            newItem.fileName = item.fileName;
            newItem.id = item.id;
            newItem.substitutions = item.substitutions;
            newItem.suppressErrors = item.suppressErrors;
            return new IContributionItem[] { new SubmenuContributionItem(subs,
                    item.id, item.subMenuName,
                    new CommonAbstractMenuContribution[] { newItem }, removals) };
        }
        return getContributionItemsInternal(items, subs, removals);
    }

    private IContributionItem[] getContributionItemsInternal(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {
        CommonIncludeMenuContribution item = (CommonIncludeMenuContribution) items;
        List<IContributionItem> contribList = new ArrayList<IContributionItem>();
        try {
            // Read the file
            JAXBContext ctx = MenuSerialization.getJaxbContext();

            Unmarshaller um = ctx.createUnmarshaller();
            um.setSchema(DiscoverMenuContributions.schema);

            File file = PathManagerFactory.getPathManager().getStaticFile(
                    item.fileName.getPath());
            if (file == null || !file.exists())
                throw new VizException("File does not exist: "
                        + item.fileName.getPath());

            final MenuTemplateFile mtf = (MenuTemplateFile) um.unmarshal(file);

            VariableSubstitution[] combinedSub = VariableSubstitution.combine(
                    subs, item.substitutions);

            if (mtf.contributions != null) {
                for (CommonAbstractMenuContribution c : mtf.contributions) {
                    IContribItemProvider amc = MenuXMLMap.getProvider(c
                            .getClass());
                    IContributionItem[] contribItems = amc
                            .getContributionItems(c, combinedSub, removals);
                    if (contribItems != null && contribItems.length > 0) {
                        contribList.addAll(Arrays.asList(contribItems));
                    }
                }
            }
        } catch (JAXBException e) {
            throw new VizException("Unable to unmarshal sub-xml file: "
                    + item.fileName, e);
        }

        return contribList.toArray(new IContributionItem[contribList.size()]);
    }
}
