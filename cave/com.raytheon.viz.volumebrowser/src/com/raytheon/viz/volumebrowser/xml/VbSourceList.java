package com.raytheon.viz.volumebrowser.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleImgContribution;
import com.raytheon.uf.common.menus.xml.CommonToolBarContribution;
import com.raytheon.uf.common.menus.xml.CommonToolbarSubmenuContribution;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * 
 * List of sources for populating the volume browser tool bar menus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Jan 06, 2011           bsteffen    Initial creation
 * Dec 11, 2013  2602     bsteffen    Remove ISerializableObject.
 * Mar 18, 2014  2874     bsteffen    Allow subMenus and move contribution
 *                                    creation from DataListsProdTableComp
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class VbSourceList {

    private final static String VB_SOURCE_FILE = "volumebrowser/VbSources.xml";

    private final static char SUB_MENU_SPLIT = '/';

    private static class VbSourceListener implements ILocalizationFileObserver {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
         * (com.raytheon.uf.common.localization.FileUpdatedMessage)
         */
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            synchronized (VB_SOURCE_FILE) {
                instance = null;
            }
        }

    }

    private static ILocalizationFileObserver observer = null;

    private static VbSourceList instance;

    @XmlElement(name = "vbSource")
    private List<VbSource> entries;

    /**
     * @return the entries
     */
    public List<VbSource> getEntries() {
        return entries;
    }

    /**
     * @param entries
     *            the entries to set
     */
    public void setEntries(List<VbSource> entries) {
        this.entries = entries;
    }

    public static VbSourceList getInstance() {
        synchronized (VB_SOURCE_FILE) {
            if (instance == null) {
                LocalizationFile file = PathManagerFactory.getPathManager()
                        .getStaticLocalizationFile(VB_SOURCE_FILE);
                if (observer == null) {
                    observer = new VbSourceListener();
                    file.addFileUpdatedObserver(observer);
                }

                instance = JAXB.unmarshal(file.getFile(), VbSourceList.class);

            }
            return instance;
        }
    }

    /**
     * Use the VbSources information to build {@link CommonToolBarContribution}
     * s.
     * 
     * @param selectedView
     *            the active viewMenu for the sources tool bar.
     * @return
     */
    public static List<CommonToolBarContribution> getToolBarContributions(
            ViewMenu selectedView) {
        /*
         * When processing the VbSource objects we need to look up the
         * contributions based off String values in the XML.
         */
        Map<String, CommonAbstractMenuContribution> contributions = new HashMap<String, CommonAbstractMenuContribution>();
        /*
         * For every category, subcategory or subMenu keep a list of all menu
         * contributions that fall within that cataegory/menu.
         */
        Map<CommonAbstractMenuContribution, List<CommonAbstractMenuContribution>> subContributions = new LinkedHashMap<CommonAbstractMenuContribution, List<CommonAbstractMenuContribution>>();

        for (VbSource source : VbSourceList.getInstance().getEntries()) {
            /* Skip sources that are not active for this view */
            if (source.getViews() != null
                    && !source.getViews().contains(selectedView)) {
                continue;
            }
            String cat = source.getCategory();
            String subCat = source.getSubCategory();

            /*
             * Start with the menu item for the source and then find where it
             * goes.
             */
            CommonMenuContribution mContrib = new CommonMenuContribution();
            mContrib.key = source.getKey();
            if (source.getName() != null) {
                mContrib.menuText = source.getName();
            } else {
                // Attempt a lookup in the grib model table
                DatasetInfo info = DatasetInfoLookup.getInstance().getInfo(
                        source.getKey());
                if (info != null) {
                    mContrib.menuText = info.getTitle();
                } else {
                    mContrib.menuText = source.getKey();
                }
            }

            CommonAbstractMenuContribution contrib = mContrib;

            if (subCat != null) {
                /*
                 * If there is a subCategory then that should appear within the
                 * category and any submenues within that category.
                 */
                String subCatkey = cat + ':' + subCat;
                if (contributions.containsKey(subCatkey)) {
                    CommonTitleImgContribution tContrib = (CommonTitleImgContribution) contributions
                            .get(subCatkey);
                    List<CommonAbstractMenuContribution> subList = subContributions
                            .get(tContrib);
                    subList.add(contrib);
                    contrib = null;
                } else {
                    CommonTitleImgContribution tContrib = new CommonTitleImgContribution();
                    tContrib.titleText = subCat;
                    tContrib.displayDashes = true;
                    tContrib.displayImage = true;
                    List<CommonAbstractMenuContribution> subList = new ArrayList<CommonAbstractMenuContribution>();
                    subList.add(contrib);
                    subContributions.put(tContrib, subList);
                    contributions.put(subCatkey, tContrib);
                    contrib = tContrib;
                }

            }
            /*
             * contrib will be null if the subCategory was already created by a
             * different source in which case no further processing is needed.
             */
            if (contrib != null) {
                int sepInd = cat.lastIndexOf(SUB_MENU_SPLIT);
                while (sepInd > -1) {
                    String parent = cat.substring(0, sepInd);
                    String child = cat.substring(sepInd + 1);
                    if (contributions.containsKey(cat)) {
                        CommonAbstractMenuContribution sContrib = contributions
                                .get(cat);
                        List<CommonAbstractMenuContribution> subList = subContributions
                                .get(sContrib);
                        subList.add(contrib);
                        contrib = null;
                        /*
                         * If the subMenu already exists we don't need to
                         * process any further
                         */
                        break;
                    } else {
                        CommonToolbarSubmenuContribution sContrib = new CommonToolbarSubmenuContribution();
                        sContrib.menuText = child;
                        sContrib.id = selectedView + cat;
                        List<CommonAbstractMenuContribution> subList = new ArrayList<CommonAbstractMenuContribution>();
                        subList.add(contrib);
                        subContributions.put(sContrib, subList);
                        contributions.put(cat, sContrib);
                        contrib = sContrib;
                    }
                    cat = parent;
                    sepInd = cat.lastIndexOf(SUB_MENU_SPLIT);
                }
                if (contrib != null) {
                    if (contributions.containsKey(cat)) {
                        CommonAbstractMenuContribution pContrib = contributions
                                .get(cat);
                        List<CommonAbstractMenuContribution> subList = subContributions
                                .get(pContrib);
                        subList.add(contrib);
                    } else {
                        CommonToolBarContribution rContrib = new CommonToolBarContribution();
                        rContrib.toolItemText = cat;
                        rContrib.id = selectedView + cat;
                        List<CommonAbstractMenuContribution> subList = new ArrayList<CommonAbstractMenuContribution>();
                        subList.add(contrib);
                        subContributions.put(rContrib, subList);
                        contributions.put(cat, rContrib);
                        contrib = rContrib;
                    }
                }
            }
        }
        /*
         * Now that all sources are processed, set the contributions within the
         * toolbar and submenu contributions. Also add all subcategories to the
         * end of the menu that ocntains them.
         */
        contributions = null;
        List<CommonToolBarContribution> rootContributions = new ArrayList<CommonToolBarContribution>();
        for (Entry<CommonAbstractMenuContribution, List<CommonAbstractMenuContribution>> entry : subContributions
                .entrySet()) {
            CommonAbstractMenuContribution contrib = entry.getKey();
            /*
             * Pull out all sub categories(CommonTitleImgContribution), move
             * them to the end of the list and then add all items within the
             * subcategory to the contributions. This is because sub categories
             * are just a visual seperator not an actual element with children.
             */
            List<CommonAbstractMenuContribution> list = entry.getValue();
            List<CommonAbstractMenuContribution> titleItems = new ArrayList<CommonAbstractMenuContribution>();
            ListIterator<CommonAbstractMenuContribution> it = list
                    .listIterator();
            while (it.hasNext()) {
                CommonAbstractMenuContribution subContrib = it.next();
                if (subContrib instanceof CommonTitleImgContribution) {
                    it.remove();
                    titleItems.add(subContrib);
                }
            }
            for (CommonAbstractMenuContribution titleItem : titleItems) {
                list.add(titleItem);
                list.addAll(subContributions.get(titleItem));
            }
            if (contrib instanceof CommonToolBarContribution) {
                CommonToolBarContribution rContrib = (CommonToolBarContribution) contrib;
                rContrib.contributions = list
                        .toArray(new CommonAbstractMenuContribution[0]);
                rootContributions.add(rContrib);
            } else if (contrib instanceof CommonToolbarSubmenuContribution) {
                CommonToolbarSubmenuContribution sContrib = (CommonToolbarSubmenuContribution) contrib;
                sContrib.contributions = list
                        .toArray(new CommonAbstractMenuContribution[0]);
            }
        }
        return rootContributions;
    }
}
