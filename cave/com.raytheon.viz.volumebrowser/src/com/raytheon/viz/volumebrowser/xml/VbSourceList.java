package com.raytheon.viz.volumebrowser.xml;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleImgContribution;
import com.raytheon.uf.common.menus.xml.CommonToolBarContribution;
import com.raytheon.uf.common.menus.xml.CommonToolbarSubmenuContribution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
 * Aug 19, 2014  3506     mapeters    Populate toolbar contributions from directory of 
 *                                    source files instead of one file, merge sources from 
 *                                    different localization levels instead of overriding.
 * Jul 07, 2015  4641     mapeters    Fix/improve comparators for VbSource sorting.
 * Jul 10, 2015  4641     mapeters    Added check for sources with null key/category fields.
 * Oct 05, 2015  3861     bsteffen    Remove deprecated method call on LocalizationFile
 * Feb 12, 2016  5242     dgilling    Remove calls to deprecated Localization APIs.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class VbSourceList {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VbSourceList.class);

    private final static IPathManager pm = PathManagerFactory.getPathManager();

    /** The sources categories in order */
    private static final String[] CATEGORIES = new String[] { "Volume",
            "SfcGrid", "Local", "Point" };

    /**
     * Comparator for sorting sources (compares category, then subcategory, then
     * name).
     */
    private static Comparator<VbSource> comparator = new Comparator<VbSource>() {
        @Override
        public int compare(VbSource source1, VbSource source2) {
            String cat1 = source1.getCategory();
            String cat2 = source2.getCategory();
            if (!cat1.equals(cat2)) {
                /*
                 * Categories are in the format
                 * "DropDownMenu/SubMenu/SubMenu/..."
                 */
                String[] cat1Parts = cat1.split("/");
                String[] cat2Parts = cat2.split("/");
                int minParts = Math.min(cat1Parts.length, cat2Parts.length);
                for (int i = 0; i < minParts; i++) {
                    if (!cat1Parts[i].equals(cat2Parts[i])) {
                        /*
                         * Compare the drop down menu names differently to keep
                         * them in the order the NWS is used to.
                         */
                        Comparator<String> comparator = (i == 0) ? categoryComparator
                                : stringComparator;
                        return comparator.compare(cat1Parts[i], cat2Parts[i]);
                    }
                }

                /*
                 * At this point, categories must match up to the end of the
                 * smaller of the two (e.g. SfcGrid and SfcGrid/RTOFS/forecast).
                 * Return the comparison of sourceWithShorterCategory's name and
                 * otherSource's next submenu level (RTOFS in the example).
                 */
                if (cat1Parts.length > minParts) {
                    return stringComparator.compare(cat1Parts[minParts],
                            source2.getName());
                } else {
                    return stringComparator.compare(source1.getName(),
                            cat2Parts[minParts]);
                }
            }

            /*
             * Compare subcategories next. If one source has a subcategory and
             * another doesn't, return the source with a subcategory as being
             * larger (later in the list).
             */
            String subCat1 = source1.getSubCategory();
            String subCat2 = source2.getSubCategory();
            if (subCat1 != null && subCat2 != null) {
                if (!subCat1.equals(subCat2)) {
                    return stringComparator.compare(subCat1, subCat2);
                }
            } else if (subCat1 != null) {
                return 1;
            } else if (subCat2 != null) {
                return -1;
            }

            // Compare names if categories and subcategories match.
            return stringComparator.compare(source1.getName(),
                    source2.getName());
        }
    };

    /**
     * Comparator for comparing two strings, ignoring capitalization and
     * comparing numeric values (assumes there are no leading zeros in the
     * numeric values).
     */
    private static Comparator<String> stringComparator = new Comparator<String>() {
        @Override
        public int compare(String s1, String s2) {
            int n1 = s1.length();
            int n2 = s2.length();
            int min = Math.min(n1, n2);
            String number1 = "";
            String number2 = "";
            for (int i = 0; i < min; i++) {
                char c1 = s1.charAt(i);
                char c2 = s2.charAt(i);
                if (Character.isDigit(c1) && Character.isDigit(c2)) {
                    /*
                     * If aligned characters are both digits, store them as
                     * strings and proceed to next pair of aligned characters.
                     */
                    number1 += c1;
                    number2 += c2;
                    continue;
                } else if (!(Character.isDigit(c1) || Character.isDigit(c2))) {
                    /*
                     * If neither aligned character is a digit, return
                     * difference in stored numbers if they aren't equal,
                     * otherwise reset numbers if they aren't already empty.
                     */
                    if (!number1.equals(number2)) {
                        return Integer.valueOf(number1)
                                - Integer.valueOf(number2);
                    } else if (!number1.isEmpty()) {
                        number1 = "";
                        number2 = "";
                    }
                } else if (!number1.isEmpty()) {
                    /*
                     * Exactly one of the two characters must be a digit to
                     * reach here. If the numbers aren't empty, whichever string
                     * has the extra digit is larger as its number is larger.
                     */
                    if (Character.isDigit(c1)) {
                        return 1;
                    } else {
                        return -1;
                    }
                }
                c1 = Character.toUpperCase(c1);
                c2 = Character.toUpperCase(c2);
                if (c1 != c2) {
                    c1 = Character.toLowerCase(c1);
                    c2 = Character.toLowerCase(c2);
                    if (c1 != c2) {
                        // No overflow because of numeric promotion
                        return c1 - c2;
                    }
                }
            }
            /*
             * If two strings end with unequal numeric values after for loop,
             * check for additional digits beyond minimum length to determine
             * order.
             */
            if (!number1.equals(number2)) {
                if (n1 > n2 && Character.isDigit(s1.charAt(n2))) {
                    return 1;
                } else if (n2 > n1 && Character.isDigit(s2.charAt(n1))) {
                    return -1;
                } else {
                    return Integer.valueOf(number1) - Integer.valueOf(number2);
                }
            }
            return n1 - n2;
        }
    };

    /**
     * Comparator for comparing the drop down menu names of the sources.
     * Determines order based on {@link #CATEGORIES}.
     */
    private static Comparator<String> categoryComparator = new Comparator<String>() {
        @Override
        public int compare(String cat1, String cat2) {
            if (cat1.equals(cat2)) {
                return 0;
            }
            for (String category : CATEGORIES) {
                /*
                 * The categories aren't equal (checked for above), so whichever
                 * one appears first in the ordered categories list (CATEGORIES)
                 * should be returned as being smaller (making it also appear
                 * earlier in the sorted list of VbSources).
                 */
                if (cat1.equals(category)) {
                    return -1;
                } else if (cat2.equals(category)) {
                    return 1;
                }
            }

            /*
             * If neither category is in the ordered list of expected
             * categories, compare them alphabetically.
             */
            return stringComparator.compare(cat1, cat2);
        }
    };

    /**
     * @deprecated This file path string exists only to support legacy overrides
     *             and should eventually be removed.
     */
    @Deprecated
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

    /**
     * List of all the sources from one file at one localization level.
     */
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

    /**
     * List of all sources from all files at all localization levels.
     */
    private List<VbSource> allSources;

    /**
     * @return the list of all sources
     */
    public synchronized List<VbSource> getAllSources() {
        return allSources;
    }

    public static VbSourceList getInstance() {
        synchronized (VB_SOURCE_FILE) {
            if (instance == null) {
                instance = new VbSourceList();
                instance.populateAllSources();
                if (observer == null) {
                    observer = new VbSourceListener();
                    LocalizationFile vbDirectory = pm
                            .getStaticLocalizationFile("volumebrowser");
                    vbDirectory.addFileUpdatedObserver(observer);
                }
            }
            return instance;
        }
    }

    public synchronized void populateAllSources() {
        allSources = new ArrayList<VbSource>();
        List<String> fileNames = new ArrayList<String>();
        LocalizationFile vbSourceFile = pm
                .getStaticLocalizationFile(VB_SOURCE_FILE);
        if (vbSourceFile == null) {
            LocalizationFile[] files = pm.listStaticFiles(
                    "volumebrowser/VbSources", null, false, true);
            for (LocalizationFile file : files) {
                fileNames.add(file.getPath());
            }
        } else {
            fileNames.add(VB_SOURCE_FILE);
        }
        for (String fileName : fileNames) {
            Map<LocalizationLevel, LocalizationFile> localizationFilesMap = pm
                    .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                            fileName);
            LocalizationLevel[] levels = pm.getAvailableLevels();
            /*
             * Add sources from localization files to entries, in order of
             * greatest precedence to lowest
             */
            for (int i = levels.length - 1; i >= 0; i--) {
                LocalizationFile locFile = localizationFilesMap.get(levels[i]);
                if (locFile != null) {
                    List<VbSource> sources = null;
                    try (InputStream is = locFile.openInputStream()) {
                        sources = JAXB.unmarshal(is, VbSourceList.class)
                                .getEntries();
                    } catch (IOException | LocalizationException e) {
                        statusHandler
                                .handle(Priority.ERROR,
                                        locFile.getPath()
                                                + " was excluded from sources menu due to error reading file.");
                    }
                    if (sources != null) {
                        Iterator<VbSource> itr = sources.iterator();
                        while (itr.hasNext()) {
                            VbSource source = itr.next();
                            if (source.getCategory() == null
                                    || source.getKey() == null) {
                                statusHandler
                                        .handle(Priority.WARN,
                                                source
                                                        + " was excluded from sources menu due to null key and/or category field.");
                                itr.remove();
                            }
                        }
                        allSources.addAll(sources);
                    }
                }
            }
        }

        DatasetInfoLookup lookup = DatasetInfoLookup.getInstance();
        DatasetInfo info;
        // Set containing sources to not be added to lists
        Set<VbSource> removes = new HashSet<VbSource>();
        Iterator<VbSource> itr = allSources.iterator();
        // The current index in allSources
        int i = 0;
        while (itr.hasNext()) {
            VbSource source = itr.next();
            // Set display names for sources
            if (source.getName() == null) {
                info = lookup.getInfo(source.getKey());
                source.setName(info != null ? info.getTitle() : source.getKey());
            }
            if (source.getRemove()) {
                // Add sources with remove tags to removal set and remove them.
                removes.add(source);
                itr.remove();
            } else if (removes.contains(source)
                    || allSources.subList(0, i).contains(source)) {
                // Remove sources in removal set and repeats
                itr.remove();
            } else {
                // Increment index in allSources if source wasn't removed
                i++;
            }
        }
        Collections.sort(allSources, comparator);
        allSources = Collections.unmodifiableList(allSources);
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
         * contributions that fall within that category/menu.
         */
        Map<CommonAbstractMenuContribution, List<CommonAbstractMenuContribution>> subContributions = new LinkedHashMap<CommonAbstractMenuContribution, List<CommonAbstractMenuContribution>>();

        for (VbSource source : VbSourceList.getInstance().getAllSources()) {
            String key = source.getKey();
            /*
             * Skip sources that are not active for this view or are marked for
             * removal
             */
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
            mContrib.key = key;
            mContrib.menuText = source.getName();

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
         * end of the menu that contains them.
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
             * are just a visual separator not an actual element with children.
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
