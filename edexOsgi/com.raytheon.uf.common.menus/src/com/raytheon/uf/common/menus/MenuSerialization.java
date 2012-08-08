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
package com.raytheon.uf.common.menus;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonCommandContribution;
import com.raytheon.uf.common.menus.xml.CommonDynamicMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonIncludeMenuItem;
import com.raytheon.uf.common.menus.xml.CommonMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonMenuContributionFile;
import com.raytheon.uf.common.menus.xml.CommonPlaceholderMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSatBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSubmenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleImgContribution;
import com.raytheon.uf.common.menus.xml.CommonToolBarContribution;
import com.raytheon.uf.common.menus.xml.CommonToolbarSubmenuContribution;
import com.raytheon.uf.common.menus.xml.MenuTemplateFile;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;

/**
 * 
 * The list of serializable classes associated with menu and menu items.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2011            bsteffen     Initial creation
 * Jul 31, 2012 #875       rferrel     Added CommonDynamicMenuContribution.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MenuSerialization {

    private static JAXBContext context;

    public static synchronized JAXBContext getJaxbContext()
            throws JAXBException {
        if (context == null) {
            context = JAXBContext.newInstance(MenuTemplateFile.class,
                    CommonAbstractMenuContribution.class,
                    CommonMenuContribution.class,
                    CommonToolbarSubmenuContribution.class,
                    CommonBundleMenuContribution.class,
                    CommonCommandContribution.class,
                    CommonIncludeMenuContribution.class,
                    CommonIncludeMenuItem.class,
                    CommonMenuContributionFile.class,
                    CommonPlaceholderMenuContribution.class,
                    CommonSatBundleMenuContribution.class,
                    CommonSeparatorMenuContribution.class,
                    CommonSubmenuContribution.class,
                    CommonTitleContribution.class,
                    CommonTitleImgContribution.class,
                    CommonToolBarContribution.class,
                    VariableSubstitution.class,
                    CommonDynamicMenuContribution.class);

        }
        return context;
    }

}
