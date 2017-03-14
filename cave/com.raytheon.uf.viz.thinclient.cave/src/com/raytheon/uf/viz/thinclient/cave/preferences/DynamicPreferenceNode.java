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
package com.raytheon.uf.viz.thinclient.cave.preferences;

import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceNode;

/**
 * Preference node that is added at runtime
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DynamicPreferenceNode extends PreferenceNode {

    public static interface IPreferencePageFactory {
        public IPreferencePage createNewPage();
    }

    private IPreferencePageFactory factory;

    private String label;

    /**
     * @param id
     * @param label
     * @param image
     * @param className
     */
    public DynamicPreferenceNode(String id, String label,
            IPreferencePageFactory factory) {
        super(id);
        this.label = label;
        this.factory = factory;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferenceNode#createPage()
     */
    @Override
    public void createPage() {
        setPage(factory.createNewPage());
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferenceNode#getLabelText()
     */
    @Override
    public String getLabelText() {
        return label;
    }

}
