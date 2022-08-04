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
package com.raytheon.uf.viz.grib.wizard.page;

import org.eclipse.jface.wizard.WizardPage;

import com.raytheon.uf.viz.grib.wizard.GribWizardData;

/**
 * 
 * Base page for other pages to extend. Provides a field for holding the data
 * and requires implementations to be able to repopulate when the data changes,
 * which happens when data is imported.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 26, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public abstract class AbstractGribWizardPage extends WizardPage {

    protected final GribWizardData data;

    public AbstractGribWizardPage(String pageName, GribWizardData data) {
        super(pageName);
        setTitle(pageName);
        this.data = data;
    }

    public abstract void populateFromData();

}
