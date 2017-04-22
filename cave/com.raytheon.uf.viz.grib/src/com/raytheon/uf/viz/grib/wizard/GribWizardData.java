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
package com.raytheon.uf.viz.grib.wizard;

import java.nio.file.Path;

import com.raytheon.uf.common.grib.GridModel;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.viz.volumebrowser.xml.VbSource;

/**
 * 
 * Class containing all the different data objects needed by the different pages
 * of the {@link GribWizard}. Having a single class hold this data makes it
 * easier for pages to build off of and validate values from other pages.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribWizardData {

    private GridModel model;

    private GridCoverage coverage;

    private VbSource vbSource;
    
    private LocalizationContext saveContext;

    private Path exportPath;

    public GridModel getOrCreateModel() {
        if (model == null) {
            model = new GridModel();
        }
        return model;
    }

    public GridModel getModel() {
        return model;
    }

    public void setModel(GridModel model) {
        this.model = model;
    }

    public GridCoverage getCoverage() {
        return coverage;
    }

    public void setCoverage(GridCoverage coverage) {
        this.coverage = coverage;
    }

    public VbSource getVbSource() {
        return vbSource;
    }

    public void setVbSource(VbSource vbSource) {
        this.vbSource = vbSource;
    }

    public LocalizationContext getSaveContext() {
        return saveContext;
    }

    public void setSaveContext(LocalizationContext saveContext) {
        this.saveContext = saveContext;
    }

    public Path getExportPath() {
        return exportPath;
    }

    public void setExportPath(Path exportPath) {
        this.exportPath = exportPath;
    }

}
