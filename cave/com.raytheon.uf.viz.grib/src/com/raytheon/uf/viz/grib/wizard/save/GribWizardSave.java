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
package com.raytheon.uf.viz.grib.wizard.save;

import java.util.Arrays;

import com.raytheon.uf.common.grib.GridModel;
import com.raytheon.uf.common.grib.GridModelSet;
import com.raytheon.uf.common.grib.tables.GribTableLookup;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.viz.grib.wizard.GribWizardData;
import com.raytheon.viz.volumebrowser.xml.VbSourceList;

/**
 * 
 * Base class for saving the results of a grib wizard. Sub classes can implement
 * a single method and they will be able to save all the objects in the
 * {@link GribWizardData}.
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
public abstract class GribWizardSave {

    protected abstract void save(String location, Object object)
            throws Exception;

    public void save(GribWizardData data) throws Exception {

        GridCoverage coverage = data.getCoverage();
        if (coverage != null) {
            save("grib/grids/" + coverage.getName() + ".xml", coverage);

        }

        GridModel model = data.getOrCreateModel();
        String centerName = GribTableLookup.getInstance()
                .getTableValue(-1, -1, "0", model.getCenter()).toString();
        if (centerName == null || centerName.isEmpty()) {
            centerName = "UNK";
        } else {
            /*
             * Alot of the centers have a designator such as WMC, NMC, RAFC that
             * aren't really useful in the file path.
             */
            centerName = centerName.replaceAll("\\(.*\\)", "").trim();
        }

        GridModelSet set = new GridModelSet();
        set.addModels(Arrays.asList(model));
        String name = model.getName();
        save("grib/models/" + centerName + '/' + name + ".xml", set);

        if (data.getVbSource() != null) {
            VbSourceList vbList = new VbSourceList();
            vbList.setEntries(Arrays.asList(data.getVbSource()));
            save("volumebrowser/VbSources/" + name + ".xml", vbList);
        }

        return;
    }
}
