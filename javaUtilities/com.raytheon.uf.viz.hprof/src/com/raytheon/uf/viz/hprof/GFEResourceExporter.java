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
package com.raytheon.uf.viz.hprof;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;

/**
 * 
 * Export information about GFEResources, really just the parmId at this point.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 08, 2014  2648     bsteffen    Initial doc
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GFEResourceExporter extends AbstractExporter {

    public GFEResourceExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected String getFileName() {
        return "gfeResources.txt";
    }

    @Override
    protected String getComment() {
        return "# This file contains the parmId of every GFE Resource";
    }

    @Override
    protected String getInfo() {
        return "Generating output for GFEResource...";
    }

    @Override
    protected void exportInternal() throws IOException {
        List<SmartInstance> resources = getInstances("com.raytheon.viz.gfe.rsc.GFEResource");
        if (resources.isEmpty()) {
            return;
        }
        for (SmartInstance resource : resources) {
            println(resource + "{");
            println("  parmID =  "
                    + resource.get("parm").get("gridInfo").get("parmID")
                            .getString("parmId"));
            println("}");
        }
    }

}
