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
 * Export information about the resources waiting to dispose.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 20, 2014  2648     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DisposingResourceExporter extends AbstractExporter {

    public DisposingResourceExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected String getFileName() {
        return "disposingResources.txt";
    }

    @Override
    protected String getComment() {
        StringBuilder comment = new StringBuilder();
        comment.append("# This file contains resources that are waiting for the ResourceCatalog to\n");
        comment.append("# dispose them. These resources have already been removed from displays and are\n");
        comment.append("# waiting for asyncronous access to the UI thread.");
        return comment.toString();
    }

    @Override
    protected String getInfo() {
        return "Generating output for disposing Resources...";
    }

    @Override
    protected void exportInternal() throws IOException {
        List<SmartInstance> runnables = getInstances("com.raytheon.uf.viz.core.rsc.ResourceCatalog$1");
        if (runnables.isEmpty()) {
            return;
        }
        for (SmartInstance runnable : runnables) {
            SmartInstance resource = runnable.get("val$resource");
            println(resource.toString());
        }
    }

}
