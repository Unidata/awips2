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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.SmartInstance;
import com.raytheon.hprof.data.heap.dump.InstanceDump;

/**
 * 
 * Base class for objects exporting information from an hprof file. Provides a
 * framework for creating and commenting a file for output.
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
public abstract class AbstractExporter {

    protected final HprofFile hprof;

    private final File outputDirectory;

    private Writer writer;

    public AbstractExporter(HprofFile hprof, File outputDirectory) {
        this.hprof = hprof;
        this.outputDirectory = outputDirectory;
    }

    public void export() {
        System.out.println(getInfo());
        try {
            exportInternal();
        } catch (Throwable e) {
            e.printStackTrace();
        }
        close();
    }

    protected abstract String getFileName();

    protected abstract void exportInternal() throws IOException;

    protected String getComment() {
        return null;
    }

    protected String getInfo() {
        return "Generating output for " + getClass().getSimpleName() + "...";
    }

    protected void print(String output) throws IOException {
        if (writer == null) {
            writer = new BufferedWriter(new FileWriter(new File(
                    outputDirectory, getFileName())));
            String comment = getComment();
            if (comment != null) {
                println(comment);
            }
        }
        writer.append(output);
    }

    protected void println(String output) throws IOException {
        print(output + "\n");
    }

    protected void close() {
        if (writer != null) {
            try {
                writer.close();
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
    }

    protected List<SmartInstance> getInstances(String className) {
        List<InstanceDump> instances = hprof.getInstances(className);
        List<SmartInstance> smartInstances = new ArrayList<SmartInstance>(
                instances.size());
        for (InstanceDump instance : instances) {
            smartInstances.add(new SmartInstance(hprof, instance));
        }
        return smartInstances;
    }

}
