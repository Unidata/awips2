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
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.hprof.HprofFile;
import com.raytheon.hprof.Id;
import com.raytheon.hprof.SmartInstance;
import com.raytheon.hprof.data.HeapDumpRecord;
import com.raytheon.hprof.data.heap.BasicType;
import com.raytheon.hprof.data.heap.dump.ClassDump;

/**
 * 
 * Export information about D2DProcedures dialogs
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
public class D2DProcedureDialogExporter extends AbstractExporter {

    public D2DProcedureDialogExporter(HprofFile hprof, File outputDirectory) {
        super(hprof, outputDirectory);
    }

    @Override
    protected String getFileName() {
        return "d2dProcedureDialogs.txt";
    }

    @Override
    protected String getComment() {
        return "# This file contains information about open D2D Procedure Dialogs.";
    }

    @Override
    protected String getInfo() {
        return "Generating output for D2D Procedure Dialogs...";
    }

    @Override
    protected void exportInternal() throws IOException {
        Id classId = hprof
                .lookUpClass("com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg");
        if (classId == null) {
            System.out.println("None found.");
            return;
        }
        HeapDumpRecord heapDump = hprof.getHeapDump();
        ClassDump classDump = heapDump.getClassDump(classId);
        SmartInstance openDialogs = null;
        for (Entry<Id, BasicType> staticEntry : classDump.getStaticFields()
                .entrySet()) {
            String name = hprof.getString(staticEntry.getKey());
            if ("openDialogs".equals(name)) {
                openDialogs = new SmartInstance(hprof,
                        heapDump.getInstance(staticEntry.getValue()
                                .getObjectId()));
                break;
            }
        }
        HashMap<String, SmartInstance> openDialogsMap = openDialogs
                .toStringKeyedHashMap();
        if (!openDialogsMap.isEmpty()) {
            println("ProcedureDlg.openDialogs{");
            for (Entry<String, SmartInstance> entry : openDialogsMap.entrySet()) {
                println("  " + entry.getKey() + " = " + entry.getValue());

            }
            println("}\n");
        }

        List<SmartInstance> dialogs = getInstances("com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg");
        for (SmartInstance dialog : dialogs) {
            outputProcedureDialog(dialog);
        }
    }

    protected void outputProcedureDialog(SmartInstance dialog)
            throws IOException {

        println(dialog + "{");
        println("  fileName =  " + dialog.getString("fileName") + "");
        println("}");

    }

}
