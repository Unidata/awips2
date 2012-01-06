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

package com.raytheon.edex.msg;

import java.util.ArrayList;
import java.util.Iterator;

import com.raytheon.uf.common.message.response.AbstractResponseMessage;

public class ResponseMessageInline extends AbstractResponseMessage {

    private String[] fileName;

    private ArrayList<byte[]> zippedData = new ArrayList<byte[]>();

    public ArrayList<byte[]> getZippedData() {
        ArrayList<byte[]> list = new ArrayList<byte[]>();
        for (byte[] array : zippedData) {
            list.add(array);
        }
        return list;

    }

    public void setZippedData(ArrayList<byte[]> fileData) {
        this.zippedData.clear();
        for (Iterator<byte[]> iter = fileData.iterator(); iter.hasNext();) {
            byte[] element = iter.next();
            this.zippedData.add(element);

        }
        this.zippedData = fileData;
    }

    public String[] getFileName() {
        return fileName;
    }

    public void setFileName(String[] fileName) {
        this.fileName = fileName;
    }

}
