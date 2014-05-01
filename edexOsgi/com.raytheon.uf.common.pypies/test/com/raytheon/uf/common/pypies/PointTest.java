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
package com.raytheon.uf.common.pypies;

import java.awt.Point;
import java.io.File;
import java.io.IOException;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.FileUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class PointTest implements ISerializableObject {

    @DynamicSerializeElement
    private Point[] points;

    public Point[] getPoints() {
        return points;
    }

    public void setPoints(Point[] points) {
        this.points = points;
    }

    /**
     * @param args
     * @throws IOException
     * @throws SerializationException
     */
    public static void main(String[] args) {
        PointTest test = new PointTest();
        test.setPoints(new Point[] { new Point(3, 7), new Point(151, 42) });
        try {
            byte[] serialized = SerializationUtil.transformToThrift(test);
            FileUtil.bytes2File(serialized, new File("/tmp/javaPoints"));
        } catch (Exception e) {
            e.printStackTrace();
        }

        File pyFile = new File("/tmp/pythonPoints");
        if (pyFile.exists()) {
            try {
                byte[] bytes = FileUtil.file2bytes(pyFile);
                PointTest t = (PointTest) SerializationUtil
                        .transformFromThrift(bytes);
                for (Point p : t.getPoints()) {
                    System.out.println(p);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

        }
    }

}
