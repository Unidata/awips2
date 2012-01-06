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

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.datastorage.StorageException;
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
 * Jun 9, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class TestStoreRequest implements ISerializableObject {

    public static enum TEST_ENUM {
        FIRST, SECOND, THIRD
    };

    @DynamicSerializeElement
    private String groupName;

    @DynamicSerializeElement
    private int size;

    @DynamicSerializeElement
    private String otherName;

    @DynamicSerializeElement
    private String nullTest;

    @DynamicSerializeElement
    private double value;

    @DynamicSerializeElement
    private boolean good;

    @DynamicSerializeElement
    private float floatValue;

    @DynamicSerializeElement
    private float[] floatArray;

    @DynamicSerializeElement
    private int[] intArray;

    @DynamicSerializeElement
    private Map<String, Float> testMap;

    @DynamicSerializeElement
    private Set<Integer> testSet;

    @DynamicSerializeElement
    private TEST_ENUM someEnum;

    public String getGroupName() {
        return groupName;
    }

    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public String getOtherName() {
        return otherName;
    }

    public void setOtherName(String otherName) {
        this.otherName = otherName;
    }

    public String getNullTest() {
        return nullTest;
    }

    public void setNullTest(String nullTest) {
        this.nullTest = nullTest;
    }

    public double getValue() {
        return value;
    }

    public void setValue(double value) {
        this.value = value;
    }

    public boolean isGood() {
        return good;
    }

    public void setGood(boolean good) {
        this.good = good;
    }

    public float getFloatValue() {
        return floatValue;
    }

    public void setFloatValue(float floatValue) {
        this.floatValue = floatValue;
    }

    public float[] getFloatArray() {
        return floatArray;
    }

    public void setFloatArray(float[] floatArray) {
        this.floatArray = floatArray;
    }

    public Map<String, Float> getTestMap() {
        return testMap;
    }

    public void setTestMap(Map<String, Float> testMap) {
        this.testMap = testMap;
    }

    public Set<Integer> getTestSet() {
        return testSet;
    }

    public void setTestSet(Set<Integer> testSet) {
        this.testSet = testSet;
    }

    public TEST_ENUM getSomeEnum() {
        return someEnum;
    }

    public void setSomeEnum(TEST_ENUM someEnum) {
        this.someEnum = someEnum;
    }

    public int[] getIntArray() {
        return intArray;
    }

    public void setIntArray(int[] intArray) {
        this.intArray = intArray;
    }

    public static void main(String[] args) throws Exception {
        TestStoreRequest test = new TestStoreRequest();
        test.setGood(true);
        test.setGroupName("/abc/def/ghi");
        test.setOtherName("fsdjakl;fjajfka;");
        test.setSize(54);
        test.setValue(4.6);
        test.setFloatValue(3.334f);
        float[] f = new float[302088];
        f[0] = 4.44f;
        test.setFloatArray(f);
        // test.setFloatArray(new float[] { 1.1f, 3004.23f, 45.34f, -0.1f });
        int[] i = new int[3020088];
        i[0] = 59;
        test.setIntArray(i);
        // test.setIntArray(new int[] { 0, 1, 2, 3, 5 });
        Map<String, Float> map = new HashMap<String, Float>();
        map.put("dog", 28.7f);
        map.put("cat", 14.4f);
        test.setTestMap(map);
        Set<Integer> set = new HashSet<Integer>();
        set.add(77);
        set.add(1);
        test.setTestSet(set);
        test.setSomeEnum(TEST_ENUM.SECOND);
        byte[] bytes = null;
        try {
            long t0 = System.currentTimeMillis();
            bytes = SerializationUtil.transformToThrift(test);
            long t1 = System.currentTimeMillis();
            System.out.println("Java serializing took: "
                    + ((t1 - t0) / 1000.0f));
        } catch (SerializationException e) {
            throw new StorageException("Can't serialize request", null, e);
        }
        File file = new File("/tmp/javaOut");
        FileUtil.bytes2File(bytes, file);
    }

}
