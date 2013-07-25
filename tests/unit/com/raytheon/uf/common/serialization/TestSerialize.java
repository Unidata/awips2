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
package com.raytheon.uf.common.serialization;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Ignore;

import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Test the serialization capabilities
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 12, 2008				chammack	Initial creation
 * Jul 25, 2013 2208        njensen     Moved to tests project
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

// TODO fix?
@Ignore
public class TestSerialize {

    @DynamicSerialize
    public static class Test {
        @DynamicSerializeElement
        private String _String;

        @DynamicSerializeElement
        private int _int;

        @DynamicSerializeElement
        private Integer _Integer;

        @DynamicSerializeElement
        private float _float;

        @DynamicSerializeElement
        private Float _Float;

        @DynamicSerializeElement
        private byte _byte;

        @DynamicSerializeElement
        private Byte _Byte;

        @DynamicSerializeElement
        private long _long;

        @DynamicSerializeElement
        private Long _Long;

        @DynamicSerializeElement
        private double _double;

        @DynamicSerializeElement
        private Double _Double;

        @DynamicSerializeElement
        private short _short;

        @DynamicSerializeElement
        private Short _Short;

        /**
         * @return the _String
         */
        public String get_String() {
            return _String;
        }

        /**
         * @param string
         *            the _String to set
         */
        public void set_String(String string) {
            _String = string;
        }

        /**
         * @return the _int
         */
        public int get_int() {
            return _int;
        }

        /**
         * @param _int
         *            the _int to set
         */
        public void set_int(int _int) {
            this._int = _int;
        }

        /**
         * @return the _Integer
         */
        public Integer get_Integer() {
            return _Integer;
        }

        /**
         * @param integer
         *            the _Integer to set
         */
        public void set_Integer(Integer integer) {
            _Integer = integer;
        }

        /**
         * @return the _float
         */
        public float get_float() {
            return _float;
        }

        /**
         * @param _float
         *            the _float to set
         */
        public void set_float(float _float) {
            this._float = _float;
        }

        /**
         * @return the _Float
         */
        public Float get_Float() {
            return _Float;
        }

        /**
         * @param float1
         *            the _Float to set
         */
        public void set_Float(Float float1) {
            _Float = float1;
        }

        /**
         * @return the _byte
         */
        public byte get_byte() {
            return _byte;
        }

        /**
         * @param _byte
         *            the _byte to set
         */
        public void set_byte(byte _byte) {
            this._byte = _byte;
        }

        /**
         * @return the _Byte
         */
        public Byte get_Byte() {
            return _Byte;
        }

        /**
         * @param byte1
         *            the _Byte to set
         */
        public void set_Byte(Byte byte1) {
            _Byte = byte1;
        }

        /**
         * @return the _long
         */
        public long get_long() {
            return _long;
        }

        /**
         * @param _long
         *            the _long to set
         */
        public void set_long(long _long) {
            this._long = _long;
        }

        /**
         * @return the _Long
         */
        public Long get_Long() {
            return _Long;
        }

        /**
         * @param long1
         *            the _Long to set
         */
        public void set_Long(Long long1) {
            _Long = long1;
        }

        /**
         * @return the _double
         */
        public double get_double() {
            return _double;
        }

        /**
         * @param _double
         *            the _double to set
         */
        public void set_double(double _double) {
            this._double = _double;
        }

        /**
         * @return the _Double
         */
        public Double get_Double() {
            return _Double;
        }

        /**
         * @param double1
         *            the _Double to set
         */
        public void set_Double(Double double1) {
            _Double = double1;
        }

        /**
         * @return the _short
         */
        public short get_short() {
            return _short;
        }

        /**
         * @param _short
         *            the _short to set
         */
        public void set_short(short _short) {
            this._short = _short;
        }

        /**
         * @return the _Short
         */
        public Short get_Short() {
            return _Short;
        }

        /**
         * @param short1
         *            the _Short to set
         */
        public void set_Short(Short short1) {
            _Short = short1;
        }

    }

    @DynamicSerialize
    public static class Test2 {
        @DynamicSerializeElement
        private String x;

        @DynamicSerializeElement
        private int y;

        /**
         * @return the x
         */
        public String getX() {
            return x;
        }

        /**
         * @param x
         *            the x to set
         */
        public void setX(String x) {
            this.x = x;
        }

        /**
         * @return the y
         */
        public int getY() {
            return y;
        }

        /**
         * @param y
         *            the y to set
         */
        public void setY(int y) {
            this.y = y;
        }

    }

    @DynamicSerialize
    public static class TestContainer {
        @DynamicSerializeElement
        private float[] array;

        @DynamicSerializeElement
        private Set<Float> set;

        @DynamicSerializeElement
        private List<Double> list;

        @DynamicSerializeElement
        private List<Test2> list2;

        @DynamicSerializeElement
        private Map<String, Test2> map;

        /**
         * @return the list
         */
        public List<Double> getList() {
            return list;
        }

        /**
         * @param list
         *            the list to set
         */
        public void setList(List<Double> list) {
            this.list = list;
        }

        /**
         * @return the list2
         */
        public List<Test2> getList2() {
            return list2;
        }

        /**
         * @param list2
         *            the list2 to set
         */
        public void setList2(List<Test2> list2) {
            this.list2 = list2;
        }

        /**
         * @return the map
         */
        public Map<String, Test2> getMap() {
            return map;
        }

        /**
         * @param map
         *            the map to set
         */
        public void setMap(Map<String, Test2> map) {
            this.map = map;
        }

        /**
         * @return the array
         */
        public float[] getArray() {
            return array;
        }

        /**
         * @param foo
         *            the array to set
         */
        public void setArray(float[] array) {
            this.array = array;
        }

        /**
         * @return the set
         */
        public Set<Float> getSet() {
            return set;
        }

        /**
         * @param set
         *            the set to set
         */
        public void setSet(Set<Float> set) {
            this.set = set;
        }

    }

    @DynamicSerialize
    public static class TestParent {
        @DynamicSerializeElement
        private AdapterObject adapterObject;

        /**
         * @return the adapterObject
         */
        public AdapterObject getAdapterObject() {
            return adapterObject;
        }

        /**
         * @param parentString
         *            the adapterObject to set
         */
        public void setAdapterObject(AdapterObject adapterObject) {
            this.adapterObject = adapterObject;
        }

    }

    @DynamicSerialize
    public static class TestAdvancedCollection1 {
        @DynamicSerializeElement
        private LinkedHashMap<String, String> foo;

        /**
         * @return the foo
         */
        public LinkedHashMap<String, String> getFoo() {
            return foo;
        }

        /**
         * @param foo
         *            the foo to set
         */
        public void setFoo(LinkedHashMap<String, String> foo) {
            this.foo = foo;
        }

    }

    public static class AdapterObject {
        private String innerObject;

        /**
         * @return the innerObject
         */
        public String getInnerObject() {
            return innerObject;
        }

        /**
         * @param innerObject
         *            the innerObject to set
         */
        public void setInnerObject(String innerObject) {
            this.innerObject = innerObject;
        }

    }

    public static class TestAdapter implements
            ISerializationTypeAdapter<AdapterObject> {

        @Override
        public AdapterObject deserialize(IDeserializationContext deserializer)
                throws SerializationException {
            String s = deserializer.readString();
            AdapterObject ao = new AdapterObject();
            ao.setInnerObject(s);
            return ao;
        }

        @Override
        public void serialize(ISerializationContext serializer,
                AdapterObject object) throws SerializationException {
            serializer.writeString(object.getInnerObject());
        }

    }

    @DynamicSerialize
    public static class TestChild extends TestParent {
        @DynamicSerializeElement
        private String childString;

        /**
         * @return the childString
         */
        public String getChildString() {
            return childString;
        }

        /**
         * @param childString
         *            the childString to set
         */
        public void setChildString(String childString) {
            this.childString = childString;
        }

    }

    @org.junit.Test
    public void testBasicFunctionality() {
        Test inTest = new Test();
        inTest.set_byte((byte) 1);
        inTest.set_Byte(new Byte((byte) 2));
        inTest.set_double(2.1);
        inTest.set_Double(new Double(4.4));
        inTest.set_float(4.2f);
        inTest.set_Float(5.5f);
        inTest.set_int(8);
        inTest.set_Integer(new Integer(32));
        inTest.set_long(1000000L);
        inTest.set_Long(new Long(10000000000000L));
        inTest.set_short((short) 8);
        inTest.set_Short(new Short((short) 92));
        inTest.set_String("abcdefg");

        DynamicSerializationManager dmgr = DynamicSerializationManager
                .getManager(SerializationType.Thrift);

        DynamicSerializationManager.inspect(inTest.getClass());
        byte[] bdata = null;
        try {
            bdata = dmgr.serialize(inTest);
            System.out.println("Serialized data of size: " + bdata.length);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Test outTest = null;

        try {
            ByteArrayInputStream bais = new ByteArrayInputStream(bdata);
            outTest = (Test) dmgr.deserialize(bais);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        // Make sure somehow we didn't get the same exact objects
        Assert.assertNotSame(inTest, outTest);

        // Verify results
        Assert.assertEquals(inTest.get_double(), outTest.get_double());
        Assert.assertEquals(inTest.get_Double(), outTest.get_Double());

        Assert.assertEquals(inTest.get_byte(), outTest.get_byte());
        Assert.assertEquals(inTest.get_Byte(), outTest.get_Byte());

        Assert.assertEquals(inTest.get_float(), outTest.get_float());
        Assert.assertEquals(inTest.get_Float(), outTest.get_Float());

        Assert.assertEquals(inTest.get_int(), outTest.get_int());
        Assert.assertEquals(inTest.get_Integer(), outTest.get_Integer());

        Assert.assertEquals(inTest.get_long(), outTest.get_long());
        Assert.assertEquals(inTest.get_Long(), outTest.get_Long());

        Assert.assertEquals(inTest.get_short(), outTest.get_short());
        Assert.assertEquals(inTest.get_Short(), outTest.get_Short());

        Assert.assertEquals(inTest.get_String(), outTest.get_String());

    }

    @org.junit.Test
    public void testContainerFunctionality() {

        DynamicSerializationManager dmgr = DynamicSerializationManager
                .getManager(SerializationType.Thrift);

        TestContainer inContainer = new TestContainer();

        Test2 inTest2_1 = new Test2();
        inTest2_1.setX("hijklmnop");
        inTest2_1.setY(127);

        Test2 inTest2_2 = new Test2();
        inTest2_2.setX("0123456");
        inTest2_2.setY(256);

        float[] in = new float[614 * 428];
        for (int i = 0; i < in.length; i++) {
            in[i] = i;
        }

        inContainer.setArray(in);

        Set<Float> inFloatSet = new HashSet<Float>();
        inFloatSet.add(1.0f);
        inFloatSet.add(1.3f);
        inFloatSet.add(2.2f);
        inContainer.setSet(inFloatSet);

        List<Double> inDoubleList = new ArrayList<Double>();
        inDoubleList.add(1.2);
        inDoubleList.add(0.5);
        inContainer.setList(inDoubleList);

        List<Test2> inTest2List = new ArrayList<Test2>();
        inTest2List.add(inTest2_1);
        inTest2List.add(inTest2_2);
        inContainer.setList2(inTest2List);

        Map<String, Test2> inMap = new HashMap<String, Test2>();
        inMap.put("abc", inTest2_1);
        inContainer.setMap(inMap);

        DynamicSerializationManager.inspect(inContainer.getClass());
        byte[] bdata = null;
        try {
            bdata = dmgr.serialize(inContainer);
            System.out.println("Serialized data of size: " + bdata.length);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }
        TestContainer outContainer = null;

        try {
            ByteArrayInputStream bais = new ByteArrayInputStream(bdata);
            outContainer = (TestContainer) dmgr.deserialize(bais);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        // Make sure somehow we didn't get the same exact objects
        Assert.assertNotSame(inContainer, outContainer);
        Assert.assertNotSame(inContainer.getArray(), outContainer.getArray());

        // Check the data came across okay
        Assert.assertEquals(inContainer.getArray().length,
                outContainer.getArray().length);
        for (int i = 0; i < inContainer.getArray().length; i++) {
            Assert.assertEquals(inContainer.getArray()[i],
                    outContainer.getArray()[i]);
        }

        Assert.assertEquals(inContainer.getList().size(), outContainer
                .getList().size());
        for (int i = 0; i < inContainer.getList().size(); i++) {
            Assert.assertEquals(inContainer.getList().get(i), outContainer
                    .getList().get(i));
        }

        Assert.assertEquals(inContainer.getList2().size(), outContainer
                .getList2().size());
        for (int i = 0; i < inContainer.getList2().size(); i++) {

            Assert.assertEquals(inContainer.getList2().get(i).getX(),
                    outContainer.getList2().get(i).getX());
            Assert.assertEquals(inContainer.getList2().get(i).getY(),
                    outContainer.getList2().get(i).getY());
        }

        Assert.assertEquals(inContainer.getSet().size(), outContainer.getSet()
                .size());
        Iterator<Float> iterator1 = inContainer.getSet().iterator();
        Iterator<Float> iterator2 = outContainer.getSet().iterator();
        while (iterator1.hasNext()) {
            Assert.assertEquals(iterator1.next(), iterator2.next());
        }

    }

    @org.junit.Test
    public void testInheritance() {
        TestChild tc = new TestChild();
        AdapterObject ao = new AdapterObject();
        ao.setInnerObject("parent");
        tc.setAdapterObject(ao);
        tc.setChildString("child");

        DynamicSerializationManager dmgr = DynamicSerializationManager
                .getManager(SerializationType.Thrift);

        DynamicSerializationManager.inspect(tc.getClass());
        byte[] bdata = null;
        try {
            bdata = dmgr.serialize(tc);
            System.out.println("Serialized data of size: " + bdata.length);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }
        TestChild outTest = null;

        try {
            ByteArrayInputStream bais = new ByteArrayInputStream(bdata);
            outTest = (TestChild) dmgr.deserialize(bais);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Assert.assertEquals(tc.getAdapterObject().getInnerObject(), outTest
                .getAdapterObject().getInnerObject());
        Assert.assertEquals(tc.getChildString(), outTest.getChildString());

    }

    @org.junit.Test
    public void testAdvancedCollections() {
        TestAdvancedCollection1 coll = new TestAdvancedCollection1();

        LinkedHashMap<String, String> lhm = new LinkedHashMap<String, String>();
        lhm.put("1", "one");
        lhm.put("2", "two");

        coll.setFoo(lhm);
        DynamicSerializationManager dmgr = DynamicSerializationManager
                .getManager(SerializationType.Thrift);

        DynamicSerializationManager.inspect(coll.getClass());
        byte[] bdata = null;
        try {
            bdata = dmgr.serialize(coll);
            System.out.println("Serialized data of size: " + bdata.length);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }
        TestAdvancedCollection1 outTest = null;

        try {
            ByteArrayInputStream bais = new ByteArrayInputStream(bdata);
            outTest = (TestAdvancedCollection1) dmgr.deserialize(bais);
            System.out.println(outTest);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Assert.assertEquals(LinkedHashMap.class, outTest.getFoo().getClass());

    }

    @DynamicSerialize
    public static class NullTester {
        @DynamicSerializeElement
        private Object[] objArr;

        public Object[] getObjArr() {
            return objArr;
        }

        public void setObjArr(Object[] objArr) {
            this.objArr = objArr;
        }

    }

    @org.junit.Test
    public void testNullsInArrays() {
        DynamicSerializationManager dmgr = DynamicSerializationManager
                .getManager(SerializationType.Thrift);

        DynamicSerializationManager.inspect(NullTester.class);

        NullTester nt = new NullTester();
        Object[] obj = new Object[3];
        obj[0] = new String("test");
        obj[1] = null;
        obj[2] = new Integer(3);

        nt.setObjArr(obj);

        byte[] d = null;
        try {
            d = dmgr.serialize(nt);
        } catch (SerializationException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        try {
            Object result = dmgr.deserialize(new ByteArrayInputStream(d));

            Assert.assertTrue(result instanceof NullTester);
            Assert.assertTrue(((NullTester) result).getObjArr() != null);
            Assert.assertTrue(((NullTester) result).getObjArr() instanceof Object[]);
            Assert.assertTrue(((Object[]) ((NullTester) result).getObjArr()).length == obj.length);
            Assert.assertTrue(((Object[]) ((NullTester) result).getObjArr())[0]
                    .equals(obj[0]));
            Assert.assertNull(((Object[]) ((NullTester) result).getObjArr())[1]);
            Assert.assertTrue(((Object[]) ((NullTester) result).getObjArr())[2]
                    .equals(obj[2]));
        } catch (SerializationException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

    }
}
