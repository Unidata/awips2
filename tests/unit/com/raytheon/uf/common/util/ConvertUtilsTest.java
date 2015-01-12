// header placeholder 2a20 c8e1
package com.raytheon.uf.common.util;

import java.lang.reflect.Field;
import java.util.Date;

import org.junit.Assert;
import org.junit.Test;
import com.raytheon.uf.common.convert.ConvertUtil;
/**
 * Tests for convert utils
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2013            bclement     Initial creation
 * Sep 04, 2014 3365       ccody        Changes for removing Data_Delivery dependencies
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ConvertUtilsTest {

    public static class TestEnity {

        protected int number;

        protected NestedTestEnity nested;

        /**
         * @param number
         * @param nested
         */
        public TestEnity(int number, NestedTestEnity nested) {
            this.number = number;
            this.nested = nested;
        }

        /**
         * @return the number
         */
        public int getNumber() {
            return number;
        }

        /**
         * @param number
         *            the number to set
         */
        public void setNumber(int number) {
            this.number = number;
        }

        /**
         * @return the nested
         */
        public NestedTestEnity getNested() {
            return nested;
        }

        /**
         * @param nested
         *            the nested to set
         */
        public void setNested(NestedTestEnity nested) {
            this.nested = nested;
        }

    }

    public static class NestedTestEnity {

        protected Date time;

        /**
         * @param time
         */
        public NestedTestEnity(Date time) {
            this.time = time;
        }

        /**
         * @return the time
         */
        public Date getTime() {
            return time;
        }

        /**
         * @param time
         *            the time to set
         */
        public void setTime(Date time) {
            this.time = time;
        }

    }

    /**
     * @throws NoSuchFieldException
     * @throws SecurityException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * 
     */
    @Test
    public void getFieldsTest() throws SecurityException, NoSuchFieldException,
            IllegalArgumentException, IllegalAccessException {
        final Date now = new Date();

        final String[] path = { "nested", "time" };
        final TestEnity ent = new TestEnity(42, new NestedTestEnity(now));
        Field[] res = ConvertUtil.getFields(TestEnity.class, path);
        Assert.assertEquals(path.length, res.length);

        NestedTestEnity resNested = (NestedTestEnity) res[0].get(ent);
        Date resDate = (Date) res[1].get(resNested);
        Assert.assertEquals(now, resDate);
    }

}
