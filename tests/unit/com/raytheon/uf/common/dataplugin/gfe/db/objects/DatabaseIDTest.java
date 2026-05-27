package com.raytheon.uf.common.dataplugin.gfe.db.objects;

import org.junit.Assert;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;

public class DatabaseIDTest {

    @Test
    public void testEqualsSameIDs() {
        DatabaseID testID1 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "20210819_1200");
        DatabaseID testID2 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "20210819_1200");
        Assert.assertTrue(testID1.equals(testID2));
        Assert.assertTrue(testID2.equals(testID1));
    }

    @Test
    public void testEqualsDifferentIDs() {
        DatabaseID testID1 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "20210819_1200");
        DatabaseID testID2 = new DatabaseID("BYZ", DataType.GRID, "test1",
                "gridtest", "20210819_1201");
        Assert.assertFalse(testID1.equals(testID2));
        Assert.assertFalse(testID2.equals(testID1));
    }

    @Test
    public void testCompareToSameIDs() {
        DatabaseID testID1 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "20210819_1200");
        DatabaseID testID2 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "20210819_1200");
        Assert.assertEquals(0, testID1.compareTo(testID2));
        Assert.assertEquals(0, testID2.compareTo(testID1));
    }

    @Test
    public void testCompareToDifferentIDs() {
        DatabaseID testID1 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "20210819_1200");
        DatabaseID testID2 = new DatabaseID("OAX", DataType.GRID, "test1",
                "gridtest", "20210819_1201");
        Assert.assertEquals(-1, testID1.compareTo(testID2));
        Assert.assertEquals(1, testID2.compareTo(testID1));
    }

    @Test
    public void testCompareToNullModelTime() {
        DatabaseID testID1 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "");
        DatabaseID testID2 = new DatabaseID("OAX", DataType.GRID, "test",
                "testgrid", "20210819_1200");
        Assert.assertEquals(1, testID1.compareTo(testID2));
        Assert.assertEquals(-1, testID2.compareTo(testID1));
    }
}
